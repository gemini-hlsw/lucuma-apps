// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.tcs

import cats.data.NonEmptySet
import cats.effect.Sync
import cats.syntax.all.*
import coulomb.syntax.*
import coulomb.units.accepted.ArcSecond
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.M1Source
import lucuma.core.enums.Site
import lucuma.core.enums.StepGuideState
import lucuma.core.enums.TipTiltSource
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.GuideConfig
import lucuma.core.model.sequence.TelescopeConfig as CoreTelescopeConfig
import mouse.all.*
import observe.common.ObsQueriesGql.ObsQuery.Data.Observation.TargetEnvironment
import observe.model.enums.NodAndShuffleStage
import observe.model.enums.Resource
import observe.server.ConfigResult
import observe.server.InstrumentGuide
import observe.server.Length
import observe.server.ObserveFailure
import observe.server.altair.Altair
import observe.server.tcs.TcsController.*
import observe.server.tcs.TcsNorthController.TcsNorthAoConfig
import observe.server.tcs.TcsNorthController.TcsNorthConfig
import org.typelevel.log4cats.Logger

class TcsNorth[F[_]: {Sync, Logger}] private (
  tcsController: TcsNorthController[F],
  subsystems:    NonEmptySet[Subsystem],
  gaos:          Option[Altair[F]],
  guideDb:       GuideConfigDb[F]
)(config: TcsNorth.TcsSeqConfig)
    extends Tcs[F] {
  import Tcs.*

  val Log: Logger[F] = Logger[F]

  override val resource: Resource = Resource.TCS

  // Helper function to output the part of the TCS configuration that is actually applied.
  private def subsystemConfig(tcs: TcsNorthConfig, subsystem: Subsystem): String =
    ((tcs, subsystem) match {
      case (_, Subsystem.Mount)                   => pprint.apply(tcs.tc)
      case (_, Subsystem.AGUnit)                  => pprint.apply(List(tcs.agc.sfPos, tcs.agc.hrwfs))
      case (x: BasicTcsConfig[Site.GN.type], _)   => pprint.apply(x.guiding)
      case (x: TcsNorthAoConfig, Subsystem.M1)    => pprint.apply(x.gc.m1Guide)
      case (x: TcsNorthAoConfig, Subsystem.M2)    => pprint.apply(x.gc.m2Guide)
      case (x: TcsNorthAoConfig, Subsystem.OIWFS) => pprint.apply(x.gds.oiwfs.value)
      case (x: TcsNorthAoConfig, Subsystem.PWFS1) => pprint.apply(x.gds.pwfs1.value)
      case (x: TcsNorthAoConfig, Subsystem.PWFS2) => pprint.apply(x.gds.pwfs2.value)
      case (x: TcsNorthAoConfig, Subsystem.Gaos)  => pprint.apply(x.gds.aoguide)
    }).plainText

  override def configure: F[ConfigResult[F]] =
    buildTcsConfig.flatMap { cfg =>
      subsystems.traverse_(s =>
        Log.debug(s"Applying TCS/$s configuration/config: ${subsystemConfig(cfg, s)}")
      ) *>
        tcsController.applyConfig(subsystems, gaos, cfg).as(ConfigResult(this))
    }

  override def notifyObserveStart: F[Unit] = tcsController.notifyObserveStart

  override def notifyObserveEnd: F[Unit] = tcsController.notifyObserveEnd

  val defaultGuiderConf: GuiderConfig = GuiderConfig(ProbeTrackingConfig.Parked, GuiderSensorOff)
  def calcGuiderConfig(
    inUse:     Boolean,
    guideWith: Option[StepGuideState]
  ): GuiderConfig                     =
    guideWith
      .flatMap(v => inUse.option(GuiderConfig(v.toProbeTracking, v.toGuideSensorOption)))
      .getOrElse(defaultGuiderConf)

  // The step's own guide state is sent to Navigate.
  private def buildBasicTcsConfig: TcsNorthConfig =
    BasicTcsConfig(
      TelescopeConfig(config.offsetA, config.wavelA, config.instrumentDefocus),
      AGConfig(config.lightPath, HrwfsConfig.Auto.some),
      config.instrument,
      config.guiding
    )

  private def buildTcsAoConfig(gc: GuideConfig, ao: Altair[F]): F[TcsNorthConfig] =
    gc.gaosGuide
      .flatMap(_.swap.toOption.map { aog =>
        val aoGuiderConfig = ao
          .hasTarget(aog)
          .fold(
            calcGuiderConfig(calcGuiderInUse(gc.tcsGuide, TipTiltSource.GAOS, M1Source.GAOS),
                             config.guideWithAO
            ),
            GuiderConfig(ProbeTrackingConfig.Off,
                         config.guideWithAO.map(_.toGuideSensorOption).getOrElse(GuiderSensorOff)
            )
          )

        AoTcsConfig[Site.GN.type](
          gc.tcsGuide,
          TelescopeConfig(config.offsetA, config.wavelA, config.instrumentDefocus),
          AoGuidersConfig[AoGuide](
            P1Config(
              calcGuiderConfig(
                calcGuiderInUse(gc.tcsGuide, TipTiltSource.PWFS1, M1Source.PWFS1) | ao.usesP1(aog),
                config.guideWithP1
              )
            ),
            AoGuide(aoGuiderConfig),
            OIConfig(
              calcGuiderConfig(
                calcGuiderInUse(gc.tcsGuide, TipTiltSource.OIWFS, M1Source.OIWFS) | ao.usesOI(aog),
                config.guideWithOI
              )
            )
          ),
          AGConfig(config.lightPath, HrwfsConfig.Auto.some),
          aog,
          config.instrument
        ): TcsNorthConfig
      })
      .map(_.pure[F])
      .getOrElse(
        ObserveFailure
          .Execution("Attempting to run Altair sequence before Altair has being configured.")
          .raiseError[F, TcsNorthConfig]
      )

  def buildTcsConfig: F[TcsNorthConfig] =
    gaos
      .map(ao => guideDb.value.flatMap(c => buildTcsAoConfig(c.config, ao)))
      .getOrElse(buildBasicTcsConfig.pure[F])

  override def nod(
    stage:  NodAndShuffleStage,
    offset: InstrumentOffset,
    guided: Boolean
  ): F[ConfigResult[F]] =
    buildTcsConfig
      .flatMap { cfg =>
        Log.debug(s"Moving to nod ${stage.symbol}") *>
          tcsController.nod(subsystems, cfg)(stage, offset, guided)
      }
      .as(ConfigResult(this))
}

object TcsNorth {
  final case class TcsSeqConfig(
    guideWithP1:       Option[StepGuideState],
    guideWithP2:       Option[StepGuideState],
    guideWithOI:       Option[StepGuideState],
    guideWithAO:       Option[StepGuideState],
    offsetA:           Option[InstrumentOffset],
    wavelA:            Option[Wavelength],
    instrumentDefocus: Option[Length],
    lightPath:         LightPath,
    instrument:        InstrumentGuide,
    guiding:           StepGuideState
  )

  private[tcs] def config(
    instrument:          InstrumentGuide,
    targets:             TargetEnvironment,
    telescopeConfig:     CoreTelescopeConfig,
    lightPath:           LightPath,
    observingWavelength: Option[Wavelength],
    instrumentDefocus:   Option[Length]
  ): TcsSeqConfig = {
    val p: Offset.P = telescopeConfig.offset.p
    val q: Offset.Q = telescopeConfig.offset.q

    val guiding: StepGuideState = telescopeConfig.guiding

    val gwp1   = targets.guideEnvironment.guideTargets
      .exists(_.probe === GuideProbe.PWFS1)
      .option(telescopeConfig.guiding)
    val gwp2   = targets.guideEnvironment.guideTargets
      .exists(_.probe === GuideProbe.PWFS2)
      .option(telescopeConfig.guiding)
    val gwoi   = targets.guideEnvironment.guideTargets
      .exists(_.probe === GuideProbe.GmosOIWFS)
      .option(telescopeConfig.guiding)
    val gwao   = none.map(_ => guiding)
    val offset =
      InstrumentOffset(
        OffsetP(Angle.signedDecimalArcseconds.get(p.toAngle).toDouble.withUnit[ArcSecond]),
        OffsetQ(Angle.signedDecimalArcseconds.get(q.toAngle).toDouble.withUnit[ArcSecond])
      ).some

    TcsSeqConfig(
      gwp1,
      gwp2,
      gwoi,
      gwao,
      offset,
      observingWavelength,
      instrumentDefocus,
      lightPath,
      instrument,
      guiding
    )
  }

  def fromConfig[F[_]: {Sync, Logger}](
    controller:          TcsNorthController[F],
    subsystems:          NonEmptySet[Subsystem],
    gaos:                Option[Altair[F]],
    instrument:          InstrumentGuide,
    guideConfigDb:       GuideConfigDb[F]
  )(
    targets:             TargetEnvironment,
    telescopeConfig:     CoreTelescopeConfig,
    lightPath:           LightPath,
    observingWavelength: Option[Wavelength],
    defocus:             Option[Length]
  ): TcsNorth[F] = new TcsNorth(controller, subsystems, gaos, guideConfigDb)(
    config(instrument, targets, telescopeConfig, lightPath, observingWavelength, defocus)
  )

}

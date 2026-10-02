// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.tcs

import cats.data.NonEmptySet
import cats.effect.Sync
import cats.syntax.all.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.TelescopeConfig as CoreTelescopeConfig
import observe.model.enums.NodAndShuffleStage
import observe.model.enums.Resource
import observe.server.ConfigResult
import observe.server.Length
import observe.server.tcs.TcsController.*
import observe.server.tcs.TcsNorthController.TcsNorthConfig
import org.typelevel.log4cats.Logger

class TcsNorth[F[_]: {Sync, Logger}] private (
  tcsController: TcsNorthController[F],
  subsystems:    NonEmptySet[Subsystem]
)(config: TcsNorth.TcsSeqConfig)
    extends Tcs[F] {

  val Log: Logger[F] = Logger[F]

  override val resource: Resource = Resource.TCS

  // Helper function to output the part of the TCS configuration that is actually applied.
  private def subsystemConfig(tcs: TcsNorthConfig, subsystem: Subsystem): String =
    (subsystem match {
      case Subsystem.Mount  => pprint.apply(tcs.tc)
      case Subsystem.AGUnit => pprint.apply(tcs.lightPath)
      case _                => pprint.apply(tcs.guiding)
    }).plainText

  override def configure: F[ConfigResult[F]] = {
    val cfg = buildTcsConfig
    subsystems.traverse_(s =>
      Log.debug(s"Applying TCS/$s configuration/config: ${subsystemConfig(cfg, s)}")
    ) *>
      tcsController.applyConfig(subsystems, cfg).as(ConfigResult(this))
  }

  override def notifyObserveStart: F[Unit] = tcsController.notifyObserveStart

  override def notifyObserveEnd: F[Unit] = tcsController.notifyObserveEnd

  // The step's own guide state is sent to Navigate. The guide configuration set from TCC is not used.
  def buildTcsConfig: TcsNorthConfig =
    TcsConfig(
      TelescopeConfig(config.offsetA, config.wavelA, config.instrumentDefocus),
      config.lightPath,
      config.instrument,
      config.guiding
    )

  override def nod(
    stage:  NodAndShuffleStage,
    offset: InstrumentOffset,
    guided: Boolean
  ): F[ConfigResult[F]] =
    Log.debug(s"Moving to nod ${stage.symbol}") *>
      tcsController.nod(subsystems, buildTcsConfig)(stage, offset, guided).as(ConfigResult(this))
}

object TcsNorth {
  final case class TcsSeqConfig(
    offsetA:           Option[InstrumentOffset],
    wavelA:            Option[Wavelength],
    instrumentDefocus: Option[Length],
    lightPath:         LightPath,
    instrument:        Instrument,
    guiding:           StepGuideState
  )

  private[tcs] def config(
    instrument:          Instrument,
    telescopeConfig:     CoreTelescopeConfig,
    lightPath:           LightPath,
    observingWavelength: Option[Wavelength],
    instrumentDefocus:   Option[Length]
  ): TcsSeqConfig =
    TcsSeqConfig(
      telescopeConfig.offset.toInstrumentOffset.some,
      observingWavelength,
      instrumentDefocus,
      lightPath,
      instrument,
      telescopeConfig.guiding
    )

  def fromConfig[F[_]: {Sync, Logger}](
    controller:          TcsNorthController[F],
    subsystems:          NonEmptySet[Subsystem],
    instrument:          Instrument
  )(
    telescopeConfig:     CoreTelescopeConfig,
    lightPath:           LightPath,
    observingWavelength: Option[Wavelength],
    defocus:             Option[Length]
  ): TcsNorth[F] = new TcsNorth(controller, subsystems)(
    config(instrument, telescopeConfig, lightPath, observingWavelength, defocus)
  )

}

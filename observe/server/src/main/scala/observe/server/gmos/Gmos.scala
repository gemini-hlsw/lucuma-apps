// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.gmos

import cats.*
import cats.data.Kleisli
import cats.effect.Async
import cats.effect.Ref
import cats.effect.Temporal
import cats.syntax.all.*
import coulomb.Quantity
import coulomb.syntax.*
import coulomb.units.accepted.ArcSecond
import coulomb.units.accepted.Millimeter
import eu.timepit.refined.api.Refined.*
import lucuma.core.enums.GmosAdc
import lucuma.core.enums.GmosEOffsetting
import lucuma.core.enums.GmosGratingOrder
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.Instrument
import lucuma.core.enums.LightSinkName
import lucuma.core.enums.MosPreImaging
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.StepType as CoreStepType
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gmos.GmosNodAndShuffle
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import monocle.Getter
import observe.common.ObsQueriesGql.ObsQuery.Data.Observation.TargetEnvironment
import observe.model.GmosParameters.*
import observe.model.SystemOverrides
import observe.model.dhs.ImageFileId
import observe.model.enums.Guiding
import observe.model.enums.NodAndShuffleStage
import observe.model.enums.NodAndShuffleStage.*
import observe.model.enums.ObserveCommandResult
import observe.server.*
import observe.server.StepKind.ExclusiveDarkOrBias
import observe.server.gmos.GmosController.Config
import observe.server.gmos.GmosController.Config.*
import observe.server.gmos.GmosController.GmosSite
import observe.server.gmos.NSObserveCommand
import observe.server.keywords.DhsClient
import observe.server.keywords.DhsClientProvider
import observe.server.keywords.DhsInstrument
import observe.server.keywords.Header
import observe.server.keywords.KeywordsClient
import observe.server.tcs.FOCAL_PLANE_SCALE
import observe.server.tcs.FocalPlaneScale.*
import org.typelevel.log4cats.Logger

import java.time.temporal.ChronoUnit

abstract class Gmos[F[_]: {Temporal, Logger}, T <: GmosSite](
  val controller:    GmosController[F, T],
  dhsClientProvider: DhsClientProvider[F],
  nsCmdR:            Ref[F, Option[NSObserveCommand]],
  val config:        GmosController.GmosConfig[T]
) extends DhsInstrument[F]
    with InstrumentSystem[F] {

  import InstrumentSystem._

  override val contributorName: String = "gmosdc"

  override val keywordsClient: KeywordsClient[F] = this

  val nsCmdRef: Ref[F, Option[NSObserveCommand]] = nsCmdR

  val nsCount: F[Int] = controller.nsCount

  override def observeControl: InstrumentSystem.CompleteControl[F] =
    if (isNodAndShuffle)
      CompleteControl(
        StopObserveCmd(stopNS),
        AbortObserveCmd(abortNS),
        PauseObserveCmd(pauseNS),
        ContinuePausedCmd(controller.resumePaused),
        StopPausedCmd(controller.stopPaused),
        AbortPausedCmd(controller.abortPaused)
      )
    else
      CompleteControl(
        StopObserveCmd(_ => controller.stopObserve),
        AbortObserveCmd(controller.abortObserve),
        PauseObserveCmd(_ => controller.pauseObserve),
        ContinuePausedCmd(controller.resumePaused),
        StopPausedCmd(controller.stopPaused),
        AbortPausedCmd(controller.abortPaused)
      )

  private def stopNS(gracefully: Boolean): F[Unit] =
    if (gracefully)
      nsCmdRef.set(NSObserveCommand.StopGracefully.some)
    else
      nsCmdRef.set(NSObserveCommand.StopImmediately.some) *> controller.stopObserve

  private def abortNS: F[Unit] =
    nsCmdRef.set(NSObserveCommand.AbortImmediately.some) *> controller.abortObserve

  private def pauseNS(gracefully: Boolean): F[Unit] =
    if (gracefully)
      nsCmdRef.set(NSObserveCommand.PauseGracefully.some)
    else
      nsCmdRef.set(NSObserveCommand.PauseImmediately.some) *> controller.pauseObserve

  override def observe: Kleisli[F, ImageFileId, ObserveCommandResult] =
    Kleisli { fileId =>
      controller.observe(fileId, calcObserveTime)
    }

  override def instrumentActions: InstrumentActions[F] =
    new GmosInstrumentActions(this, controller)

  override def notifyObserveEnd: F[Unit] =
    controller.endObserve

  override def notifyObserveStart: F[Unit] = Applicative[F].unit

  override def configure: F[ConfigResult[F]] =
    controller
      .applyConfig(config)
      .as(ConfigResult(this))

  override def calcObserveTime: TimeSpan =
    config.dc.t /| config.ns.exposureDivider.value

  override def observeProgress(total: TimeSpan, elapsed: ElapsedTime): fs2.Stream[F, Progress] =
    controller.observeProgress(total, elapsed)

  def isNodAndShuffle: Boolean = config.ns match
    case NsConfig.NoNodAndShuffle => false
    case _                        => true

  override val dhsClient: DhsClient[F] = dhsClientProvider.dhsClient(dhsInstrumentName)
}

object Gmos {

  def rowsToShuffle(stage: NodAndShuffleStage, rows: NsRows): Int =
    if (stage === StageA) 0 else rows.value

  trait ParamGetters[
    T <: GmosSite
  ] {
    type S = GmosSite.StaticConfig[T]
    type D = GmosSite.DynamicConfig[T]
    val exposure: Getter[D, TimeSpan]
    val filter: Getter[D, Option[GmosController.GmosSite.Filter[T]]]
    val grating: Getter[D, Option[GmosController.GmosSite.Grating[T]]]
    val order: Getter[D, Option[GmosController.Config.GratingOrder]]
    val wavelength: Getter[D, Option[Wavelength]]
    val builtinFpu: Getter[D, Option[GmosController.GmosSite.FPU[T]]]
    val customFpu: Getter[D, Option[String]]
    val dtax: Getter[D, GmosController.Config.DTAX]
    val stageMode: Getter[S, GmosController.GmosSite.StageMode[T]]
    val nodAndShuffle: Getter[S, Option[GmosNodAndShuffle]]
    val roi: Getter[D, GmosRoi]
    val readout: Getter[D, GmosCcdMode]
    val isMosPreimaging: Getter[S, MosPreImaging]
    val centralWavelength: Getter[D, Option[Wavelength]]
  }

  def calcDisperser[T <: GmosSite](
    grt:   Option[GmosController.GmosSite.Grating[T]],
    order: Option[GmosGratingOrder],
    wl:    Option[Wavelength]
  ): GmosController.Config.GmosGrating[T] =
    grt
      .flatMap { disp =>
        // Workaround for missing order: Use order 1 as default
        val o = order.getOrElse(GmosGratingOrder.One)

        if (o === GmosGratingOrder.Zero)
          GmosController.Config.GmosGrating.Order0[T](disp).some
        else
          wl.map(w => GmosController.Config.GmosGrating.OrderN[T](disp, o, w))
      }
      .getOrElse(GmosController.Config.GmosGrating.Mirror())

  def fpuFromFPUnit[T <: GmosSite](
    n: Option[GmosController.GmosSite.FPU[T]],
    m: Option[String]
  ): Option[GmosFPU[T]] =
    n.map(GmosController.Config.BuiltInFPU[T].apply)
      .orElse(m.map(GmosController.Config.CustomMaskFPU[T].apply))

  def exposureTime(
    exp:      TimeSpan,
    nsConfig: NsConfig
  ): TimeSpan = exp /| nsConfig.exposureDivider.value

  def shutterStateObserveType(obsType: StepKind): ShutterState = obsType match {
    case StepKind.DarkOrBias(_) | StepKind.ExclusiveDarkOrBias(_) | StepKind.DarkOrBiasNS(_) =>
      ShutterState.CloseShutter
    case _                                                                                   => ShutterState.OpenShutter
  }

  def calcGainSetting(r: GmosCcdMode): Double = (r.ampReadMode, r.ampGain) match {
    case _ => 2.0
  }

  def buildConfig[
    F[_],
    T <: GmosSite
  ](
    stepType:   StepKind,
    staticCfg:  GmosSite.StaticConfig[T],
    dynamicCfg: GmosSite.DynamicConfig[T]
  )(using
    getters:    ParamGetters[T]
  ): GmosController.GmosConfig[T] = {

    def ccConfigFromSequenceConfig: Config.CCConfig[T] = {
      val isDarkOrBias: Boolean = stepType match {
        case StepKind.DarkOrBias(_)          => true
        case StepKind.DarkOrBiasNS(_)        => true
        case StepKind.ExclusiveDarkOrBias(_) => true
        case _                               => false
      }

      if (isDarkOrBias) Config.DarkOrBias[T]()
      else
        GmosController.Config.StandardCCConfig[T](
          getters.filter.get(dynamicCfg),
          calcDisperser[T](
            getters.grating.get(dynamicCfg),
            getters.order.get(dynamicCfg),
            getters.wavelength.get(dynamicCfg)
          ),
          fpuFromFPUnit[T](getters.builtinFpu.get(dynamicCfg), getters.customFpu.get(dynamicCfg)),
          getters.stageMode.get(staticCfg),
          getters.dtax.get(dynamicCfg),
          GmosAdc.Follow,
          getters.nodAndShuffle.get(staticCfg).map(_.eOffset).getOrElse(GmosEOffsetting.Off)
        )
    }

    def extractROIs: RegionsOfInterest =
      RegionsOfInterest.fromOCS(getters.roi.get(dynamicCfg), List.empty)

    def dcConfigFromSequenceConfig(
      nsConfig: NsConfig
    ): DCConfig = DCConfig(
      exposureTime(getters.exposure.get(dynamicCfg), nsConfig),
      shutterStateObserveType(stepType),
      CCDReadout(
        getters.readout.get(dynamicCfg).ampReadMode,
        getters.readout.get(dynamicCfg).ampGain,
        getters.readout.get(dynamicCfg).ampCount,
        calcGainSetting(getters.readout.get(dynamicCfg))
      ),
      CCDBinning(getters.readout.get(dynamicCfg).xBin, getters.readout.get(dynamicCfg).yBin),
      extractROIs
    )

    val nsConfig: NsConfig = getters.nodAndShuffle
      .get(staticCfg)
      .map { n =>
        NsConfig.NodAndShuffle(
          NsCycles(n.shuffleCycles.value),
          NsRows(n.shuffleOffset.value),
          Vector(NSPosition(NodAndShuffleStage.StageA, n.posA, Guiding.Guide),
                 NSPosition(NodAndShuffleStage.StageB, n.posB, Guiding.Guide)
          ),
          getters.exposure.get(dynamicCfg)
        )
      }
      .getOrElse(NsConfig.NoNodAndShuffle)

    GmosController.GmosConfig[T](
      ccConfigFromSequenceConfig,
      dcConfigFromSequenceConfig(nsConfig),
      nsConfig
    )

  }

  final case class GmosStatusGen(ns: NsConfig) extends StepStatusGen

  def isNodAndShuffle[S <: StaticConfig](
    staticConfig: S,
    l:            Getter[S, Option[GmosNodAndShuffle]]
  ): Boolean =
    l.get(staticConfig).isDefined

  def calcStepType[S <: StaticConfig](
    instrument: Instrument,
    stepCfg:    StepConfig,
    staticCfg:  S,
    obsClass:   ObserveClass,
    l:          Getter[S, Option[GmosNodAndShuffle]]
  ): Either[ObserveFailure, StepKind] = {
    val stdType = SeqTranslate.calcStepType(instrument, stepCfg, obsClass)
    if (Gmos.isNodAndShuffle(staticCfg, l)) {
      stdType.flatMap {
        case StepKind.DarkOrBias(_)      => StepKind.DarkOrBiasNS(instrument).asRight
        case StepKind.CelestialObject(_) => StepKind.NodAndShuffle(instrument).asRight
        case st                          => ObserveFailure.Unexpected(s"N&S is not supported for steps of type $st").asLeft
      }
    } else {
      stdType.map {
        case StepKind.DarkOrBias(inst) => StepKind.ExclusiveDarkOrBias(instrument)
        case x                         => x
      }
    }
  }

  def instrumentStepBuilder[F[_]: {Async, Logger}, T <: GmosSite](
    gmosInstrument: Instrument,
    sysBuilder:     (
      Systems.OverriddenSystems[F],
      Ref[F, Option[NSObserveCommand]],
      GmosController.GmosConfig[T]
    ) => SystemOverrides => Gmos[F, T]
  )(using
    getters:        ParamGetters[T]
  ): F[InstrumentStepBuilder[F, GmosSite.StaticConfig[T], GmosSite.DynamicConfig[T]]] = {
    type S = GmosSite.StaticConfig[T]
    type D = GmosSite.DynamicConfig[T]

    Ref
      .of[F, Option[NSObserveCommand]](none)
      .map(r =>
        new InstrumentStepBuilder[F, S, D] {
          override def build(
            systems:           Systems.OverriddenSystems[F],
            coreStepType:      CoreStepType,
            targetEnvironment: TargetEnvironment,
            staticConfig:      S,
            odbStep:           Step[D],
            observingTime:     Timestamp
          ): Either[ObserveFailure, InstrumentStep[F]] =
            Gmos
              .calcStepType[S](
                gmosInstrument,
                odbStep.stepConfig,
                staticConfig,
                odbStep.observeClass,
                getters.nodAndShuffle
              )
              .map { stType =>
                val config: GmosController.GmosConfig[T] = Gmos.buildConfig[F, T](
                  stType,
                  staticConfig,
                  odbStep.instrumentConfig
                )
                new InstrumentStep[F] {
                  override val oiOffsetGuideThreshold: Option[Quantity[Double, Millimeter]] =
                    (0.01.withUnit[ArcSecond] :\ FOCAL_PLANE_SCALE).some

                  override def stepType: StepKind = stType

                  override def sfName: LightSinkName = LightSinkName.Gmos

                  override def instrumentSystem(
                    sysOverrides: SystemOverrides
                  ): InstrumentSystem[F] = sysBuilder(systems, r, config)(sysOverrides)

                  override def instrumentHeader(kwClient: KeywordsClient[F]): Header[F] =
                    GmosHeader.header[F, T](
                      kwClient,
                      GmosObsKeywordsReader(staticConfig, odbStep.instrumentConfig),
                      systems.systems.gmosKeywordReader,
                      systems.systems.tcsKeywordReader
                    )

                  override def instrument: Instrument = gmosInstrument

                  override def observeTimeout: TimeSpan =
                    TimeSpan.unsafeFromDuration(110, ChronoUnit.SECONDS)

                  override def centralWavelength: Option[Wavelength] =
                    getters.centralWavelength.get(odbStep.instrumentConfig)
                }
              }
        }
      )
  }

}

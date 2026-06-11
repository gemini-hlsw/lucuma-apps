// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.igrins2

import cats.MonadThrow
import cats.data.Kleisli
import cats.effect.Temporal
import cats.syntax.all.*
import fs2.Stream
import lucuma.core.enums.Instrument
import lucuma.core.enums.LightSinkName
import lucuma.core.enums.StepType as OcsStepType
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.core.model.sequence.igrins2.Igrins2StaticConfig
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import observe.common.ObsQueriesGql.ObsQuery.Data.Observation.TargetEnvironment
import observe.model.ObserveStage
import observe.model.SystemOverrides
import observe.model.dhs.ImageFileId
import observe.model.enums.ObserveCommandResult
import observe.server.*
import observe.server.keywords.GdsClient
import observe.server.keywords.GdsInstrument
import observe.server.keywords.Header
import observe.server.keywords.KeywordsClient
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.syntax.*

final case class Igrins2[F[_]: {Logger as L, MonadThrow as F, Temporal}](
  controller: Igrins2Controller[F],
  config:     Igrins2Config
) extends GdsInstrument[F]
    with InstrumentSystem[F] { self =>

  override val gdsClient: GdsClient[F] = controller.gdsClient

  override val keywordsClient: KeywordsClient[F] = this

  override val resource: Instrument = Instrument.Igrins2

  override val contributorName: String = "igrins2"

  val readoutOverhead: TimeSpan = TimeSpan.fromSeconds(120).get

  val abort: F[Unit] = controller.abort

  def sequenceComplete: F[Unit] =
    info"IGRINS 2 Sequence complete" *>
      controller.sequenceComplete.handleErrorWith: e =>
        L.error(e)("Error in sequence complete")

  override def observeControl: InstrumentSystem.ObserveControl[F] =
    InstrumentSystem.UnpausableControl(InstrumentSystem.StopObserveCmd(_ => F.unit),
                                       InstrumentSystem.AbortObserveCmd(abort)
    )

  override def observe: Kleisli[F, ImageFileId, ObserveCommandResult] =
    Kleisli: fileId =>
      controller
        .observe(fileId, calcObserveTime)
        .as(ObserveCommandResult.Success: ObserveCommandResult)

  override def configure: F[ConfigResult[F]] =
    trace"Apply config ${config.configuration.toGiapi}" *>
      controller
        .applyConfig(config)
        .as(ConfigResult[F](self))

  override def notifyObserveEnd: F[Unit] =
    controller.endObserve

  override def notifyObserveStart: F[Unit] = F.unit

  override def calcObserveTime: TimeSpan =
    config.exposureTime +| config.readoutTime +| readoutOverhead

  override def observeProgress(
    total:   TimeSpan,
    elapsed: InstrumentSystem.ElapsedTime
  ): Stream[F, Progress] =
    val step = TimeSpan.fromSeconds(1.5).get

    Stream.force(
      for {
        progress <- controller.exposureProgress
        totalExp <-
          controller.requestedTime
            .map(_.flatMap(t => TimeSpan.fromSeconds(t.toDouble)).getOrElse(total))
      } yield ProgressUtil
        .realCountdownWithObsStage[F](
          totalExp,
          progress
            .map(TimeSpan.fromSeconds(_).map(_ +| step).getOrElse(TimeSpan.Zero)),
          (controller.dcIsPreparing,
           controller.dcIsAcquiring,
           controller.dcIsReadingOut,
           controller.dcIsWritingMEF
          ).mapN { (a, b, c, d) =>
            ObserveStage.fromBooleans(a, b, c, d)
          }
        )
    )

  override def instrumentActions: InstrumentActions[F] =
    InstrumentActions.defaultInstrumentActions[F]

}

object Igrins2:
//  object specifics:
//
////    override def instrument: Instrument =
////      Instrument.Igrins2
//
//    override def sfName: LightSinkName =
//      LightSinkName.Igrins2

  // TODO this could be part of the static config in core
  val CentralWavelength: Option[Wavelength] = Wavelength.fromIntNanometers(1975)

//  def build[F[_]: {MonadThrow, Temporal, Logger}](
//    controller:      Igrins2Controller[F],
//    dynamicConfig:   Igrins2DynamicConfig,
//    telescopeConfig: TelescopeConfig,
//    observeClass:    ObserveClass
//  ): Igrins2[F] =
//    Igrins2(controller, Igrins2Config(dynamicConfig, telescopeConfig, observeClass))

  def build[F[_]: {MonadThrow, Temporal, Logger}]
    : F[InstrumentStepBuilder[F, Igrins2StaticConfig, Igrins2DynamicConfig]] =
    new InstrumentStepBuilder[F, Igrins2StaticConfig, Igrins2DynamicConfig] {
      override def build(
        systems:           Systems.OverriddenSystems[F],
        stepType:          OcsStepType,
        targetEnvironment: TargetEnvironment,
        staticConf:        Igrins2StaticConfig,
        step:              Step[Igrins2DynamicConfig],
        observingTime:     Timestamp
      ): Either[ObserveFailure, InstrumentStep[F]] =
        SeqTranslate.calcStepType(Instrument.Igrins2, step.stepConfig, step.observeClass).map {
          stepKind =>
            val config: Igrins2Config =
              Igrins2Config(step.instrumentConfig, step.telescopeConfig, step.observeClass)
            new InstrumentStep[F] {
              override def stepType: StepKind = stepKind

              override def sfName: LightSinkName = LightSinkName.Igrins2

              override def instrumentSystem(sysOverrides: SystemOverrides): InstrumentSystem[F] =
                Igrins2(systems.igrins2(sysOverrides), config)

              override def instrumentHeader(client: KeywordsClient[F]): Header[F] =
                Igrins2Header.header(client, systems.systems.tcsKeywordReader)

              override def instrument: Instrument = Instrument.Igrins2

              override def centralWavelength: Option[Wavelength] =
                step.instrumentConfig.centralWavelength.some
            }

        }
    }.pure[F]

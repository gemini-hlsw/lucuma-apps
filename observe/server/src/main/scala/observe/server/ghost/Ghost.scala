// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.ghost

import cats.data.Kleisli
import cats.effect.Async
import cats.effect.Ref
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import fs2.Stream
import lucuma.core.enums.Instrument
import lucuma.core.enums.LightSinkName
import lucuma.core.enums.StepType as OcsStepType
import lucuma.core.math.*
import lucuma.core.model.*
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.ghost.GhostDetector
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.ghost.GhostStaticConfig
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.schemas.ObservationDB.Scalars.Timestamp
import observe.common.ObsQueriesGql.ObsQuery.Data.Observation.TargetEnvironment
import observe.model.CurrentConditions
import observe.model.SystemOverrides
import observe.model.dhs.ImageFileId
import observe.model.enums.ObserveCommandResult
import observe.server.*
import observe.server.InstrumentSystem.*
import observe.server.keywords.GdsClient
import observe.server.keywords.GdsInstrument
import observe.server.keywords.Header
import observe.server.keywords.KeywordsClient
import org.typelevel.log4cats.Logger

final case class Ghost[F[_]: {Logger, Async}](
  controller: GhostController[F],
  conditions: Ref[F, CurrentConditions],
  config:     GhostConfig
) extends GdsInstrument[F]
    with InstrumentSystem[F]
    with GhostLUT { self =>

  val readOutTimeExtra: TimeSpan = 420.secondTimeSpan

  override val gdsClient: GdsClient[F] = controller.gdsClient

  override val keywordsClient: KeywordsClient[F] = this

  override val resource: Instrument = Instrument.Ghost

  override val contributorName: String = "ghost"

  override def observeControl: InstrumentSystem.ObserveControl[F] =
    Uncontrollable

  override def observe: Kleisli[F, ImageFileId, ObserveCommandResult] =
    Kleisli { fileId =>
      controller
        .observe(fileId, calcObserveTime)
        .as(ObserveCommandResult.Success: ObserveCommandResult)
    }

  override def configure: F[ConfigResult[F]] =
    conditions.get.flatMap(controller.applyConfig(config, _)).as(ConfigResult[F](self))

  override def notifyObserveEnd: F[Unit] =
    controller.endObserve

  override def notifyObserveStart: F[Unit] = Async[F].unit

  override def calcObserveTime: TimeSpan =
    if (config.isBias) {
      val coAdds = config.coAdds.getOrElse(PosInt.unsafeFrom(1))
      totalObserveTime(GhostDetector.Blue(config.blueConfig.value.copy(exposureCount = coAdds)),
                       GhostDetector.Red(config.redConfig.value.copy(exposureCount = coAdds))
      ) +| readOutTimeExtra
    } else
      totalObserveTime(config.blueConfig, config.redConfig) +| readOutTimeExtra

  override def observeProgress(
    total:   TimeSpan,
    elapsed: InstrumentSystem.ElapsedTime
  ): Stream[F, Progress] =
    ProgressUtil.obsCountdown[F](total, elapsed.value)

  override def instrumentActions: InstrumentActions[F] =
    InstrumentActions.defaultInstrumentActions[F]

}

object Ghost {

  def buildConfig(
    static:            GhostStaticConfig,
    step:              Step[GhostDynamicConfig],
    targetEnvironment: TargetEnvironment,
    observingTime:     Timestamp
  ): Either[ObserveFailure, GhostConfig] =
    GhostConfig(
      static,
      step,
      targetEnvironment,
      observingTime
    )

  object specifics extends InstrumentStaticInfo {
    override val instrument: Instrument = Instrument.Ghost
  }

  def instrumentStepBuilder[F[_]: {Async, Logger}](
    conditionsRef: Ref[F, CurrentConditions]
  ): F[InstrumentStepBuilder[F, GhostStaticConfig, GhostDynamicConfig]] =
    new InstrumentStepBuilder[F, GhostStaticConfig, GhostDynamicConfig] {
      override def build(
        systems:           Systems.OverriddenSystems[F],
        stepType:          OcsStepType,
        targetEnvironment: TargetEnvironment,
        staticConf:        GhostStaticConfig,
        step:              Step[GhostDynamicConfig],
        observingTime:     Timestamp
      ): Either[ObserveFailure, InstrumentStep[F]] = for {
        stType <-
          SeqTranslate.calcStepType(specifics.instrument, step.stepConfig, step.observeClass)
        cfg    <- buildConfig(staticConf, step, targetEnvironment, observingTime)
      } yield new InstrumentStep[F] {
        override def stepType: StepKind = stType

        override def sfName: LightSinkName = LightSinkName.Ghost

        override def defocusB: Option[Length] = cfg.defocusOffset

        override def instrumentSystem(sysOverrides: SystemOverrides): InstrumentSystem[F] =
          Ghost(systems.ghost(sysOverrides), conditionsRef, cfg)

        override def instrumentHeader(client: KeywordsClient[F]): Header[F] = GhostHeader.header(
          client,
          systems.systems.tcsKeywordReader,
          GhostKeywordsReader(cfg, conditionsRef),
          step
        )

        override def instrument: Instrument = specifics.instrument

        override def centralWavelength: Option[Wavelength] =
          step.instrumentConfig.centralWavelength.some
      }
    }.pure[F]
}

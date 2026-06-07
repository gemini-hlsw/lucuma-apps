// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.gnirs

import cats.Applicative
import cats.data.Kleisli
import cats.effect.Async
import cats.syntax.all.*
import fs2.Stream
import lucuma.core.enums.Instrument
import lucuma.core.enums.LightSinkName
import lucuma.core.enums.StepType as OcsStepType
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsStaticConfig
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import observe.common.ObsQueriesGql.ObsQuery.Data.Observation.TargetEnvironment
import observe.model.SystemOverrides
import observe.model.dhs.ImageFileId
import observe.model.enums.ObserveCommandResult
import observe.server.*
import observe.server.gnirs.GnirsController.GnirsConfig
import observe.server.keywords.DhsClient
import observe.server.keywords.DhsClientProvider
import observe.server.keywords.DhsInstrument
import observe.server.keywords.Header
import observe.server.keywords.KeywordsClient
import org.typelevel.log4cats.Logger

final case class Gnirs[F[_]: {Async, Logger}](
  controller:        GnirsController[F],
  dhsClientProvider: DhsClientProvider[F],
  config:            GnirsConfig
) extends DhsInstrument[F]
    with InstrumentSystem[F] {

  override val resource: Instrument = Instrument.Gnirs

  override val contributorName: String = "gnirs"

  override val dhsInstrumentName: String = "GNIRS"

  override val dhsClient: DhsClient[F] = dhsClientProvider.dhsClient(dhsInstrumentName)

  override val keywordsClient: KeywordsClient[F] = this

  override def observeControl: InstrumentSystem.ObserveControl[F] =
    InstrumentSystem.UnpausableControl(
      InstrumentSystem.StopObserveCmd(_ => controller.stopObserve),
      InstrumentSystem.AbortObserveCmd(controller.abortObserve)
    )

  override def observe: Kleisli[F, ImageFileId, ObserveCommandResult] =
    Kleisli { fileId =>
      controller.observe(fileId, calcObserveTime)
    }

  override def configure: F[ConfigResult[F]] =
    controller.applyConfig(config).as(ConfigResult(this))

  override def notifyObserveEnd: F[Unit] = controller.endObserve

  override def notifyObserveStart: F[Unit] = Applicative[F].unit

  override def calcObserveTime: TimeSpan =
    GnirsController.calcObserveTime(config.dynamicConfig)

  override def observeProgress(
    total:   TimeSpan,
    elapsed: InstrumentSystem.ElapsedTime
  ): Stream[F, Progress] =
    controller.observeProgress(total)

  override def instrumentActions: InstrumentActions[F] =
    InstrumentActions.defaultInstrumentActions[F]
}

object Gnirs {

  def build[F[_]: {Async, Logger}]
    : F[InstrumentStepBuilder[F, GnirsStaticConfig, GnirsDynamicConfig]] =
    new InstrumentStepBuilder[F, GnirsStaticConfig, GnirsDynamicConfig] {
      override def build(
        systems:           Systems.OverriddenSystems[F],
        stepType:          OcsStepType,
        targetEnvironment: TargetEnvironment,
        staticConf:        GnirsStaticConfig,
        step:              Step[GnirsDynamicConfig],
        observingTime:     Timestamp
      ): Either[ObserveFailure, InstrumentStep[F]] =
        SeqTranslate.calcStepType(Instrument.Gnirs, step.stepConfig, step.observeClass).map {
          stepKind =>
            val config: GnirsConfig = GnirsConfig(staticConf, step.instrumentConfig, stepType)
            new InstrumentStep[F] {
              override def stepType: StepKind = stepKind

              override def sfName: LightSinkName = LightSinkName.Gnirs

              override def centralWavelength: Option[Wavelength] =
                step.instrumentConfig.centralWavelength.some

              override def instrument: Instrument = Instrument.Gnirs

              override def instrumentSystem(sysOverrides: SystemOverrides): InstrumentSystem[F] =
                Gnirs(systems.gnirs(sysOverrides), systems.dhs(sysOverrides), config)

              override def instrumentHeader(client: KeywordsClient[F]): Header[F] =
                GnirsHeader.header(
                  client,
                  systems.systems.gnirsKeywordReader,
                  systems.systems.tcsKeywordReader
                )
            }
        }
    }.pure[F]
}

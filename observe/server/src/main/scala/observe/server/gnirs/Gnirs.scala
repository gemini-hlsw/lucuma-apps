// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.gnirs

import cats.Applicative
import cats.syntax.all.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.LightSinkName
import lucuma.core.enums.StepType as OcsStepType
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsStaticConfig
import lucuma.core.util.Timestamp
import observe.common.ObsQueriesGql.ObsQuery.Data.Observation.TargetEnvironment
import observe.model.SystemOverrides
import observe.server.*
import observe.server.keywords.Header
import observe.server.keywords.KeywordsClient

object Gnirs:

  // GNIRS is currently only supported for loading and display in Observe; sequence
  // execution is not yet implemented. The instrument system and headers are therefore
  // only reachable at execution time, which does not happen for GNIRS yet.
  private val notImplemented: String =
    "GNIRS sequence execution is not yet implemented in Observe"

  def build[F[_]: Applicative]: F[InstrumentStepBuilder[F, GnirsStaticConfig, GnirsDynamicConfig]] =
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
            new InstrumentStep[F] {
              override def stepType: StepKind = stepKind

              override def sfName: LightSinkName = LightSinkName.Gnirs

              override def centralWavelength: Option[Wavelength] =
                step.instrumentConfig.centralWavelength.some

              override def instrument: Instrument = Instrument.Gnirs

              override def instrumentSystem(sysOverrides: SystemOverrides): InstrumentSystem[F] =
                sys.error(notImplemented)

              override def instrumentHeader(client: KeywordsClient[F]): Header[F] =
                sys.error(notImplemented)
            }
        }
    }.pure[F]

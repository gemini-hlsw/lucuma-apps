// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import lucuma.core.enums.Instrument
import lucuma.core.enums.StepType as CoreStepType
import lucuma.core.model.sequence.Step
import lucuma.core.util.Timestamp
import observe.common.ObsQueriesGql.ObsQuery.Data.Observation.TargetEnvironment

/** All the inputs an [[InstrumentStepBuilder]] needs to translate a single sequence step. */
final case class StepBuildContext[F[_], S, D](
  systems:           Systems.OverriddenSystems[F],
  coreStepType:      CoreStepType,
  targetEnvironment: TargetEnvironment,
  staticConf:        S,
  step:              Step[D],
  observingTime:     Option[Timestamp],
  customMasks:       CustomMasks
) {

  /**
   * The kind of step to execute. GMOS doesn't use this, it needs its static configuration to tell
   * nod and shuffle steps apart.
   */
  def stepKind(instrument: Instrument): Either[ObserveFailure, StepKind] =
    SeqTranslate.calcStepType(instrument, step.stepConfig, step.observeClass)
}

trait InstrumentStepBuilder[F[_], S, D] {
  def build(ctx: StepBuildContext[F, S, D]): Either[ObserveFailure, InstrumentStep[F]]
}

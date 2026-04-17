// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import lucuma.core.enums.StepType as CoreStepType
import lucuma.core.model.sequence.Step
import lucuma.core.util.Timestamp
import observe.common.ObsQueriesGql.ObsQuery.Data.Observation.TargetEnvironment

trait InstrumentStepBuilder[F[_], S, D] {
  def build(
    systems:           Systems.OverriddenSystems[F],
    stepType:          CoreStepType,
    targetEnvironment: TargetEnvironment,
    staticConf:        S,
    step:              Step[D],
    observingTime:     Timestamp
  ): Either[ObserveFailure, InstrumentStep[F]]
}

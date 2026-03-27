// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.engine

import cats.syntax.all.*
import lucuma.core.model.sequence.Step
import observe.model.StepState
import observe.server.engine.Action.ActionState

/**
 * A list of `Executions` grouped by observation.
 */
case class EngineStep[F[_]](
  id:         Step.Id,
  executions: List[ParallelActions[F]]
):
  /**
   * Calculate the `Step` `Status` based on the underlying `Action`s.
   */
  lazy val status: StepState =
    // Find an error in the Step
    executions
      .flatMap(_.toList)
      .find(Action.errored)
      .flatMap:
        x =>
          x.state.runState match
            case ActionState.Failed(Result.Error(msg)) => msg.some
            case _                                     => None
            // Return error or continue with the rest of the checks
      .map[StepState](StepState.Failed.apply)
      .getOrElse:
        // All actions in this Step were completed successfully, or the Step is empty.
        if (executions.flatMap(_.toList).exists(Action.aborted)) StepState.Aborted
        else if (executions.flatMap(_.toList).forall(Action.completed)) StepState.Completed
        else if (executions.flatMap(_.toList).forall(_.state.runState.isIdle))
          StepState.Pending
        // Not all actions are completed or pending.
        else StepState.Running

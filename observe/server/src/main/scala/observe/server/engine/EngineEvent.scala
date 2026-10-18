// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.engine

import lucuma.core.model.sequence.Step
import observe.model.ClientId
import observe.model.Observation
import observe.server.engine.Result.*

/**
 * Events generated internally by the Engine.
 */
sealed trait EngineEvent extends Product with Serializable

object EngineEvent {
  case class Completed[R <: RetVal](id: Observation.Id, stepId: Step.Id, i: Int, r: OK[R])
      extends EngineEvent
  case class StopCompleted[R <: RetVal](
    id:     Observation.Id,
    stepId: Step.Id,
    i:      Int,
    r:      OKStopped[R]
  ) extends EngineEvent
  case class Aborted[R <: RetVal](
    id:     Observation.Id,
    stepId: Step.Id,
    i:      Int,
    r:      OKAborted[R]
  ) extends EngineEvent
  case class PartialResult[R <: PartialVal](
    sid:    Observation.Id,
    stepId: Step.Id,
    i:      Int,
    r:      Partial[R]
  ) extends EngineEvent
  case class Paused(id: Observation.Id, stepId: Step.Id, i: Int, r: Result.Paused)
      extends EngineEvent
  case class Failed(id: Observation.Id, stepId: Step.Id, i: Int, e: Result.Error)
      extends EngineEvent
  case class LoadFailed(id: Observation.Id, i: Int, e: Result.Error) extends EngineEvent
  case class Busy(id: Observation.Id, clientId: ClientId)            extends EngineEvent
  case class BreakpointReached(id: Observation.Id)                   extends EngineEvent
  case class Executed(id: Observation.Id)                            extends EngineEvent
  case class Executing(id: Observation.Id)                           extends EngineEvent
  case class StepComplete(id: Observation.Id)                        extends EngineEvent
  case class SequenceHeld(id: Observation.Id)                        extends EngineEvent
  case class SequenceComplete(id: Observation.Id)                    extends EngineEvent
  case object Null                                                   extends EngineEvent

  // Single action commands
  case class SingleRunCompleted[R <: RetVal](actionCoords: ActionCoords, r: OK[R])
      extends EngineEvent
  case class SingleRunFailed(actionCoords: ActionCoords, e: Result.Error) extends EngineEvent

}

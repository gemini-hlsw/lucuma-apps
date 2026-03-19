// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.engine

import cats.syntax.all.*
import lucuma.core.enums.Breakpoint
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Step
import monocle.Lens
import observe.model.SequenceStatus
import observe.model.SequenceStatus.HasInternalStop
import observe.model.SequenceStatus.HasUserStop
import observe.server.engine.Action.ActionState
import observe.server.engine.Result.RetVal

/**
 * A single pending step for an observation.
 */
case class Sequence[F[_]] private (
  obsId:       Observation.Id,
  loadedStep:  Option[EngineStep[F]],
  breakpoints: Breakpoints
)

object Sequence:

  def empty[F[_]](obsId: Observation.Id): Sequence[F] =
    Sequence(obsId, none, Breakpoints.empty)

  def apply[F[_]](
    obsId:       Observation.Id,
    loadedStep:  EngineStep[F],
    breakpoints: Breakpoints
  ): Sequence[F] =
    new Sequence(obsId, loadedStep.some, breakpoints)

  def apply[F[_]](
    obsId:       Observation.Id,
    loadedStep:  Option[EngineStep[F]],
    breakpoints: Breakpoints
  ): Sequence[F] =
    new Sequence(obsId, loadedStep, breakpoints)

  /**
   * Simplified state for single-step execution. Replaces the old Zipper/Final sealed trait.
   * `currentStep` is `Some` when a step is actively executing, `None` when idle/done.
   */
  case class State[F[_]](
    obsId:       Observation.Id,
    status:      SequenceStatus,
    currentStep: Option[EngineStep.Zipper[F]], // None = idle/done, Some = executing
    breakpoints: Breakpoints,
    singleRuns:  Map[ActionCoordsInSeq, ActionState]
  ):

    /**
     * Advances execution within the current step's execution groups. Returns `None` if the step is
     * completed (no more execution groups).
     */
    val next: Option[State[F]] =
      currentStep match
        case None    => None
        case Some(z) =>
          val newBreakpoints: Breakpoints = breakpoints - z.id
          z.next match
            case None      => // Step completed - all execution groups done
              Some(copy(currentStep = None, breakpoints = newBreakpoints))
            case Some(stz) => // More execution groups to run
              Some(copy(currentStep = Some(stz), breakpoints = newBreakpoints))

    val pending: List[EngineStep[F]] = Nil // No pending steps with single-step execution

    def rollback: State[F] = copy(currentStep = currentStep.map(_.rollback))

    def setBreakpoints(breakpointsDelta: Set[(Step.Id, Breakpoint)]): State[F] =
      copy(
        breakpoints = breakpointsDelta.foldLeft(breakpoints) { case (accum, (stepId, breakpoint)) =>
          if breakpoint === Breakpoint.Enabled then accum + stepId else accum - stepId
        }
      )

    def getCurrentBreakpoint: Boolean =
      currentStep.exists(z => breakpoints.contains(z.id) && z.done.isEmpty)

    /**
     * Current Execution
     */
    val current: Execution[F] =
      currentStep.map(_.focus).getOrElse(Execution.empty)

    val engineCurrentStep: Option[EngineStep[F]] = currentStep.map(_.toStep)

    val done: List[EngineStep[F]] =
      currentStep.flatMap(_.uncurrentify).toList

    def mark(i: Int)(r: Result): State[F] =
      currentStep match
        case None    => this
        case Some(z) =>
          val updated = EngineStep.Zipper.current[F].modify(_.mark(i)(r))(z)
          copy(currentStep = Some(updated))

    def start(i: Int): State[F] =
      currentStep match
        case None    => this
        case Some(z) =>
          val updated = EngineStep.Zipper.current[F].modify(_.start(i))(z)
          copy(currentStep = Some(updated), singleRuns = Map.empty)

    def update(stepDef: Option[List[ParallelActions[F]]]): State[F] =
      (currentStep, stepDef) match
        case (Some(z), Some(t)) => copy(currentStep = Some(z.update(t)))
        case _                  => this

    val toSequence: Sequence[F] =
      Sequence(obsId, currentStep.map(_.toStep), breakpoints)

    // Functions to handle single run of Actions
    def startSingle(c: ActionCoordsInSeq): State[F] =
      currentStep match
        case Some(z) if z.done.nonEmpty => this // already past initial execution
        case _                          =>
          copy(singleRuns = singleRuns + (c -> ActionState.Started))

    def failSingle(c: ActionCoordsInSeq, err: Result.Error): State[F] =
      if getSingleState(c).started
      then copy(singleRuns = singleRuns + (c -> ActionState.Failed(err)))
      else this

    def completeSingle[V <: RetVal](c: ActionCoordsInSeq, r: V): State[F] =
      if getSingleState(c).started
      then copy(singleRuns = singleRuns + (c -> ActionState.Completed(r)))
      else this

    def getSingleState(c: ActionCoordsInSeq): ActionState =
      singleRuns.getOrElse(c, ActionState.Idle)

    def getSingleAction(c: ActionCoordsInSeq): Option[Action[F]] =
      for
        step <- toSequence.loadedStep.filter(_.id === c.stepId)
        exec <- step.executions.get(c.execIdx.value)
        act  <- exec.get(c.actIdx.value)
      yield act

    val getSingleActionStates: Map[ActionCoordsInSeq, ActionState] = singleRuns

    def clearSingles: State[F] = copy(singleRuns = Map.empty)

  object State:

    def status[F[_]]: Lens[State[F], SequenceStatus] =
      Lens[State[F], SequenceStatus](_.status)(s => st => st.copy(status = s))

    def isRunning[F[_]](st: State[F]): Boolean = st.status.isRunning

    def canUnload[F[_]](st: State[F]): Boolean = st.status.canUnload

    def userStopRequested[F[_]](st: State[F]): Boolean = st.status.isUserStopRequested

    def anyStopRequested[F[_]](st: State[F]): Boolean = st.status match
      case SequenceStatus.Running(u, i, _, _, _) => u || i
      case _                                     => false

    def isWaitingUserPrompt[F[_]](st: State[F]): Boolean = st.status.isWaitingUserPrompt

    def isStarting[F[_]](st: State[F]): Boolean = st.status.isStarting

    def userStopSet[F[_]](v: HasUserStop): State[F] => State[F] = status.modify {
      case r @ SequenceStatus.Running(_, _, _, _, _) => r.copy(userStop = v)
      case r                                         => r
    }

    def internalStopSet[F[_]](v: HasInternalStop): State[F] => State[F] = status.modify {
      case r @ SequenceStatus.Running(_, _, _, _, _) => r.copy(internalStop = v)
      case r                                         => r
    }

    /**
     * Initialize a `State` from a single step (EngineStep).
     */
    def init[F[_]](obsId: Observation.Id, step: EngineStep[F]): State[F] =
      EngineStep.Zipper.currentify(step) match
        case Some(z) =>
          State(obsId, SequenceStatus.Idle, Some(z), Breakpoints.empty, Map.empty)
        case None    =>
          State(obsId, SequenceStatus.Idle, None, Breakpoints.empty, Map.empty)

    /**
     * Initialize a `State` from a `Sequence`.
     */
    def init[F[_]](q: Sequence[F]): State[F] =
      val zipper = q.loadedStep.flatMap(EngineStep.Zipper.currentify)
      State(q.obsId, SequenceStatus.Idle, zipper, q.breakpoints, Map.empty)

    /**
     * Create an empty/idle state with no step loaded.
     */
    def idle[F[_]](obsId: Observation.Id): State[F] =
      State(obsId, SequenceStatus.Idle, None, Breakpoints.empty, Map.empty)

    /**
     * Rebuilds the state of a sequence with a new steps definition. The sequence must not be
     * running.
     */
    def reload[F[_]](
      step:     Option[EngineStep[F]],
      oldState: State[F]
    ): State[F] =
      if oldState.status.isRunning
      then oldState
      else oldState.copy(currentStep = step.flatMap(EngineStep.Zipper.currentify))

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.engine

import cats.Endo
import cats.effect.Concurrent
import cats.effect.MonadCancelThrow
import cats.effect.std.Queue
import cats.syntax.all.*
import fs2.Stream
import lucuma.core.model.sequence.Step
import monocle.Optional
import mouse.boolean.*
import observe.model.Observation
import observe.model.SequenceStatus
import observe.model.SequenceStatus.*
import observe.server.EngineState
import observe.server.SeqEvent
import org.typelevel.log4cats.Logger

import EventResult.Outcome
import EventResult.SystemUpdate
import EventResult.UserCommandResponse
import Result.PartialVal
import Result.RetVal
import UserEvent.*
import Handle.given

class Engine[F[_]: {MonadCancelThrow, Logger}] private (
  streamQueue: Queue[F, Stream[F, Event[F]]],
  inputQueue:  Queue[F, Event[F]],
  stepLoad:    (Engine[F], Observation.Id) => EngineHandle[F, SeqEvent],
  stepReload:  (Engine[F], Observation.Id, ReloadReason) => EngineHandle[F, SeqEvent]
) {
  val L: Logger[F] = Logger[F]

  /**
   * Changes the `Status` and returns the new `Queue.State`.
   */
  private def switch(obsId: Observation.Id)(st: SequenceStatus): EngineHandle[F, Unit] =
    EngineHandle.modifySequenceState(obsId)(SequenceState.status.replace(st))

  def start(obsId: Observation.Id): EngineHandle[F, Unit] =
    EngineHandle.getSequenceState(obsId).flatMap {
      case Some(seq) =>
        {
          EngineHandle.replaceSequenceState(obsId)(
            // TODO This should be before, in startChecks... and we don't need to reload afterwards
            SequenceState.status.replace(
              SequenceStatus.Running(
                userStop = HasUserStop.No,
                internalStop = HasInternalStop.No,
                waitingUserPrompt = IsWaitingUserPrompt.No,
                waitingNextStep = IsWaitingNextStep.Yes,
                starting = IsStarting.Yes
              )
            )(seq.rollback)
          ) *> send(Event.modifyState(stepReload(this, obsId, ReloadReason.SequenceFlow)))
        }.whenA(seq.status.isIdle || seq.status.isError)
      case None      => Handle.unit
    }

  def pause(id: Observation.Id): EngineHandle[F, Unit] =
    EngineHandle.modifySequenceState(id)(SequenceState.userStopSet(HasUserStop.Yes))

  private def cancelPause(id: Observation.Id): EngineHandle[F, Unit] =
    EngineHandle.modifySequenceState(id)(SequenceState.userStopSet(HasUserStop.No))

  def startSingle(c: ActionCoords): EngineHandle[F, Outcome] =
    EngineHandle.getState.flatMap { st =>
      val resultStream: Option[Stream[F, Result]] =
        for
          seq <- EngineState.sequenceStateAt(c.obsId).getOption(st)
          if (seq.status.isIdle || seq.status.isError) && !seq.getSingleState(c.actCoords).active
          act <- seq.rollback.getSingleAction(c.actCoords)
        yield act.gen

      resultStream
        .map { p =>
          EngineHandle.modifySequenceState[F](c.obsId)(u => u.startSingle(c.actCoords)) *>
            EngineHandle
              .fromEventStream(
                p.attempt.flatMap {
                  case Right(r @ Result.OK(_))    =>
                    Stream.emit(Event.singleRunCompleted(c, r))
                  case Right(e @ Result.Error(_)) =>
                    Stream.emit(Event.singleRunFailed(c, e))
                  case Right(r)                   =>
                    Stream.emit(
                      Event.singleRunFailed(
                        c,
                        Result.Error(s"Unhandled result for single run action: $r")
                      )
                    )
                  case Left(t: Throwable)         => Stream.raiseError[F](t)
                }
              )
              .as[Outcome](Outcome.Ok)
        }
        .getOrElse(EngineHandle.pure(Outcome.Failure))
    }

  private def completeSingleRun[V <: RetVal](c: ActionCoords, r: V): EngineHandle[F, Unit] =
    EngineHandle.modifySequenceState(c.obsId)(_.completeSingle(c.actCoords, r))

  private def failSingleRun(c: ActionCoords, e: Result.Error): EngineHandle[F, Unit] =
    EngineHandle.modifySequenceState(c.obsId)(_.failSingle(c.actCoords, e))

  /**
   * Tells if a sequence can be safely removed
   */
  def canUnload(obsId: Observation.Id)(st: EngineState[F]): Boolean =
    EngineState.sequenceStateAt(obsId).getOption(st).forall(canUnload)

  def canUnload(seq: SequenceState[F]): Boolean = SequenceState.canUnload(seq)

  /**
   * Refresh the steps executions of an existing sequence. Does not add nor remove steps.
   * @param id
   *   sequence identifier
   * @param steps
   *   List of new steps definitions
   * @return
   */
  def update(obsId: Observation.Id, step: Option[EngineStep[F]]): Endo[EngineState[F]] =
    EngineState.sequenceStateAt(obsId).modify(_.update(step.map(_.executions)))

  def updateStep(step: Option[EngineStep[F]]): Endo[SequenceState[F]] =
    _.update(step.map(_.executions))

  /**
   * Adds the current `Execution` to the completed `Queue`, makes the next pending `Execution` the
   * current one, and initiates the actual execution.
   *
   * If there are no more pending `Execution`s, it emits the `Finished` event.
   */
  private def next(obsId: Observation.Id): EngineHandle[F, Unit] =
    EngineHandle
      .getSequenceState(obsId)
      .flatMap(seqState =>
        seqState
          .map { seq =>
            seq.status match {
              case SequenceStatus.Running(userStop, internalStop, _, _, _) =>
                seq.withNextExecution match {
                  // Empty state
                  case None                               =>
                    send(Event.finished(obsId))
                  // Step completed (no more execution groups)
                  case Some(qs) if qs.currentStep.isEmpty =>
                    EngineHandle.replaceSequenceState(obsId)(qs) *>
                      (if (userStop || internalStop)
                         switch(obsId)(SequenceStatus.Idle) *>
                           send(Event.sequencePaused(obsId))
                       else
                         switch(obsId)(
                           SequenceStatus.Running(
                             userStop,
                             internalStop,
                             waitingUserPrompt = IsWaitingUserPrompt.No,
                             waitingNextStep = IsWaitingNextStep.Yes,
                             starting = IsStarting.No
                           )
                         ) *> send(Event.stepComplete(obsId))
                         // TODO We should clear the loaded step!
                           *> send(Event.modifyState(stepLoad(this, obsId))))
                  // Execution group completed. Check requested stop and breakpoint.
                  case Some(qs)                           =>
                    EngineHandle.replaceSequenceState(obsId)(qs) *>
                      (if (
                         qs.getCurrentBreakpoint && !qs.currentExecution.execution
                           .exists(_.uninterruptible)
                       ) {
                         switch(obsId)(SequenceStatus.Idle) *> send(Event.breakpointReached(obsId))
                       } else send(Event.executing(obsId)))
                }
              case _                                                       => EngineHandle.unit
            }
          }
          .getOrElse(EngineHandle.unit)
      )

  def startNewStep(obsId: Observation.Id): EngineHandle[F, Unit] =
    EngineHandle
      .getSequenceState(obsId)
      .flatMap(seqState =>
        seqState
          .map { seq =>
            EngineHandle.debug(s"In startnewStep with seq: $seq") >>
              {
                seq.status match {
                  case SequenceStatus
                        .Running(userStop, internalStop, _, IsWaitingNextStep.Yes, isStarting) =>
                    // TODO Review if all of these conditions are possible now.
                    if (!isStarting && (userStop || internalStop)) {
                      if (seq.currentStep.isEmpty)
                        send(Event.finished(obsId))
                      else
                        switch(obsId)(SequenceStatus.Idle)
                    } else {
                      if (seq.currentStep.isEmpty)
                        send(Event.finished(obsId))
                      else if (!isStarting && seq.getCurrentBreakpoint)
                        switch(obsId)(SequenceStatus.Idle) *> send(Event.breakpointReached(obsId))
                      else
                        switch(obsId)(
                          SequenceStatus.Running(
                            userStop,
                            internalStop,
                            IsWaitingUserPrompt.No,
                            IsWaitingNextStep.No,
                            IsStarting.No
                          )
                        ) *>
                          send(Event.executing(obsId))
                    }
                  case _ => EngineHandle.unit
                }
              }
          }
          .getOrElse(EngineHandle.unit)
      )

  /**
   * Executes all actions in the `Current` `Execution` in parallel. When all are done it emits the
   * `Executed` event. It also updates the `State` as needed.
   */
  // Send the expected event when the `Action` is executed
  // It doesn't catch run time exceptions. If desired, the Action has to do it itself.
  private def act(
    id:     Observation.Id,
    stepId: Step.Id,
    t:      (Stream[F, Result], Int)
  ): Stream[F, Event[F]] = t match {
    case (gen, i) =>
      gen
        .takeThrough:
          case Result.Partial(_) => true
          case _                 => false
        .attempt
        .flatMap:
          case Right(r @ Result.OK(_))        => Stream.emit(Event.completed(id, stepId, i, r))
          case Right(r @ Result.OKStopped(_)) => Stream.emit(Event.stopCompleted(id, stepId, i, r))
          case Right(r @ Result.OKAborted(_)) => Stream.emit(Event.aborted(id, stepId, i, r))
          case Right(r @ Result.Partial(_))   => Stream.emit(Event.partial(id, stepId, i, r))
          case Right(e @ Result.Error(_))     => Stream.emit(Event.failed(id, stepId, i, e))
          case Right(r @ Result.Paused(_))    => Stream.emit(Event.paused(id, stepId, i, r))
          case Left(t: Throwable)             => Stream.raiseError[F](t)
  }

  private def execute(obsId: Observation.Id)(using Concurrent[F]): EngineHandle[F, Unit] =
    EngineHandle.getState.flatMap(st =>
      EngineState
        .sequenceStateAt(obsId)
        .getOption(st)
        .map { seq =>
          seq.currentStep match
            case None    =>
              // No step executing — sequence is done
              EngineHandle.replaceSequenceState(obsId)(seq) >>
                send(Event.finished(obsId))
            case Some(z) =>
              val stepId                         = z.id
              val u: List[Stream[F, Event[F]]]   =
                seq.currentExecution.actions
                  .map(_.gen)
                  .zipWithIndex
                  .map(act(obsId, stepId, _))
              val v: Stream[F, Event[F]]         = Stream.emits(u).parJoin(u.length)
              val w: List[EngineHandle[F, Unit]] =
                seq.currentExecution.actions.indices
                  .map(i => EngineHandle.modifySequenceState[F](obsId)(_.start(i)))
                  .toList
              w.sequence *> Handle.fromEventStream(v)
        }
        .getOrElse(EngineHandle.unit)
    )

  private def actionStop(
    obsId: Observation.Id,
    f:     EngineState[F] => Stream[F, Event[F]]
  ): EngineHandle[F, Unit] =
    EngineHandle
      .getSequenceState(obsId)
      .flatMap(_.map { s =>
        (EngineHandle.fromEventStream(f) >>
          EngineHandle.modifySequenceState(obsId)(
            SequenceState.internalStopSet(HasInternalStop.Yes)
          ))
          .whenA(SequenceState.isRunning(s))
      }.getOrElse(EngineHandle.unit))

  /**
   * Given the index of the completed `Action` in the current `Execution`, it marks the `Action` as
   * completed and returns the new updated `State`.
   *
   * When the index doesn't exist it does nothing.
   */
  private def complete[R <: RetVal](
    obsId: Observation.Id,
    i:     Int,
    r:     Result.OK[R]
  ): EngineHandle[F, Unit] =
    EngineHandle.modifySequenceState[F](obsId)(_.mark(i)(r)) *>
      EngineHandle
        .getSequenceState(obsId)
        .flatMap(
          _.flatMap(
            _.currentExecution.execution
              .forall(Action.completed)
              .option(EngineHandle.fromSingleEvent(Event.executed(obsId)))
          ).getOrElse(EngineHandle.unit)
        )

  private def stopComplete[R <: RetVal](
    obsId: Observation.Id,
    i:     Int,
    r:     Result.OKStopped[R]
  ): EngineHandle[F, Unit] =
    EngineHandle.modifySequenceState[F](obsId)(_.mark(i)(r)) *>
      EngineHandle
        .getSequenceState(obsId)
        .flatMap(
          _.flatMap(
            _.currentExecution.execution
              .forall(Action.completed)
              .option(Handle.fromSingleEvent(Event.executed(obsId)))
          ).getOrElse(EngineHandle.unit)
        )

  private def abort[R <: RetVal](
    obsId: Observation.Id,
    i:     Int,
    r:     Result.OKAborted[R]
  ): EngineHandle[F, Unit] =
    EngineHandle.modifySequenceState[F](obsId)(_.mark(i)(r)) *>
      switch(obsId)(SequenceStatus.Aborted)

  private def partialResult[R <: PartialVal](
    obsId: Observation.Id,
    i:     Int,
    p:     Result.Partial[R]
  ): EngineHandle[F, Unit] =
    EngineHandle.modifySequenceState(obsId)(_.mark(i)(p))

  def actionPause(id: Observation.Id, i: Int, p: Result.Paused): EngineHandle[F, Unit] =
    EngineHandle.modifySequenceState(id)(s =>
      SequenceState.internalStopSet(HasInternalStop.No)(s).mark(i)(p)
    )

  private def actionResume(
    obsId: Observation.Id,
    i:     Int,
    cont:  Stream[F, Result]
  ): EngineHandle[F, Unit] =
    EngineHandle
      .getSequenceState(obsId)
      .flatMap(_.collect {
        case s
            if s.currentStep.exists(z =>
              SequenceState
                .isRunning(s) && s.currentExecution.execution.lift(i).exists(Action.paused)
            ) =>
          EngineHandle.modifySequenceState[F](obsId)(_.start(i)) *>
            EngineHandle.fromEventStream(act(obsId, s.currentStep.get.id, (cont, i)))
      }.getOrElse(EngineHandle.unit))

  /**
   * For now it only changes the `Status` to `Paused` and returns the new `State`. In the future
   * this function should handle the failed action.
   */
  private def fail(obsId: Observation.Id)(i: Int, e: Result.Error): EngineHandle[F, Unit] =
    EngineHandle.modifySequenceState[F](obsId)(_.mark(i)(e)) *>
      switch(obsId)(SequenceStatus.Failed(e.msg))

  private def logError(e: Result.Error): EngineHandle[F, Unit] = error(e.errMsg.getOrElse(e.msg))

  /**
   * Log info lifted into Handle.
   */
  private def info(msg: => String): EngineHandle[F, Unit] = EngineHandle.liftF(L.info(msg))

  /**
   * Log warning lifted into Handle.
   */
  private def warning(msg: => String): EngineHandle[F, Unit] = EngineHandle.liftF(L.warn(msg))

  /**
   * Log debug lifted into Handle.
   */
  private def debug(msg: => String): EngineHandle[F, Unit] = EngineHandle.liftF(L.debug(msg))

  /**
   * Log error lifted into Handle
   */
  private def error(msg: => String): EngineHandle[F, Unit] = EngineHandle.liftF(L.error(msg))

  /**
   * Enqueue `Event` in the Handle.
   */
  private def send(ev: Event[F]): EngineHandle[F, Unit] = Handle.fromEventStream(Stream(ev))

  private def handleUserEvent(ue: UserEvent[F]): EngineHandle[F, EventResult] = ue match {
    case Start(obsId, _, _)                =>
      debug(s"Engine: Start requested for sequence $obsId") *> start(obsId) *>
        EngineHandle.pure(UserCommandResponse(ue, Outcome.Ok, None))
    case Pause(obsId, _)                   =>
      debug(s"Engine: Pause requested for sequence $obsId") *> pause(obsId) *>
        EngineHandle.pure(UserCommandResponse(ue, Outcome.Ok, None))
    case CancelPause(obsId, _)             =>
      debug(s"Engine: Pause canceled for sequence $obsId") *> cancelPause(obsId) *>
        EngineHandle.pure(UserCommandResponse(ue, Outcome.Ok, None))
    case Breakpoints(obsId, _, stepIds, v) =>
      debug(s"Engine: breakpoints changed for sequence $obsId and steps $stepIds to $v") *>
        EngineHandle.modifySequenceState[F](obsId)(_.setBreakpoints(stepIds.map(id => (id, v)))) *>
        EngineHandle.pure(UserCommandResponse(ue, Outcome.Ok, None))
    case Poll(_)                           =>
      debug("Engine: Polling current state") *>
        EngineHandle.pure(UserCommandResponse(ue, Outcome.Ok, None))
    case GetState(f)                       =>
      EngineHandle.fromEventStream(f) *>
        EngineHandle.pure(UserCommandResponse(ue, Outcome.Ok, None))
    case ModifyState(f)                    =>
      f.map((r: SeqEvent) => UserCommandResponse[F](ue, Outcome.Ok, Some(r)))
    case ActionStop(obsId, f)              =>
      debug("Engine: Action stop requested") *> actionStop(obsId, f) *>
        EngineHandle.pure(UserCommandResponse(ue, Outcome.Ok, None))
    case ActionResume(obsId, i, cont)      =>
      debug("Engine: Action resume requested") *> actionResume(obsId, i, cont) *>
        EngineHandle.pure(UserCommandResponse(ue, Outcome.Ok, None))
    case LogDebug(msg, _)                  =>
      debug(msg) *> EngineHandle.pure(UserCommandResponse(ue, Outcome.Ok, None))
    case LogInfo(msg, _)                   =>
      info(msg) *> EngineHandle.pure(UserCommandResponse(ue, Outcome.Ok, None))
    case LogWarning(msg, _)                =>
      warning(msg) *> EngineHandle.pure(UserCommandResponse(ue, Outcome.Ok, None))
    case LogError(msg, _)                  =>
      error(msg) *> EngineHandle.pure(UserCommandResponse(ue, Outcome.Ok, None))
    case Pure(v)                           =>
      EngineHandle.pure(UserCommandResponse(ue, Outcome.Ok, v.some))
  }

  private def handleSystemEvent(
    se: SystemEvent
  )(using Concurrent[F]): EngineHandle[F, EventResult] =
    import SystemEvent.*
    se match {
      case Completed(obsId, _, i, r)     =>
        debug(s"Engine: From sequence $obsId: Action completed ($r)") *>
          complete(obsId, i, r) *>
          EngineHandle.pure(SystemUpdate(se, Outcome.Ok))
      case StopCompleted(obsId, _, i, r) =>
        debug(s"Engine: From sequence $obsId: Action completed with stop ($r)") *>
          stopComplete(obsId, i, r) *>
          EngineHandle.pure(SystemUpdate(se, Outcome.Ok))
      case Aborted(obsId, _, i, r)       =>
        debug(s"Engine: From sequence $obsId: Action completed with abort ($r)") *>
          abort(obsId, i, r) *>
          EngineHandle.pure(SystemUpdate(se, Outcome.Ok))
      case PartialResult(obsId, _, i, r) =>
        debug(s"Engine: From sequence $obsId: Partial result ($r)") *>
          partialResult(obsId, i, r) *>
          EngineHandle.pure(SystemUpdate(se, Outcome.Ok))
      case Paused(obsId, _, i, r)        =>
        debug("Engine: Action paused") *>
          actionPause(obsId, i, r) *> EngineHandle.pure(SystemUpdate(se, Outcome.Ok))
      case Failed(obsId, _, i, e)        =>
        logError(e) *> fail(obsId)(i, e) *>
          EngineHandle.pure(SystemUpdate(se, Outcome.Ok))
      case LoadFailed(obsId, i, e)       =>
        logError(e) *> fail(obsId)(i, e) *>
          EngineHandle.pure(SystemUpdate(se, Outcome.Ok))
      case Busy(obsId, _)                =>
        warning(
          s"Cannot run sequence $obsId " +
            s"because " +
            s"required systems are in use."
        ) *> EngineHandle.pure(SystemUpdate(se, Outcome.Ok))
      case BreakpointReached(obsId)      =>
        debug(s"Engine: Breakpoint reached in observation [$obsId]") *>
          EngineHandle.pure(SystemUpdate(se, Outcome.Ok))
      case Executed(obsId)               =>
        debug(s"Engine: Execution $obsId completed") *>
          next(obsId) *> EngineHandle.pure(SystemUpdate(se, Outcome.Ok))
      case Executing(obsId)              =>
        debug("Engine: Executing") *>
          execute(obsId) *> EngineHandle.pure(SystemUpdate(se, Outcome.Ok))
      case StepComplete(obsId)           =>
        debug(s"Engine: Step completed for observation [$obsId]") *>
          EngineHandle.pure(SystemUpdate(se, Outcome.Ok))
      case SequencePaused(obsId)         =>
        debug(s"Engine: Sequence paused for observation [$obsId]") *>
          EngineHandle.pure(SystemUpdate(se, Outcome.Ok))
      case SequenceComplete(obsId)       =>
        debug("Engine: Finished") *>
          switch(obsId)(SequenceStatus.Completed) *> EngineHandle.pure(SystemUpdate(se, Outcome.Ok))
      case SingleRunCompleted(c, r)      =>
        debug(s"Engine: single action $c completed with result $r") *>
          completeSingleRun(c, r.response) *> EngineHandle.pure(SystemUpdate(se, Outcome.Ok))
      case SingleRunFailed(c, e)         =>
        debug(s"Engine: single action $c failed with error $e") *>
          failSingleRun(c, e) *> EngineHandle.pure(SystemUpdate(se, Outcome.Ok))
      case Null                          => EngineHandle.pure(SystemUpdate(se, Outcome.Ok))
    }

  /**
   * Main logical thread to handle events and produce output.
   */
  private def run(
    onSystemEvent: PartialFunction[SystemEvent, EngineHandle[F, Unit]]
  )(ev: Event[F])(using Concurrent[F]): EngineHandle[F, EventResult] =
    ev match
      case Event.EventUser(ue)   => handleUserEvent(ue)
      case Event.EventSystem(se) =>
        handleSystemEvent(se).flatMap: (r: EventResult) =>
          onSystemEvent.applyOrElse(se, (_: SystemEvent) => EngineHandle.unit).as(r)

  /** Traverse a process with a stateful computation. */
  // input, stream of events
  // initalState: state
  // f takes an event and the current state, it produces a new state, a new value B and more actions
  def mapEvalState(
    initialState: EngineState[F],
    f:            (
      Event[F],
      EngineState[F]
    ) => F[(EngineState[F], (EventResult, EngineState[F]), Stream[F, Event[F]])]
  )(using Concurrent[F]): Stream[F, (EventResult, EngineState[F])] =
    Stream.exec(streamQueue.offer(Stream.fromQueueUnterminated(inputQueue))) ++
      Stream
        .fromQueueUnterminated(streamQueue)
        .parJoinUnbounded
        .evalMapAccumulate(initialState): (s, a) =>
          f(a, s).flatMap:
            // Optimization to avoid processing empty streams.
            case (ns, b, Stream.empty) => (ns, b).pure[F]
            case (ns, b, st)           => streamQueue.offer(st) >> (ns, b).pure[F]
        .map(_._2)

  private def runE(
    onSystemEvent: PartialFunction[SystemEvent, EngineHandle[F, Unit]]
  )(ev: Event[F], s: EngineState[F])(using
    ci:            Concurrent[F]
  ): F[(EngineState[F], (EventResult, EngineState[F]), Stream[F, Event[F]])] =
    run(onSystemEvent)(ev).stateT.run(s).map { case (si, (r, p)) =>
      (si, (r, si), p)
    }

  // Only used for testing.
  def process(
    onSystemEvent: PartialFunction[SystemEvent, Handle[F, EngineState[F], Event[F], Unit]]
  )(s0: EngineState[F])(using
    ev:            Concurrent[F]
  ): Stream[F, (EventResult, EngineState[F])] =
    mapEvalState(s0, runE(onSystemEvent)(_, _))

  def offer(in: Event[F]): F[Unit] = inputQueue.offer(in)

  def inject(f: F[Event[F]]): F[Unit] = streamQueue.offer(Stream.eval(f))

}

object Engine {

  trait State[F[_], D] {
    def sequenceStateIndex(sid: Observation.Id): Optional[D, SequenceState[F]]
  }

  trait Types[S, E] {
    type StateType = S
    type EventData = E
  }

  /**
   * Redefines an existing sequence. Changes the step actions, removes steps, adds new steps.
   */
  def reload[F[_]](
    oldSeqState: SequenceState[F],
    step:        Option[EngineStep[F]]
  ): SequenceState[F] =
    SequenceState.reload(step, oldSeqState)

  def build[F[_]: {Concurrent, Logger}](
    loadNextStep:   (Engine[F], Observation.Id) => EngineHandle[F, SeqEvent],
    reloadNextStep: (Engine[F], Observation.Id, ReloadReason) => EngineHandle[F, SeqEvent]
  ): F[Engine[F]] = for {
    sq <- Queue.unbounded[F, Stream[F, Event[F]]]
    iq <- Queue.unbounded[F, Event[F]]
  } yield new Engine(sq, iq, loadNextStep, reloadNextStep)

}

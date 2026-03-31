// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.engine

import cats.data.NonEmptyList
import cats.data.OptionT
import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosLong
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation as LObservation
import lucuma.core.model.sequence.Step
import observe.common.test.*
import observe.model.ActionType
import observe.model.SequenceStatus
import observe.server.EngineState
import observe.server.SeqEvent
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.Function.const

class SequenceSuite extends munit.CatsEffectSuite {

  private given Logger[IO] = Slf4jLogger.getLoggerFromName[IO]("observe-engine")

  private val obsId = LObservation.Id(PosLong.unsafeFrom(1))

  // All tests check the output of running a sequence against the expected sequence of updates.

  private val executionEngine = Engine.build[IO]((_, obsId, _, _) =>
    // Assuming a 1-step sequence.
    EngineHandle
      .fromSingleEvent[IO](Event.sequenceComplete(obsId))
      .as(SeqEvent.SequenceCompleted(obsId))
  )

  def simpleStep(id: Step.Id): EngineStep[IO] =
    EngineStep(
      id = id,
      executions = List(
        NonEmptyList.of(action, action), // Execution
        NonEmptyList.one(action) // Execution
      )
    )

  def isFinished(status: SequenceStatus): Boolean = status match {
    case SequenceStatus.Idle      => true
    case SequenceStatus.Completed => true
    case SequenceStatus.Failed(_) => true
    case _                        => false
  }

  def runToCompletion(s0: EngineState[IO]): IO[Option[EngineState[IO]]] =
    for {
      eng <- executionEngine
      _   <- eng.offer(Event.modifyState(eng.startLoadedStep(obsId).as(SeqEvent.NullSeqEvent)))
      v   <- eng
               .process(PartialFunction.empty)(s0)
               .drop(1)
               .takeThrough(a => !isFinished(a._2.sequences(obsId).seq.status))
               .compile
               .last
    } yield v.map(_._2)

  test("stop on breakpoints") {
    // With single-step execution, a breakpoint on the current step
    // causes getCurrentBreakpoint to return true and the sequence stays Idle.
    val seq = initSeqState(
      obsId,
      simpleStep(stepId(1)),
      SequenceType.Science,
      Breakpoints(Set(stepId(1)))
    )

    assert(seq.status === SequenceStatus.Idle)
    assert(seq.getCurrentBreakpoint)
  }

  test("resume execution to completion after a breakpoint") {
    // Verify breakpoint is initially set
    val seqWithBp: SequenceState[IO] = initSeqState(
      obsId,
      simpleStep(stepId(1)),
      SequenceType.Science,
      Breakpoints(Set(stepId(1))),
      SequenceStatus.Running.Init
    )
    assert(seqWithBp.getCurrentBreakpoint)

    // Clear breakpoint and run to completion
    val cleared: SequenceState[IO] =
      seqWithBp.setBreakpoints(Set((stepId(1), Breakpoint.Disabled)))
    assert(!cleared.getCurrentBreakpoint)

    val qs0: EngineState[IO] =
      TestUtil.initStateWithSequence(obsId, cleared)

    val qs1: IO[Option[EngineState[IO]]] = runToCompletion(qs0)

    (for {
      s <- OptionT(qs1)
      t <- OptionT.pure(s.sequences(obsId))
    } yield t.seq.status === SequenceStatus.Completed && t.seq.loadedStep.isEmpty).value
      .map(_.getOrElse(fail("Sequence not found")))
      .assert
  }

  // TODO: Share these fixtures with StepSpec
  private object DummyResult extends Result.RetVal
  private val result: Result     = Result.OK(DummyResult)
  private val action: Action[IO] = fromF[IO](ActionType.Undefined, IO(result))

  def simpleStep2(
    pending: List[ParallelActions[IO]],
    focus:   Execution[IO],
    done:    List[NonEmptyList[Result]]
  ): ExecutionZipper[IO] = {
    val rollback: (Execution[IO], List[ParallelActions[IO]]) = {
      val doneParallelActions: List[ParallelActions[IO]]  = done.map(_.map(const(action)))
      val focusParallelActions: List[ParallelActions[IO]] = focus.toParallelActionsList
      doneParallelActions ++ focusParallelActions ++ pending match {
        case Nil     => (Execution.empty, Nil)
        case x :: xs => (Execution(x.toList), xs)
      }
    }

    ExecutionZipper(
      id = stepId(1),
      pending = pending,
      focus = focus,
      done = done.map(_.map { r =>
        val x = fromF[IO](ActionType.Observe, IO(r))
        x.copy(state = Execution.actionStateFromResult(r)(x.state))
      }),
      rolledback = rollback
    )

  }
  // Step zipper with done.nonEmpty (step has progressed past initial execution)
  val stepzr0: ExecutionZipper[IO] =
    simpleStep2(Nil, Execution.empty, List(NonEmptyList.one(result)))

  test("startSingle should mark a single Action as started") {
    val seq = initSeqState(
      obsId,
      simpleStep(stepId(1)),
      SequenceType.Science,
      Breakpoints(Set(stepId(1)))
    )

    val c = ActionCoordsInSeq(stepId(1), ExecutionIndex(0), ActionIndex(1))

    assert(
      seq
        .startSingle(c)
        .getSingleState(c) == Action.ActionState.Started
    )
  }

  test("startSingle should not start single Action from completed Step") {
    // A step that has progressed past initial execution (done.nonEmpty)
    val seq1 = SequenceState[IO](
      obsId = obsId,
      status = SequenceStatus.Running.Init,
      loadedStep = Some(LoadedStep(DummyStepGen, stepzr0)),
      currentSequenceType = SequenceType.Science,
      breakpoints = Breakpoints.empty,
      singleRuns = Map.empty
    )
    val c1   = ActionCoordsInSeq(stepId(1), ExecutionIndex(0), ActionIndex(0))

    assert(seq1.startSingle(c1).getSingleState(c1).isIdle)
  }

  test("failSingle should mark a single running Action as failed") {
    val c   = ActionCoordsInSeq(stepId(1), ExecutionIndex(0), ActionIndex(0))
    val seq = initSeqState(
      obsId,
      simpleStep(stepId(1)),
      SequenceType.Science,
      Breakpoints.empty
    ).startSingle(c)
    val c2  = ActionCoordsInSeq(stepId(1), ExecutionIndex(1), ActionIndex(0))

    assert(seq.failSingle(c, Result.Error("")).getSingleState(c).errored)
    assert(seq.failSingle(c2, Result.Error("")).getSingleState(c2).isIdle)
  }

  test("failSingle should mark a single running Action as completed") {
    val c   = ActionCoordsInSeq(stepId(1), ExecutionIndex(0), ActionIndex(0))
    val seq = initSeqState(
      obsId,
      simpleStep(stepId(1)),
      SequenceType.Science,
      Breakpoints.empty
    ).startSingle(c)

    assert(seq.completeSingle(c, DummyResult).getSingleState(c).completed)
  }

}

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.engine

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.Ref
import cats.implicits.*
import fs2.Stream
import lucuma.core.enums.Instrument.GmosSouth
import lucuma.core.enums.SequenceType
import munit.CatsEffectSuite
import observe.common.test.*
import observe.model.ActionType
import observe.model.ClientId
import observe.model.SequenceStatus
import observe.model.SequenceStatus.*
import observe.model.SequenceStatus.HasUserStop
import observe.model.StepState
import observe.model.enums.Resource
import observe.server.EngineState
import observe.server.SeqEvent
import observe.server.engine.EventResult.*
import observe.server.engine.SystemEvent.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.util.UUID
import scala.Function.const
import scala.concurrent.duration.*

class StepSuite extends CatsEffectSuite {

  private given L: Logger[IO] = Slf4jLogger.getLoggerFromName[IO]("observe")

  private val seqId = observationId(1)

  private val executionEngine = Engine.build[IO]((eng, obsId) =>
    eng.startLoadedStep(obsId).as(SeqEvent.NullSeqEvent)
    // (eng, obsId, _) => eng.startLoadedStep(obsId).as(SeqEvent.NullSeqEvent)
  )

  private object DummyResult extends Result.RetVal with Serializable
  private val result                      = Result.OK(DummyResult)
  private val failure                     = Result.Error("Dummy error")
  private val actionFailed                = fromF[IO](ActionType.Undefined, IO(failure))
    .copy(state = Action.State(Action.ActionState.Failed(failure), Nil))
  private val action: Action[IO]          = fromF[IO](ActionType.Undefined, IO(result))
  private val actionCompleted: Action[IO] =
    action.copy(state = Action.State(Action.ActionState.Completed(DummyResult), Nil))
  private val clientId: ClientId          = ClientId(UUID.randomUUID)

  def simpleStep(
    pending: List[ParallelActions[IO]],
    focus:   Execution[IO],
    done:    List[NonEmptyList[Result]]
  ): EngineStep.ExecutionZipper[IO] = {
    val rollback: (Execution[IO], List[ParallelActions[IO]]) = {
      val doneParallelActions: List[ParallelActions[IO]]  = done.map(_.map(const(action)))
      val focusParallelActions: List[ParallelActions[IO]] = focus.toParallelActionsList
      doneParallelActions ++ focusParallelActions ++ pending match {
        case Nil     => (Execution.empty, Nil)
        case x :: xs => (Execution(x.toList), xs)
      }
    }

    EngineStep.ExecutionZipper(
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

  val stepz0: EngineStep.ExecutionZipper[IO]   = simpleStep(Nil, Execution.empty, Nil)
  val stepza0: EngineStep.ExecutionZipper[IO]  =
    simpleStep(List(NonEmptyList.one(action)), Execution.empty, Nil)
  val stepza1: EngineStep.ExecutionZipper[IO]  =
    simpleStep(List(NonEmptyList.one(action)), Execution(List(actionCompleted)), Nil)
  val stepzr0: EngineStep.ExecutionZipper[IO]  =
    simpleStep(Nil, Execution.empty, List(NonEmptyList.one(result)))
  val stepzr1: EngineStep.ExecutionZipper[IO]  =
    simpleStep(Nil, Execution(List(actionCompleted, actionCompleted)), Nil)
  val stepzr2: EngineStep.ExecutionZipper[IO]  = simpleStep(
    Nil,
    Execution(List(actionCompleted, actionCompleted)),
    List(NonEmptyList.one(result))
  )
  val stepzar0: EngineStep.ExecutionZipper[IO] =
    simpleStep(Nil, Execution(List(actionCompleted, action)), Nil)
  val stepzar1: EngineStep.ExecutionZipper[IO] = simpleStep(
    List(NonEmptyList.one(action)),
    Execution(List(actionCompleted, actionCompleted)),
    List(NonEmptyList.one(result))
  )
  private val startEvent                       = ???
  // Event.start[IO](seqId, user, clientId)

  /**
   * Emulates TCS configuration in the real world.
   */
  val configureTcs: Action[IO] = fromF[IO](
    ActionType.Configure(Resource.TCS),
    for {
      _ <- L.info("System: Start TCS configuration")
      _ <- IO.sleep(new FiniteDuration(200, MILLISECONDS))
      _ <- L.info("System: Complete TCS configuration")
    } yield Result.OK(DummyResult)
  )

  /**
   * Emulates Instrument configuration in the real world.
   */
  val configureInst: Action[IO] = fromF[IO](
    ActionType.Configure(GmosSouth),
    for {
      _ <- L.info("System: Start Instrument configuration")
      _ <- IO.sleep(new FiniteDuration(150, MILLISECONDS))
      _ <- L.info("System: Complete Instrument configuration")
    } yield Result.OK(DummyResult)
  )

  /**
   * Emulates an observation in the real world.
   */
  val observe: Action[IO] = fromF[IO](
    ActionType.Observe,
    for {
      _ <- L.info("System: Start observation")
      _ <- IO.sleep(new FiniteDuration(200, MILLISECONDS))
      _ <- L.info("System: Complete observation")
    } yield Result.OK(DummyResult)
  )

  def error(errMsg: String): Action[IO] = fromF[IO](
    ActionType.Undefined,
    IO.sleep(new FiniteDuration(200, MILLISECONDS)) *>
      Result.Error(errMsg).pure[IO]
  )

  def aborted: Action[IO] = fromF[IO](ActionType.Undefined,
                                      IO.sleep(new FiniteDuration(200, MILLISECONDS)) *>
                                        Result.OKAborted(DummyResult).pure[IO]
  )

  def errorSet1(errMsg: String): (Ref[IO, Int], Action[IO]) = {
    val ref    = Ref.unsafe[IO, Int](0)
    val action = fromF[IO](ActionType.Undefined,
                           ref.update(_ + 1).as(Result.OK(DummyResult)),
                           Result.Error(errMsg).pure[IO],
                           ref.update(_ + 1).as(Result.OK(DummyResult))
    )
    (ref, action)
  }

  def errorSet2(errMsg: String): Action[IO] =
    fromF[IO](ActionType.Undefined, Result.Error(errMsg).pure[IO])

  def fatalError(errMsg: String): Action[IO] =
    fromF[IO](ActionType.Undefined, IO.raiseError(new RuntimeException(errMsg)))

  def triggerPause(eng: Engine[IO]): Action[IO] = fromF[IO](
    ActionType.Undefined,
    for {
      _ <- eng.offer(Event.pause(seqId, user))
      // There is not a distinct result for Pause because the Pause action is a
      // trick for testing but we don't need to support it in real life, the pause
      // input event is enough.
    } yield Result.OK(DummyResult)
  )

  def triggerStart(eng: Engine[IO]): Action[IO] = fromF[IO](
    ActionType.Undefined,
    for {
      // _ <- eng.offer(Event.start(seqId, user, clientId))
      _ <- eng.offer(???)
      // Same case that the pause action
    } yield Result.OK(DummyResult)
  )

  def isFinished(status: SequenceStatus): Boolean = status match {
    case SequenceStatus.Idle      => true
    case SequenceStatus.Completed => true
    case SequenceStatus.Failed(_) => true
    case SequenceStatus.Aborted   => true
    case _                        => false
  }

  private def runToCompletioIO(s0: EngineState[IO]) =
    for {
      eng <- executionEngine
      // _   <- eng.offer(Event.start[IO](seqId, user, clientId))
      _   <- eng.offer(???)
    } yield eng
      .process(PartialFunction.empty)(s0)
      .drop(1)
      .takeThrough(a => !isFinished(a._2.sequences(seqId).seq.status))
      .map(_._2)
      .compile

  def runToCompletionLastIO(s0: EngineState[IO]): IO[Option[EngineState[IO]]] =
    runToCompletioIO(s0).flatMap(_.last)

  def runToCompletionAllIO(s0: EngineState[IO]): IO[List[EngineState[IO]]] =
    runToCompletioIO(s0).flatMap(_.toList)

  // This test must have a simple step definition and the known sequence of updates that running that step creates.
  // The test will just run step and compare the output with the predefined sequence of updates.

  // The difficult part is to set the pause command to interrupts the step execution in the middle.
  // TODO: With single-step execution, the step may complete before the pause event is processed.
  // This test needs adaptation to the new timing characteristics.
  test(
    "pause should stop execution in response to a pause command, after current step completes".ignore
  ) {
    def qs0(eng: Engine[IO]): EngineState[IO] =
      TestUtil.initStateWithSequence(
        seqId,
        SequenceState.init(
          obsId = seqId,
          loadedStep = EngineStep(
            id = stepId(1),
            executions = List(
              NonEmptyList.of(configureTcs, configureInst, triggerPause(eng)), // Execution
              NonEmptyList.one(observe) // Execution
            )
          ),
          sequenceType = SequenceType.Science,
          breakpoints = Breakpoints.empty
        )
      )

    def notFinished(v: (EventResult, EngineState[IO])): Boolean =
      !isFinished(v._2.sequences(seqId).seq.status)

    val m =
      for {
        eng <- Stream.eval(executionEngine)
        _   <- Stream.eval(eng.offer(startEvent))
        u   <- eng
                 .process(PartialFunction.empty)(qs0(eng))
                 .drop(1)
                 .takeThrough(notFinished)
                 .map(_._2)
      } yield u.sequences(seqId)

    m.compile.last.map { l =>
      l.map(_.seq).exists { s =>
        // After pause, the step should still be in progress (observe execution pending)
        // and sequence status should be Idle (paused)
        s.currentStep.isDefined && s.status === SequenceStatus.Idle
      }
    }.assert

  }

  test(
    "resume execution from the non-running state in response to a resume command, rolling back a partially run step."
  ) {
    // Engine state with one idle sequence partially executed. Step partially done.
    val qs0: EngineState[IO] =
      TestUtil.initStateWithSequence(
        seqId,
        SequenceState[IO](
          obsId = observationId(1),
          status = SequenceStatus.Idle,
          currentStep = Some(
            EngineStep.ExecutionZipper(
              id = stepId(2),
              pending = Nil,
              focus = Execution(List(observe)),
              done = List(NonEmptyList.of(actionCompleted, actionCompleted)),
              rolledback =
                (Execution(List(configureTcs, configureInst)), List(NonEmptyList.one(observe)))
            )
          ),
          currentSequenceType = SequenceType.Science,
          breakpoints = Breakpoints.empty,
          singleRuns = Map.empty
        )
      )

    val qs1 =
      for {
        eng <- executionEngine
        _   <- eng.offer(startEvent)
        u   <- eng
                 .process(PartialFunction.empty)(qs0)
                 .take(1)
                 .compile
                 .last
      } yield u.flatMap(_._2.sequences.get(seqId))

    qs1.map {
      _.map(_.seq).exists { s =>
        s.currentStep.exists { z =>
          z.toEngineStep match {
            case EngineStep(_, List(ex1, ex2)) =>
              Execution(ex1.toList).actions.length == 2 && Execution(
                ex2.toList
              ).actions.length == 1
            case _                             => false
          }
        } && s.status.isRunning
      }
    }.assert

  }

  test("cancel a pause request in response to a cancel pause command.") {
    val qs0: EngineState[IO] =
      TestUtil.initStateWithSequence(
        seqId,
        SequenceState[IO](
          obsId = observationId(1),
          status = SequenceStatus.Running(
            HasUserStop.Yes,
            HasInternalStop.No,
            IsWaitingUserPrompt.No,
            IsWaitingNextStep.No,
            IsStarting.No
          ),
          currentStep = Some(
            EngineStep.ExecutionZipper(
              id = stepId(2),
              pending = Nil,
              focus = Execution(List(observe)),
              done = List(NonEmptyList.of(actionCompleted, actionCompleted)),
              rolledback =
                (Execution(List(configureTcs, configureInst)), List(NonEmptyList.one(observe)))
            )
          ),
          currentSequenceType = SequenceType.Science,
          breakpoints = Breakpoints.empty,
          singleRuns = Map.empty
        )
      )

    val qs1 = for {
      eng <- executionEngine
      _   <- eng.offer(Event.cancelPause[IO](seqId, user))
      v   <- eng
               .process(PartialFunction.empty)(qs0)
               .take(1)
               .compile
               .last
               .map(_.map(_._2))
    } yield v

    qs1
      .map(x =>
        x.flatMap(_.sequences.get(seqId)).map(_.seq).exists { s =>
          s.status.isRunning
        }
      )
      .assert

  }

  test("engine should test pause command if step is not being executed.") {
    val qs0: EngineState[IO] =
      TestUtil.initStateWithSequence(
        seqId,
        SequenceState.init(
          obsId = seqId,
          loadedStep = EngineStep(
            id = stepId(1),
            executions = List(
              NonEmptyList.of(configureTcs, configureInst), // Execution
              NonEmptyList.one(observe) // Execution
            )
          ),
          sequenceType = SequenceType.Science,
          breakpoints = Breakpoints.empty
        )
      )

    val qss = for {
      eng <- executionEngine
      _   <- eng.offer(Event.pause[IO](seqId, user))
      v   <- eng
               .process(PartialFunction.empty)(qs0)
               .take(1)
               .compile
               .last
               .map(_.map(_._2))
    } yield v

    qss.map { x =>
      x.flatMap(_.sequences.get(seqId)).map(_.seq).exists { s =>
        s.currentStep.exists { z =>
          z.toEngineStep match {
            case EngineStep(_, List(ex1, ex2)) =>
              Execution(ex1.toList).actions.length == 2 && Execution(
                ex2.toList
              ).actions.length == 1
            case _                             => false
          }
        } && (s.status === SequenceStatus.Idle)
      }
    }.assert
  }

  // Be careful that start command doesn't run an already running sequence.
  test("engine test start command if step is already running.") {
    def qs0(eng: Engine[IO]): EngineState[IO] =
      TestUtil.initStateWithSequence(
        seqId,
        SequenceState.init(
          obsId = seqId,
          loadedStep = EngineStep(
            id = stepId(1),
            executions = List(
              NonEmptyList.of(configureTcs, configureInst), // Execution
              NonEmptyList.one(triggerStart(eng)),          // Execution
              NonEmptyList.one(observe) // Execution
            )
          ),
          sequenceType = SequenceType.Science,
          breakpoints = Breakpoints.empty
        )
      )

    val qss = for {
      eng <- executionEngine
      // _   <- eng.offer(Event.start(seqId, user, clientId))
      _   <- eng.offer(???)
      v   <- eng
               .process(PartialFunction.empty)(qs0(eng))
               .drop(1)
               .takeThrough(a => !isFinished(a._2.sequences(seqId).seq.status))
               .compile
               .toVector
    } yield v

    qss.map { x =>
      val actionsCompleted = x.map(_._1).collect { case SystemUpdate(x: Completed[?], _) => x }
      assertEquals(actionsCompleted.length, 4)

      val executionsCompleted = x.map(_._1).collect { case SystemUpdate(x: Executed, _) => x }
      assertEquals(executionsCompleted.length, 3)

      val sequencesCompleted =
        x.map(_._1).collect { case SystemUpdate(x: SequenceComplete, _) => x }
      assertEquals(sequencesCompleted.length, 1)

      x.lastOption
        .flatMap(_._2.sequences.get(seqId))
        .map(_.seq)
        .exists { s =>
          s.currentStep.isEmpty && s.status === SequenceStatus.Completed
        }
    }.assert
  }

  // For this test, one of the actions in the step must produce an error as result.
  test("engine should stop execution and propagate error when an Action ends in error.") {
    val errMsg               = "Dummy error"
    val qs0: EngineState[IO] =
      TestUtil.initStateWithSequence(
        seqId,
        SequenceState.init(
          obsId = seqId,
          loadedStep = EngineStep(
            id = stepId(1),
            executions = List(
              NonEmptyList.of(configureTcs, configureInst), // Execution
              NonEmptyList.one(error(errMsg)),
              NonEmptyList.one(observe)
            )
          ),
          sequenceType = SequenceType.Science,
          breakpoints = Breakpoints.empty
        )
      )

    val qs1 = runToCompletionLastIO(qs0)

    qs1.map { x =>
      x.flatMap(_.sequences.get(seqId)).map(_.seq).exists { s =>
        s.currentStep.exists { z =>
          z.toEngineStep match {
            // Check that the sequence stopped midway
            case EngineStep(_, List(ex1, ex2, ex3)) =>
              Execution(ex1.toList).results.length == 2 && Execution(
                ex2.toList
              ).results.length == 1 && Execution(ex3.toList).actions.length == 1
            case _                                  => false
          }
        } && (s.status == SequenceStatus.Failed(errMsg)) // And that it ended in error
      }
    }.assert
  }

  test(
    "engine should complete execution and propagate error when a partial Action ends in error."
  ) {
    val errMsg               = "Dummy error"
    val (ref, action)        = errorSet1(errMsg)
    val qs0: EngineState[IO] =
      TestUtil.initStateWithSequence(
        seqId,
        SequenceState.init(
          obsId = seqId,
          loadedStep = EngineStep(
            id = stepId(1),
            executions = List(
              NonEmptyList.one(action)
            )
          ),
          sequenceType = SequenceType.Science,
          breakpoints = Breakpoints.empty
        )
      )

    val qs1 = runToCompletionLastIO(qs0)

    qs1.map { x =>
      x.flatMap(_.sequences.get(seqId)).map(_.seq).exists { s =>
        s.currentStep.isEmpty && s.status === SequenceStatus.Completed
      }
    }.assert *>
      ref.get.map(_ === 1).assert

  }

  test("engine should mark a step as aborted if the action ends as aborted") {
    val qs0: EngineState[IO] =
      TestUtil.initStateWithSequence(
        seqId,
        SequenceState.init(
          obsId = seqId,
          loadedStep = EngineStep(
            id = stepId(1),
            executions = List(
              NonEmptyList.one(aborted)
            )
          ),
          sequenceType = SequenceType.Science,
          breakpoints = Breakpoints.empty
        )
      )

    val qs1 = runToCompletionLastIO(qs0)

    qs1.map { x =>
      x.flatMap(_.sequences.get(seqId)).map(_.seq).exists { s =>
        // And that it ended in aborted
        s.status === SequenceStatus.Aborted
      }
    }.assert
  }

  test("engine should stop execution and propagate error when a single partial Action fails") {
    val errMsg               = "Dummy error"
    val qs0: EngineState[IO] =
      TestUtil.initStateWithSequence(
        seqId,
        SequenceState.init(
          obsId = seqId,
          loadedStep = EngineStep(
            id = stepId(1),
            executions = List(
              NonEmptyList.one(errorSet2(errMsg))
            )
          ),
          sequenceType = SequenceType.Science,
          breakpoints = Breakpoints.empty
        )
      )

    val qs1 = runToCompletionLastIO(qs0)

    qs1.map { x =>
      x.flatMap(_.sequences.get(seqId)).map(_.seq).exists { s =>
        // Without the error we should have a value 2
        // And that it ended in error
        s.status === SequenceStatus.Failed(errMsg)
      }
    }.assert
  }

  test("engine should let fatal errors bubble") {
    val errMsg               = "Dummy error"
    val qs0: EngineState[IO] =
      TestUtil.initStateWithSequence(
        seqId,
        SequenceState.init(
          obsId = seqId,
          loadedStep = EngineStep(
            id = stepId(1),
            executions = List(
              NonEmptyList.one(fatalError(errMsg))
            )
          ),
          sequenceType = SequenceType.Science,
          breakpoints = Breakpoints.empty
        )
      )

    interceptIO[RuntimeException](runToCompletionLastIO(qs0))

  }

  test("engine should record a partial result and continue execution.") {

    // For result types
    case class RetValDouble(v: Double)     extends Result.RetVal
    case class PartialValDouble(v: Double) extends Result.PartialVal

    val qs0: EngineState[IO] =
      TestUtil.initStateWithSequence(
        seqId,
        SequenceState.init(
          obsId = seqId,
          loadedStep = EngineStep(
            id = stepId(1),
            executions = List(
              NonEmptyList.one(
                Action(
                  ActionType.Undefined,
                  Stream
                    .emits(
                      List(
                        Result.Partial(PartialValDouble(0.5)),
                        Result.OK(RetValDouble(1.0))
                      )
                    )
                    .covary[IO],
                  Action.State(Action.ActionState.Idle, Nil)
                )
              )
            )
          ),
          sequenceType = SequenceType.Science,
          breakpoints = Breakpoints.empty
        )
      )

    val qss = runToCompletionAllIO(qs0)

    qss.map { x =>
      val a1 = x.drop(2)
      val a  = a1.headOption.flatMap(y => y.sequences.get(seqId)).map(_.seq) match {
        case Some(s) =>
          s.currentStep.exists { z =>
            z.focus.execution.headOption match {
              case Some(
                    Action(_, _, Action.State(Action.ActionState.Started, v :: _), _)
                  ) =>
                v == PartialValDouble(0.5)
              case _ => false
            }
          } && s.status.isRunning
        case _       => false
      }
      val b  = x.lastOption.flatMap(_.sequences.get(seqId)).map(_.seq) match {
        case Some(s) =>
          s.currentStep.isEmpty && s.status === SequenceStatus.Completed
        case _       => false
      }
      a && b
    }.assert

  }

  test("uncurrentify should be None when not all executions are completed") {
    assert(stepz0.uncurrentify.isEmpty)
    assert(stepza0.uncurrentify.isEmpty)
    assert(stepza1.uncurrentify.isEmpty)
    assert(stepzr0.uncurrentify.isEmpty)
    assert(stepzr1.uncurrentify.nonEmpty)
    assert(stepzr2.uncurrentify.nonEmpty)
    assert(stepzar0.uncurrentify.isEmpty)
    assert(stepzar1.uncurrentify.isEmpty)
  }

  test("next should be None when there are no more pending executions") {
    assert(stepz0.withNextExecution.isEmpty)
    assert(stepza0.withNextExecution.isEmpty)
    assert(stepza1.withNextExecution.nonEmpty)
    assert(stepzr0.withNextExecution.isEmpty)
    assert(stepzr1.withNextExecution.isEmpty)
    assert(stepzr2.withNextExecution.isEmpty)
    assert(stepzar0.withNextExecution.isEmpty)
    assert(stepzar1.withNextExecution.nonEmpty)
  }

  val step0: EngineStep[IO] = EngineStep(stepId(1), Nil)
  val step1: EngineStep[IO] = EngineStep(stepId(1), List(NonEmptyList.one(action)))
  val step2: EngineStep[IO] =
    EngineStep(stepId(2), List(NonEmptyList.of(action, action), NonEmptyList.one(action)))

  test("currentify should be None only when a EngineStep is empty of executions") {
    assert(
      EngineStep.ExecutionZipper
        .currentify(EngineStep(stepId(1), Nil))
        .isEmpty
    )
    assert(EngineStep.ExecutionZipper.currentify(step0).isEmpty)
    assert(EngineStep.ExecutionZipper.currentify(step1).nonEmpty)
    assert(EngineStep.ExecutionZipper.currentify(step2).nonEmpty)
  }

  test("status should be completed when it doesn't have any executions") {
    assertEquals(stepz0.toEngineStep.status, StepState.Completed)
  }

  test("status should be Error when at least one Action failed") {
    assert(
      EngineStep
        .ExecutionZipper(
          id = stepId(1),
          pending = Nil,
          focus = Execution(List(action, actionFailed, actionCompleted)),
          done = Nil,
          rolledback = (Execution(List(action, action, action)), Nil)
        )
        .toEngineStep
        .status === StepState.Failed("Dummy error")
    )
  }

  test("status should be Completed when all actions succeeded") {
    assert(
      EngineStep
        .ExecutionZipper(
          id = stepId(1),
          pending = Nil,
          focus = Execution(List(actionCompleted, actionCompleted, actionCompleted)),
          done = Nil,
          rolledback = (Execution(List(action, action, action)), Nil)
        )
        .toEngineStep
        .status === StepState.Completed
    )
  }

  test("status should be Running when there are both actions and results") {
    assert(
      EngineStep
        .ExecutionZipper(
          id = stepId(1),
          pending = Nil,
          focus = Execution(List(actionCompleted, action, actionCompleted)),
          done = Nil,
          rolledback = (Execution(List(action, action, action)), Nil)
        )
        .toEngineStep
        .status === StepState.Running
    )
  }

  test("status should be Pending when there are only pending actions") {
    assert(
      EngineStep
        .ExecutionZipper(
          id = stepId(1),
          pending = Nil,
          focus = Execution(List(action, action, action)),
          done = Nil,
          rolledback = (Execution(List(action, action, action)), Nil)
        )
        .toEngineStep
        .status === StepState.Pending
    )
  }

}

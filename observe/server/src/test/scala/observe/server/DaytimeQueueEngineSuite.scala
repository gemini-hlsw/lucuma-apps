// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import cats.data.NonEmptyList
import cats.effect.Deferred
import cats.effect.IO
import cats.effect.Ref
import cats.effect.testkit.TestControl
import cats.syntax.all.*
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ExecutionEnvironment
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.Site
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.ConfigChangeEstimate
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepEstimate
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.refined.auto.*
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.schemas.model.BasicConfiguration
import observe.common.test.*
import observe.model.DaytimeQueue
import observe.model.DaytimeQueueItem
import observe.model.Notification
import observe.model.Observation
import observe.model.Observer
import observe.model.QueueItemId
import observe.model.SequenceStatus
import observe.model.Subsystem
import observe.model.enums.ObservatorySubsystem
import observe.model.enums.QueueDispatchState
import observe.model.enums.QueueItemStatus
import observe.model.enums.QueueOrderingMode
import observe.model.enums.RunOverride
import observe.server.engine.EngineEvent
import observe.server.engine.EventResult
import observe.server.engine.SequenceState
import observe.server.engine.user
import observe.server.gcal.GcalController
import observe.server.odb.OdbProxy
import observe.server.odb.OdbQueueObservation
import observe.server.odb.TestOdbProxy

import java.util.UUID
import scala.concurrent.duration.*

class DaytimeQueueEngineSuite extends TestCommon:
  import TestCommon.*

  private val observer: Observer = Observer("Joe".refined)

  private val DaytimeRole: CalibrationRole = CalibrationRole.DaytimeCalibration

  // Longer than the lamps countdown, which the tests go through in simulated time.
  private val StepTimeout: FiniteDuration = 5.minutes

  private val DrainTime: FiniteDuration = 300.millis

  private val obs1: Observation.Id = observationId(1)
  private val obs2: Observation.Id = observationId(2)
  private val obs3: Observation.Id = observationId(3)

  /** Runs the engine against its state, which is kept between calls. */
  private case class Runner(oe: ObserveEngine[IO], state: Ref[IO, EngineState[IO]]):
    def run(
      f:     ObserveEngine[IO] => IO[Unit],
      until: (EventResult, EngineState[IO]) => Boolean,
      tap:   (EventResult, EngineState[IO]) => IO[Unit] = (_, _) => IO.unit
    ): IO[(EventResult, EngineState[IO])] =
      for
        _  <- f(oe)
        s0 <- state.get
        r  <- oe
                .eventResultStream(s0)
                .evalTap((result, st) => tap(result, st))
                .takeThrough((result, st) => !until(result, st))
                .compile
                .lastOrError
                .timeout(StepTimeout)
        _  <- state.set(r._2)
      yield r

    def runUntilState(
      f:     ObserveEngine[IO] => IO[Unit],
      until: EngineState[IO] => Boolean,
      tap:   (EventResult, EngineState[IO]) => IO[Unit] = (_, _) => IO.unit
    ): IO[EngineState[IO]] =
      run(f, (_, st) => until(st), tap).map(_._2)

    /** Lets whatever is still pending happen. */
    def drain: IO[EngineState[IO]] =
      for
        s0 <- state.get
        r  <- oe.eventResultStream(s0).interruptAfter(DrainTime).compile.last
        s1  = r.map(_._2).getOrElse(s0)
        _  <- state.set(s1)
      yield s1

    def add(obsId: Observation.Id): IO[EngineState[IO]] =
      runUntilState(_.addToDaytimeQueue(obsId, user, clientId), _.daytimeQueue.contains(obsId))

    def notification(f: ObserveEngine[IO] => IO[Unit]): IO[Option[Notification]] =
      run(f, (r, _) => notificationOf(r).isDefined).map((r, _) => notificationOf(r))

  private case class Fixture(runner: Runner, odb: TestOdbProxy[IO])

  private def notificationOf(result: EventResult): Option[Notification] =
    result match
      case EventResult.UserCommandResponse(_, _, Some(SeqEvent.NotifyUser(n, _))) => n.some
      case _                                                                      => none

  private def isQueueUpdate(result: EventResult): Boolean =
    result match
      case EventResult.UserCommandResponse(_, _, Some(SeqEvent.DaytimeQueueUpdated)) => true
      case _                                                                         => false

  private def isExecuting(obsId: Observation.Id)(result: EventResult): Boolean =
    result match
      case EventResult.EngineUpdate(EngineEvent.Executing(id), _) => id === obsId
      case _                                                      => false

  private def isSequenceHeld(obsId: Observation.Id)(result: EventResult): Boolean =
    result match
      case EventResult.EngineUpdate(EngineEvent.SequenceHeld(id), _) => id === obsId
      case _                                                         => false

  private def statusOf(st: EngineState[IO], obsId: Observation.Id): Option[QueueItemStatus] =
    st.daytimeQueue.obs(obsId).map(_.status)

  private def allIn(
    status: QueueItemStatus,
    obsIds: Observation.Id*
  ): EngineState[IO] => Boolean =
    st => obsIds.forall(id => statusOf(st, id).contains_(status))

  private def runningCount(st: EngineState[IO]): Int =
    st.daytimeQueue.obsItems.count(_.status === QueueItemStatus.Running)

  private def atomId(n: Int): Atom.Id =
    Atom.Id.fromUuid(UUID.nameUUIDFromBytes(s"atom-$n".getBytes))

  private def testStepId(n: Int): Step.Id =
    Step.Id.fromUuid(UUID.nameUUIDFromBytes(s"step-$n".getBytes))

  private val StepConfigChange: StepEstimate =
    StepEstimate.fromMax(List(ConfigChangeEstimate("test", "test", 10.secondTimeSpan)), Nil)

  private def atom[D](n: Int, steps: Int, config: StepConfig, dynamic: D): Atom[D] =
    Atom(
      atomId(n),
      none,
      NonEmptyList.fromListUnsafe(
        List.tabulate(steps): i =>
          Step[D](
            testStepId(n * 100 + i),
            dynamic,
            config,
            telescopeCfg1,
            StepConfigChange,
            ObserveClass.DayCal,
            Breakpoint.Disabled
          )
      )
    )

  /** A GMOS North observation; science steps claim the TCS and GCAL, darks only the instrument. */
  private def gmosNorthObs(
    obsId:  Observation.Id,
    steps:  Int,
    config: StepConfig
  ): (Instrument, TestOdbProxy.InstrumentState) =
    Instrument.GmosNorth -> TestOdbProxy.InstrumentState(
      obsId,
      staticCfg1,
      TestOdbProxy.SequenceState(
        none,
        List(atom[DynamicConfig.GmosNorth](obsId.value.value.toInt, steps, config, dynamicCfg1))
      )
    )

  private def gmosSouthObs(
    obsId:  Observation.Id,
    steps:  Int,
    config: StepConfig
  ): (Instrument, TestOdbProxy.InstrumentState) =
    Instrument.GmosSouth -> TestOdbProxy.InstrumentState(
      obsId,
      staticCfg2,
      TestOdbProxy.SequenceState(
        none,
        List(atom[DynamicConfig.GmosSouth](obsId.value.value.toInt, steps, config, dynamicCfg2))
      )
    )

  /** Records every GCAL configuration, failing them all if `fail`. */
  private def recordingGcal(
    calls: Ref[IO, List[GcalController.GcalConfig]],
    fail:  Boolean
  ): GcalController[IO] =
    (config: GcalController.GcalConfig) =>
      calls.update(_ :+ config) >>
        IO.raiseError(new RuntimeException("GCAL is down")).whenA(fail)

  private def fixture(
    sequences: List[(Instrument, TestOdbProxy.InstrumentState)],
    roles:     Map[Observation.Id, CalibrationRole],
    gcal:      Option[GcalController[IO]] = none,
    initial:   EngineState[IO] = EngineState.default[IO],
    remaining: Map[Observation.Id, TestOdbProxy.RemainingStepsMode] = Map.empty
  ): IO[Fixture] =
    for
      odb      <- TestOdbProxy.buildMulti[IO](
                    sequences,
                    calibrationRoles = roles,
                    remainingStepsModes = remaining
                  )
      services <- defaultServices.map(s => s.copy(odb = odb, gcal = gcal.getOrElse(s.gcal)))
      oe       <-
        ObserveEngine.build(Site.GS, services, defaultSettings, ExecutionEnvironment.Development)
      state    <- Ref.of[IO, EngineState[IO]](initial)
    yield Fixture(Runner(oe, state), odb)

  private def daytime(obsIds: Observation.Id*): Map[Observation.Id, CalibrationRole] =
    obsIds.map(_ -> DaytimeRole).toMap

  /** Calls `f` the first time `obsId` starts executing. */
  private def onFirstExecuting(obsId: Observation.Id, runner: Runner)(
    f: ObserveEngine[IO] => IO[Unit]
  ): IO[(EventResult, EngineState[IO]) => IO[Unit]] =
    Ref
      .of[IO, Boolean](false)
      .map: done =>
        (result, _) =>
          done.getAndSet(true).flatMap(f(runner.oe).unlessA).whenA(isExecuting(obsId)(result))

  /** The largest value of a measure over the states seen by `tap`. */
  private case class Tracker(max: Ref[IO, Int], tap: (EventResult, EngineState[IO]) => IO[Unit])

  private def tracking(f: EngineState[IO] => Int): IO[Tracker] =
    Ref.of[IO, Int](0).map(max => Tracker(max, (_, st) => max.update(_.max(f(st)))))

  /** Calls `f` once every observation in `obsIds` has started executing. */
  private def onAllExecuting(obsIds: Set[Observation.Id], runner: Runner)(
    f: ObserveEngine[IO] => IO[Unit]
  ): IO[(EventResult, EngineState[IO]) => IO[Unit]] =
    Ref
      .of[IO, (Set[Observation.Id], Boolean)]((Set.empty, false))
      .map: seen =>
        (result, _) =>
          seen
            .modify: (ids, fired) =>
              val next: Set[Observation.Id] = ids ++ obsIds.filter(id => isExecuting(id)(result))
              val fire: Boolean             = !fired && obsIds.subsetOf(next)
              ((next, fired || fire), fire)
            .flatMap(fire => f(runner.oe).whenA(fire))

  /** A GCAL that records its configurations and fails them while `failing` holds. */
  private def switchableGcal(
    calls:   Ref[IO, List[GcalController.GcalConfig]],
    failing: Ref[IO, Boolean]
  ): GcalController[IO] =
    (config: GcalController.GcalConfig) =>
      calls.update(_ :+ config) >>
        failing.get.flatMap(f => IO.raiseError(new RuntimeException("GCAL is down")).whenA(f))

  private def stepEnds(events: List[TestOdbProxy.OdbEvent]): Int =
    events.count:
      case TestOdbProxy.StepEndStep(_) => true
      case _                           => false

  private def stepStarts(events: List[TestOdbProxy.OdbEvent]): Int =
    events.count:
      case TestOdbProxy.StepStartStep(_) => true
      case _                             => false

  private val darkConfig: StepConfig    = StepConfig.Dark
  private val scienceConfig: StepConfig = StepConfig.Science

  private val waiting: DaytimeQueueItem.Obs =
    DaytimeQueueItem.Obs(
      obs1,
      Instrument.GmosNorth,
      QueueItemStatus.Waiting,
      Set(Instrument.GmosNorth),
      TimeSpan.Zero,
      none,
      abortRequested = false,
      startedAt = none,
      total = TimeSpan.Zero
    )

  private val running: DaytimeQueueItem.Obs = waiting.copy(status = QueueItemStatus.Running)

  test("[DQ-23, DQ-24, DQ-29] sync maps a running queue observation to the state of its sequence"):
    def next(item: DaytimeQueueItem.Obs, s: Option[SequenceStatus]): DaytimeQueueItem.Obs =
      DaytimeQueueOps.nextItemStatus(item, s, Timestamp.Min)
    assertEquals(next(running, none).status, QueueItemStatus.Stopped)
    assertEquals(next(running, SequenceStatus.Completed.some).status, QueueItemStatus.Completed)
    assertEquals(
      next(running, SequenceStatus.Failed("boom").some),
      running.copy(status = QueueItemStatus.Failed, errorMsg = "boom".some)
    )
    assertEquals(next(running, SequenceStatus.Aborted.some).status, QueueItemStatus.Aborted)
    assertEquals(next(running, SequenceStatus.Idle.some).status, QueueItemStatus.Stopped)
    assertEquals(
      next(running.copy(abortRequested = true), SequenceStatus.Idle.some).status,
      QueueItemStatus.Aborted
    )
    assertEquals(next(running, SequenceStatus.Running.Init.some), running)

  test("[DQ-33] an observation that stops running keeps what it had left"):
    val started: Timestamp            = Timestamp.Min
    val item: DaytimeQueueItem.Obs    =
      running.copy(
        estimate = 10.minuteTimeSpan,
        total = 10.minuteTimeSpan,
        startedAt = started.some
      )
    val later: Timestamp              =
      Timestamp.fromInstantTruncatedAndBounded(started.toInstant.plusSeconds(240))
    val stopped: DaytimeQueueItem.Obs =
      DaytimeQueueOps.nextItemStatus(item, SequenceStatus.Idle.some, later)
    val done: DaytimeQueueItem.Obs    =
      DaytimeQueueOps.nextItemStatus(item, SequenceStatus.Completed.some, later)
    assertEquals(stopped.status, QueueItemStatus.Stopped)
    assertEquals(stopped.estimate, 6.minuteTimeSpan)
    assertEquals(stopped.total, 10.minuteTimeSpan)
    assertEquals(stopped.startedAt, none)
    assertEquals(done.estimate, TimeSpan.Zero)
    assertEquals(
      DaytimeQueueOps.nextItemStatus(item, SequenceStatus.Running.Init.some, later),
      item
    )

  test("[DQ-46] sync marks a queue observation started outside the queue as running"):
    def next(item: DaytimeQueueItem.Obs, s: Option[SequenceStatus]): DaytimeQueueItem.Obs =
      DaytimeQueueOps.nextItemStatus(item, s, Timestamp.Min)
    assertEquals(
      next(
        waiting.copy(status = QueueItemStatus.Failed, errorMsg = "x".some, abortRequested = true),
        SequenceStatus.Running.Init.some
      ),
      running.copy(startedAt = Timestamp.Min.some)
    )
    assertEquals(next(waiting, SequenceStatus.Idle.some), waiting)
    assertEquals(next(waiting, none), waiting)
    assertEquals(
      next(waiting.copy(status = QueueItemStatus.Stopped), SequenceStatus.Completed.some).status,
      QueueItemStatus.Stopped
    )

  test("[DQ-39] sync finishes a running queue and reports what changed"):
    val loaded: EngineState[IO]                            = loadDefaultSequence(seqObsId1).apply(EngineState.default[IO])
    val completed: EngineState[IO]                         =
      EngineState
        .sequenceStateAt[IO](seqObsId1)
        .andThen(SequenceState.status[IO])
        .replace(SequenceStatus.Completed)(loaded)
    val st: EngineState[IO]                                =
      completed.copy(
        daytimeQueue = DaytimeQueue.Empty.copy(
          items = List(running.copy(obsId = seqObsId1)),
          dispatchState = QueueDispatchState.Running
        ),
        daytimeQueueObserver = observer.some
      )
    val (synced: EngineState[IO], result: QueueSyncResult) = DaytimeQueueOps.sync(Timestamp.Min)(st)
    assertEquals(statusOf(synced, seqObsId1), QueueItemStatus.Completed.some)
    assertEquals(synced.daytimeQueue.dispatchState, QueueDispatchState.Idle)
    assertEquals(synced.daytimeQueueObserver, none)
    assertEquals(result, QueueSyncResult(true, true, false))

  private def queueObs(instrument: Instrument): OdbQueueObservation =
    OdbQueueObservation(DaytimeRole.some, BasicConfiguration.DaytimeCalibration(instrument).some)

  test("[DQ-2] an observation without observing mode or supported instrument is rejected"):
    val base: OdbQueueObservation = queueObs(Instrument.GmosNorth)
    assert(DaytimeQueueOps.accepted(base.copy(observingMode = none)).isLeft)
    assert(
      DaytimeQueueOps
        .accepted(
          base.copy(observingMode = BasicConfiguration.DaytimeCalibration(Instrument.Scorpio).some)
        )
        .isLeft
    )
    assertEquals(DaytimeQueueOps.accepted(base), Instrument.GmosNorth.asRight)

  test("restart is refused while the daytime queue is running"):
    val st: EngineState[IO] =
      EngineState
        .default[IO]
        .copy(daytimeQueue = DaytimeQueue.Empty.copy(dispatchState = QueueDispatchState.Running))
    assertEquals(
      ObserveEngine.restartBlockers(st).map(_.toList),
      List("The daytime queue is running").some
    )

  test(
    "[DQ-2, DQ-5] add rejects observations that are not daytime calibrations, duplicates and unreadable"
  ):
    TestControl.executeEmbed:
      for
        f       <- fixture(
                     List(gmosSouthObs(obs1, 1, darkConfig), gmosNorthObs(obs2, 1, darkConfig)),
                     daytime(obs1)
                   )
        notDay  <- f.runner.notification(_.addToDaytimeQueue(obs2, user, clientId))
        _       <- f.runner.add(obs1)
        dup     <- f.runner.notification(_.addToDaytimeQueue(obs1, user, clientId))
        unknown <- f.runner.notification(_.addToDaytimeQueue(obs3, user, clientId))
        st      <- f.runner.state.get
      yield
        assertEquals(
          notDay,
          Notification.DaytimeQueueRejected(obs2, "It is not a daytime calibration").some
        )
        assertEquals(
          dup,
          Notification.DaytimeQueueRejected(obs1, "It is already in the daytime queue").some
        )
        unknown match
          case Some(Notification.DaytimeQueueRejected(id, reason)) =>
            assertEquals(id, obs3)
            assert(reason.startsWith("It could not be read"), reason)
          case other                                               => fail(s"Unexpected notification $other")
        assertEquals(st.daytimeQueue.obsItems.map(_.obsId), List(obs1))

  test("[DQ-11] add fails for an observation whose remaining steps cannot be read"):
    TestControl.executeEmbed:
      for
        f      <- fixture(
                    List(gmosNorthObs(obs2, 1, darkConfig)),
                    daytime(obs2),
                    remaining = Map(obs2 -> TestOdbProxy.RemainingStepsMode.Failing)
                  )
        unread <- f.runner.notification(_.addToDaytimeQueue(obs2, user, clientId))
        st     <- f.runner.state.get
      yield
        unread match
          case Some(Notification.DaytimeQueueRejected(id, reason)) =>
            assertEquals(id, obs2)
            assert(reason.startsWith("It could not be read"), reason)
          case other                                               => fail(s"Unexpected notification $other")
        assertEquals(st.daytimeQueue.obsItems, Nil)

  test("[DQ-1, DQ-11] add appends a waiting observation claiming what its remaining steps need"):
    TestControl.executeEmbed:
      for
        f  <- fixture(
                List(gmosSouthObs(obs1, 2, darkConfig), gmosNorthObs(obs2, 1, scienceConfig)),
                daytime(obs1, obs2)
              )
        _  <- f.runner.add(obs1)
        st <- f.runner.add(obs2)
      yield
        val dark: Option[DaytimeQueueItem.Obs] = st.daytimeQueue.obs(obs1)
        assertEquals(dark.map(_.status), QueueItemStatus.Waiting.some)
        assertEquals(dark.map(_.subsystems), Set[Subsystem](Instrument.GmosSouth).some)
        assertEquals(dark.map(_.estimate), 20.secondTimeSpan.some)
        assertEquals(
          st.daytimeQueue.obs(obs2).map(_.subsystems),
          Set[Subsystem](
            Instrument.GmosNorth,
            ObservatorySubsystem.Tcs,
            ObservatorySubsystem.Gcal
          ).some
        )
        assert(st.sequences.isEmpty, "Adding to the queue must not load anything")

  test("[DQ-11] run queue starts observations with disjoint subsystems in parallel"):
    TestControl.executeEmbed:
      for
        f          <- fixture(
                        List(gmosNorthObs(obs1, 2, scienceConfig), gmosSouthObs(obs2, 2, darkConfig)),
                        daytime(obs1, obs2)
                      )
        _          <- f.runner.add(obs1)
        _          <- f.runner.add(obs2)
        tracker    <- tracking(runningCount)
        st         <- f.runner.runUntilState(
                        _.runDaytimeQueue(observer, user, clientId),
                        allIn(QueueItemStatus.Completed, obs1, obs2),
                        tracker.tap
                      )
        maxRunning <- tracker.max.get
      yield
        assertEquals(maxRunning, 2)
        assertEquals(st.daytimeQueue.dispatchState, QueueDispatchState.Idle)
        assertEquals(st.daytimeQueueObserver, none)

  test(
    "[DQ-11, DQ-15] run queue holds back an observation sharing a subsystem until the first one completes"
  ):
    TestControl.executeEmbed:
      for
        f          <- fixture(
                        List(gmosNorthObs(obs1, 2, scienceConfig), gmosSouthObs(obs2, 1, scienceConfig)),
                        daytime(obs1, obs2)
                      )
        _          <- f.runner.add(obs1)
        _          <- f.runner.add(obs2)
        tracker    <- tracking(runningCount)
        st         <- f.runner.runUntilState(
                        _.runDaytimeQueue(observer, user, clientId),
                        allIn(QueueItemStatus.Completed, obs1, obs2),
                        tracker.tap
                      )
        maxRunning <- tracker.max.get
      yield assertEquals(maxRunning, 1)

  private def orderingRun(mode: QueueOrderingMode): IO[Int] =
    for
      f       <- fixture(
                   List(
                     gmosNorthObs(obs1, 2, scienceConfig),
                     gmosNorthObs(obs2, 1, darkConfig),
                     gmosSouthObs(obs3, 1, darkConfig)
                   ),
                   daytime(obs1, obs2, obs3)
                 )
      _       <- f.runner.add(obs1)
      _       <- f.runner.add(obs2)
      _       <- f.runner.add(obs3)
      _       <- f.runner.run(
                   _.setDaytimeQueueOrderingMode(mode, user, clientId),
                   (r, _) => isQueueUpdate(r)
                 )
      tracker <- tracking: st =>
                   if allIn(QueueItemStatus.Running, obs1, obs3)(st) then 1 else 0
      _       <- f.runner.runUntilState(
                   _.runDaytimeQueue(observer, user, clientId),
                   allIn(QueueItemStatus.Completed, obs1, obs2, obs3),
                   tracker.tap
                 )
      overlap <- tracker.max.get
    yield overlap

  test("[DQ-13] jump ahead starts an observation below a blocked one"):
    TestControl.executeEmbed:
      assertIO(orderingRun(QueueOrderingMode.JumpAhead), 1)

  test("[DQ-13] strict ordering starts nothing below a blocked observation"):
    TestControl.executeEmbed:
      assertIO(orderingRun(QueueOrderingMode.Strict), 0)

  test("[DQ-17, DQ-18] nothing below a pause starts until continue"):
    TestControl.executeEmbed:
      for
        f       <- fixture(List(gmosSouthObs(obs1, 1, darkConfig)), daytime(obs1))
        _       <- f.runner.add(obs1)
        _       <- f.runner.run(_.addDaytimeQueuePause(0, user, clientId), (r, _) => isQueueUpdate(r))
        _       <- f.runner.run(
                     _.runDaytimeQueue(observer, user, clientId),
                     (r, _) => isQueueUpdate(r)
                   )
        paused  <- f.runner.drain
        resumed <- f.runner.runUntilState(
                     _.continueDaytimeQueuePastPause(user, clientId),
                     allIn(QueueItemStatus.Completed, obs1)
                   )
      yield
        assertEquals(statusOf(paused, obs1), QueueItemStatus.Waiting.some)
        assertEquals(paused.daytimeQueue.dispatchState, QueueDispatchState.Running)
        assert(paused.sequences.isEmpty)
        assertEquals(resumed.daytimeQueue.items.size, 1)

  test("[DQ-29, DQ-45] a failed observation becomes failed and the queue goes on"):
    TestControl.executeEmbed:
      for
        calls <- Ref.of[IO, List[GcalController.GcalConfig]](Nil)
        f     <- fixture(
                   List(gmosNorthObs(obs1, 1, scienceConfig), gmosNorthObs(obs2, 1, darkConfig)),
                   daytime(obs1, obs2),
                   recordingGcal(calls, fail = true).some
                 )
        _     <- f.runner.add(obs1)
        _     <- f.runner.add(obs2)
        st    <- f.runner.runUntilState(
                   _.runDaytimeQueue(observer, user, clientId),
                   st =>
                     allIn(QueueItemStatus.Completed, obs2)(st) &&
                       st.daytimeQueue.lampsOffError.isDefined
                 )
      yield
        assertEquals(statusOf(st, obs1), QueueItemStatus.Failed.some)
        assert(st.daytimeQueue.obs(obs1).flatMap(_.errorMsg).isDefined)
        assertEquals(st.daytimeQueue.dispatchState, QueueDispatchState.Idle)

  test("[DQ-29] an observation that cannot be read fails and the queue goes on"):
    TestControl.executeEmbed:
      val unreadable: DaytimeQueueItem.Obs =
        waiting.copy(obsId = obs3, subsystems = Set(Instrument.GmosNorth))
      for
        f  <- fixture(
                List(gmosSouthObs(obs1, 1, darkConfig)),
                daytime(obs1),
                initial = EngineState
                  .default[IO]
                  .copy(daytimeQueue = DaytimeQueue.Empty.copy(items = List(unreadable)))
              )
        _  <- f.runner.add(obs1)
        st <- f.runner.runUntilState(
                _.runDaytimeQueue(observer, user, clientId),
                allIn(QueueItemStatus.Completed, obs1)
              )
      yield assertEquals(statusOf(st, obs3), QueueItemStatus.Failed.some)

  test("[DQ-16, DQ-23] a held observation is stopped and not restarted until run queue"):
    TestControl.executeEmbed:
      for
        f       <- fixture(List(gmosSouthObs(obs1, 5, darkConfig)), daytime(obs1))
        _       <- f.runner.add(obs1)
        tap     <- onFirstExecuting(obs1, f.runner)(_.requestSequenceHold(obs1, observer, user))
        _       <- f.runner.run(
                     _.runDaytimeQueue(observer, user, clientId),
                     (r, _) => isSequenceHeld(obs1)(r),
                     tap
                   )
        held    <- f.runner.drain
        resumed <- f.runner.runUntilState(
                     _.runDaytimeQueue(observer, user, clientId),
                     allIn(QueueItemStatus.Completed, obs1)
                   )
      yield
        assertEquals(statusOf(held, obs1), QueueItemStatus.Stopped.some)
        assertEquals(held.daytimeQueue.dispatchState, QueueDispatchState.Running)
        assertEquals(held.sequences.get(obs1).map(_.seq.status), SequenceStatus.Idle.some)
        assertEquals(resumed.daytimeQueue.dispatchState, QueueDispatchState.Idle)

  test("[DQ-25, DQ-28] hold after the current steps halts the queue"):
    TestControl.executeEmbed:
      for
        f    <- fixture(
                  List(gmosSouthObs(obs1, 5, darkConfig), gmosSouthObs(obs2, 1, darkConfig)),
                  daytime(obs1, obs2)
                )
        _    <- f.runner.add(obs1)
        _    <- f.runner.add(obs2)
        tap  <- onFirstExecuting(obs1, f.runner)(_.holdDaytimeQueueAfterCurrentSteps(user, clientId))
        _    <- f.runner.run(
                  _.runDaytimeQueue(observer, user, clientId),
                  (r, _) => isSequenceHeld(obs1)(r),
                  tap
                )
        held <- f.runner.drain
      yield
        assertEquals(statusOf(held, obs1), QueueItemStatus.Stopped.some)
        assertEquals(statusOf(held, obs2), QueueItemStatus.Waiting.some)
        assertEquals(held.daytimeQueue.dispatchState, QueueDispatchState.Halted)

  test("[DQ-22] running one queue observation is rejected while its subsystems are busy"):
    TestControl.executeEmbed:
      for
        f   <- fixture(
                 List(gmosNorthObs(obs1, 5, scienceConfig), gmosSouthObs(obs2, 1, scienceConfig)),
                 daytime(obs1, obs2)
               )
        _   <- f.runner.add(obs1)
        _   <- f.runner.add(obs2)
        tap <- onFirstExecuting(obs1, f.runner)(
                 _.runDaytimeQueueItem(obs2, observer, user, clientId)
               )
        r   <- f.runner.run(
                 _.runDaytimeQueue(observer, user, clientId),
                 (r, _) => notificationOf(r).isDefined,
                 tap
               )
      yield notificationOf(r._1) match
        case Some(Notification.SubsystemConflict(id, _, _)) => assertEquals(id, obs2)
        case other                                          => fail(s"Expected a conflict: $other")

  test("[DQ-21] running one queue observation starts only that one"):
    TestControl.executeEmbed:
      for
        f  <- fixture(
                List(gmosSouthObs(obs1, 1, darkConfig), gmosNorthObs(obs2, 1, darkConfig)),
                daytime(obs1, obs2)
              )
        _  <- f.runner.add(obs1)
        _  <- f.runner.add(obs2)
        st <- f.runner.runUntilState(
                _.runDaytimeQueueItem(obs2, observer, user, clientId),
                allIn(QueueItemStatus.Completed, obs2)
              )
      yield
        assertEquals(statusOf(st, obs1), QueueItemStatus.Waiting.some)
        assertEquals(st.daytimeQueue.dispatchState, QueueDispatchState.Idle)

  /** The queue is finished and neither counting down nor turning the lamps off. */
  private def lampsSettled(st: EngineState[IO]): Boolean =
    st.daytimeQueue.isFinished && st.daytimeQueue.lampsOffAt.isEmpty && !st.daytimeQueueLampsOff

  test("[DQ-39, DQ-40, DQ-41] the GCAL lamps are turned off a countdown after the queue finishes"):
    TestControl.executeEmbed:
      for
        calls   <- Ref.of[IO, List[GcalController.GcalConfig]](Nil)
        f       <- fixture(
                     List(gmosSouthObs(obs1, 1, darkConfig)),
                     daytime(obs1),
                     recordingGcal(calls, fail = false).some
                   )
        _       <- f.runner.add(obs1)
        early   <- Ref.of[IO, Option[List[GcalController.GcalConfig]]](none)
        started <- IO.realTimeInstant
        st      <- f.runner.runUntilState(
                     _.runDaytimeQueue(observer, user, clientId),
                     lampsSettled,
                     (_, st) =>
                       calls.get
                         .flatMap(sent => early.set(sent.some))
                         .whenA(st.daytimeQueue.lampsOffAt.isDefined)
                   )
        ended   <- IO.realTimeInstant
        during  <- early.get
        sent    <- calls.get
      yield
        assertEquals(during, List.empty[GcalController.GcalConfig].some)
        assertEquals(sent, List(GcalController.GcalConfig.GcalOffIgnoringIr))
        assert(java.time.Duration.between(started, ended).getSeconds >= 60)
        assertEquals(st.daytimeQueue.lampsOffError, none)

  test("[DQ-40] a cancelled countdown leaves the GCAL lamps as they are"):
    TestControl.executeEmbed:
      for
        calls <- Ref.of[IO, List[GcalController.GcalConfig]](Nil)
        f     <- fixture(
                   List(gmosSouthObs(obs1, 1, darkConfig)),
                   daytime(obs1),
                   recordingGcal(calls, fail = false).some
                 )
        _     <- f.runner.add(obs1)
        asked <- Ref.of[IO, Boolean](false)
        // The run goes on past the time the countdown would have ended, then a command ends it.
        later  = IO.sleep(90.seconds) >>
                   f.runner.oe.setDaytimeQueueOrderingMode(QueueOrderingMode.Strict, user, clientId)
        st    <- f.runner.runUntilState(
                   _.runDaytimeQueue(observer, user, clientId),
                   _.daytimeQueue.orderingMode === QueueOrderingMode.Strict,
                   (_, st) =>
                     asked
                       .getAndSet(true)
                       .flatMap: done =>
                         (f.runner.oe.cancelDaytimeQueueLampsOff(user, clientId) >> later.start.void)
                           .unlessA(done)
                       .whenA(st.daytimeQueue.lampsOffAt.isDefined)
                 )
        sent  <- calls.get
      yield
        assertEquals(sent, Nil)
        assertEquals(st.daytimeQueue.lampsOffAt, none)

  test("[DQ-39] GCAL is busy while the queue turns its lamps off"):
    TestControl.executeEmbed:
      for
        gate <- Deferred[IO, Unit]
        gcal  = ((_: GcalController.GcalConfig) => gate.get): GcalController[IO]
        seen <- Ref.of[IO, Option[Set[Subsystem]]](none)
        f    <- fixture(List(gmosSouthObs(obs1, 1, darkConfig)), daytime(obs1), gcal.some)
        _    <- f.runner.add(obs1)
        // The command is let go once GCAL has been seen busy.
        free <- f.runner.runUntilState(
                  _.runDaytimeQueue(observer, user, clientId),
                  lampsSettled,
                  (_, st) =>
                    (seen.set(ObserveEngine.busySubsystems(st).some) >> gate.complete(()).void)
                      .whenA(st.daytimeQueueLampsOff)
                )
        busy <- seen.get
      yield
        assertEquals(busy.map(_.contains(ObservatorySubsystem.Gcal)), true.some)
        assert(!ObserveEngine.busySubsystems(free).contains(ObservatorySubsystem.Gcal))

  test("[DQ-24, DQ-42] the GCAL lamps are left alone when the queue finishes by an abort"):
    TestControl.executeEmbed:
      for
        calls <- Ref.of[IO, List[GcalController.GcalConfig]](Nil)
        f     <- fixture(
                   List(gmosSouthObs(obs1, 5, darkConfig)),
                   daytime(obs1),
                   recordingGcal(calls, fail = false).some
                 )
        _     <- f.runner.add(obs1)
        tap   <- onFirstExecuting(obs1, f.runner)(
                   _.abortDaytimeQueueItem(obs1, observer, user, clientId)
                 )
        _     <- f.runner.runUntilState(
                   _.runDaytimeQueue(observer, user, clientId),
                   allIn(QueueItemStatus.Aborted, obs1),
                   tap
                 )
        st    <- f.runner.drain
        sent  <- calls.get
      yield
        assertEquals(sent, Nil)
        assertEquals(st.daytimeQueue.lampsOffAt, none)
        assertEquals(st.daytimeQueue.dispatchState, QueueDispatchState.Idle)

  test("[DQ-26, DQ-43] the lamps countdown starts when a halted queue ends up all finished"):
    TestControl.executeEmbed:
      for
        calls <- Ref.of[IO, List[GcalController.GcalConfig]](Nil)
        f     <- fixture(
                   List(gmosSouthObs(obs1, 3, darkConfig)),
                   daytime(obs1),
                   recordingGcal(calls, fail = false).some
                 )
        _     <- f.runner.add(obs1)
        tap   <- onFirstExecuting(obs1, f.runner)(
                   _.holdDaytimeQueueAfterCurrentObservations(user, clientId)
                 )
        _     <- f.runner.runUntilState(
                   _.runDaytimeQueue(observer, user, clientId),
                   allIn(QueueItemStatus.Completed, obs1),
                   tap
                 )
        st    <- f.runner.drain
        sent  <- calls.get
      yield
        assertEquals(sent, Nil)
        assert(st.daytimeQueue.lampsOffAt.isDefined)
        assertEquals(st.daytimeQueue.dispatchState, QueueDispatchState.Halted)

  private val AllSubsystems: Set[Subsystem] =
    Set(Instrument.GmosNorth, ObservatorySubsystem.Tcs, ObservatorySubsystem.Gcal)

  /** Loads and starts a science observation outside the queue, giving the answer to the start. */
  private def manualStart(queue: DaytimeQueue): IO[Option[SeqEvent]] =
    for
      f <- fixture(
             List(gmosSouthObs(obs2, 1, scienceConfig)),
             Map.empty,
             initial = EngineState.default[IO].copy(daytimeQueue = queue)
           )
      _ <- f.runner.run(
             _.loadSequence(Instrument.GmosSouth, obs2, observer, user, clientId),
             (r, _) =>
               r match
                 case EventResult.UserCommandResponse(_, _, Some(SeqEvent.LoadSequence(_, _))) =>
                   true
                 case _                                                                        => false
           )
      r <- f.runner.run(
             _.startSequence(obs2, user, observer, clientId, RunOverride.Override),
             (r, _) =>
               r match
                 case EventResult.UserCommandResponse(_, _, Some(SeqEvent.Busy(_, _, _, _)))    =>
                   true
                 case EventResult.UserCommandResponse(_, _, Some(SeqEvent.NotifyUser(_, _)))    =>
                   true
                 case EventResult.UserCommandResponse(_, _, Some(SeqEvent.SequenceStart(_, _))) =>
                   true
                 case _                                                                         => false
           )
    yield r._1 match
      case EventResult.UserCommandResponse(_, _, event) => event
      case _                                            => none

  private def isBusy(event: Option[SeqEvent]): Boolean =
    event match
      case Some(SeqEvent.Busy(_, _, _, _)) => true
      case _                               => false

  private def isStart(event: Option[SeqEvent]): Boolean =
    event match
      case Some(SeqEvent.SequenceStart(_, _)) => true
      case _                                  => false

  test("[DQ-11] a running queue observation blocks a manual start sharing its subsystems"):
    TestControl.executeEmbed:
      val item: DaytimeQueueItem.Obs = running.copy(obsId = obs3, subsystems = AllSubsystems)
      assertIOBoolean(manualStart(DaytimeQueue.Empty.copy(items = List(item))).map(isBusy))

  test("a waiting observation of a running queue does not keep a manual start from its subsystems"):
    TestControl.executeEmbed:
      val item: DaytimeQueueItem.Obs = waiting.copy(obsId = obs3, subsystems = AllSubsystems)
      val queue: DaytimeQueue        =
        DaytimeQueue.Empty.copy(items = List(item), dispatchState = QueueDispatchState.Running)
      manualStart(queue).map(started => assert(isStart(started), s"Expected a start: $started"))

  private def runningWith(
    obsId:      Observation.Id,
    instrument: Instrument,
    subsystems: Set[Subsystem]
  ): cats.Endo[EngineState[IO]] =
    loadSequenceWithSubsystems(obsId, subsystems, EngineState.instrumentLoaded(instrument))
      .andThen:
        EngineState
          .sequenceStateAt[IO](obsId)
          .andThen(SequenceState.status[IO])
          .replace(SequenceStatus.Running.Init)

  private def loadManually(f: Fixture, instrument: Instrument, obsId: Observation.Id): IO[Unit] =
    f.runner
      .run(
        _.loadSequence(instrument, obsId, observer, user, clientId),
        (r, _) =>
          r match
            case EventResult.UserCommandResponse(_, _, Some(SeqEvent.LoadSequence(_, _))) => true
            case _                                                                        => false
      )
      .void

  test("[DQ-6, DQ-8] remove and move only change the queue order"):
    TestControl.executeEmbed:
      for
        f  <- fixture(
                List(gmosSouthObs(obs1, 1, darkConfig), gmosNorthObs(obs2, 1, darkConfig)),
                daytime(obs1, obs2)
              )
        _  <- f.runner.add(obs1)
        _  <- f.runner.add(obs2)
        m  <- f.runner.run(
                _.moveInDaytimeQueue(QueueItemId.Obs(obs2), 0, user, clientId),
                (r, _) => isQueueUpdate(r)
              )
        rm <- f.runner.run(
                _.removeFromDaytimeQueue(QueueItemId.Obs(obs1), user, clientId),
                (r, _) => isQueueUpdate(r)
              )
      yield
        assertEquals(m._2.daytimeQueue.obsItems.map(_.obsId), List(obs2, obs1))
        assertEquals(rm._2.daytimeQueue.obsItems.map(_.obsId), List(obs2))

  test("[DQ-23] a paused observation keeps its instrument until it is run again"):
    TestControl.executeEmbed:
      for
        f       <- fixture(
                     List(gmosSouthObs(obs1, 5, darkConfig), gmosSouthObs(obs2, 1, darkConfig)),
                     daytime(obs1, obs2)
                   )
        _       <- f.runner.add(obs1)
        _       <- f.runner.add(obs2)
        tap     <- onFirstExecuting(obs1, f.runner)(_.requestSequenceHold(obs1, observer, user))
        _       <- f.runner.run(
                     _.runDaytimeQueue(observer, user, clientId),
                     (r, _) => isSequenceHeld(obs1)(r),
                     tap
                   )
        paused  <- f.runner.drain
        refused <- f.runner.notification(_.runDaytimeQueueItem(obs2, observer, user, clientId))
        done    <- f.runner.runUntilState(
                     _.runDaytimeQueueItem(obs1, observer, user, clientId),
                     allIn(QueueItemStatus.Completed, obs1, obs2)
                   )
      yield
        // The dispatcher is still running, and leaves the instrument of the paused one alone.
        assertEquals(paused.daytimeQueue.dispatchState, QueueDispatchState.Running)
        assertEquals(statusOf(paused, obs1), QueueItemStatus.Stopped.some)
        assertEquals(statusOf(paused, obs2), QueueItemStatus.Waiting.some)
        assertEquals(paused.selected.gmosSouth.map(_.obsId), obs1.some)
        assertEquals(refused, Notification.InstrumentInUse(obs2, Instrument.GmosSouth).some)
        assertEquals(done.daytimeQueue.dispatchState, QueueDispatchState.Idle)

  test("[DQ-19] a partly executed observation continues from its first incomplete step"):
    TestControl.executeEmbed:
      for
        f      <- fixture(List(gmosSouthObs(obs1, 5, darkConfig)), daytime(obs1))
        _      <- f.runner.add(obs1)
        tap    <- onFirstExecuting(obs1, f.runner)(_.requestSequenceHold(obs1, observer, user))
        _      <- f.runner.run(
                    _.runDaytimeQueue(observer, user, clientId),
                    (r, _) => isSequenceHeld(obs1)(r),
                    tap
                  )
        _      <- f.runner.drain
        before <- f.odb.outCapture
        _      <- f.runner.runUntilState(
                    _.runDaytimeQueue(observer, user, clientId),
                    allIn(QueueItemStatus.Completed, obs1)
                  )
        after  <- f.odb.outCapture
      yield
        val endedBeforeResume: Int = stepEnds(before)
        assert(endedBeforeResume >= 1 && endedBeforeResume < 5,
               s"Held after $endedBeforeResume steps"
        )
        assertEquals(stepEnds(after), 5)
        assertEquals(stepStarts(after), 5)

  test("[DQ-27, DQ-28, DQ-42] abort all aborts every running observation and halts the queue"):
    TestControl.executeEmbed:
      for
        calls <- Ref.of[IO, List[GcalController.GcalConfig]](Nil)
        f     <- fixture(
                   List(
                     gmosNorthObs(obs1, 5, darkConfig),
                     gmosSouthObs(obs2, 5, darkConfig),
                     gmosNorthObs(obs3, 1, darkConfig)
                   ),
                   daytime(obs1, obs2, obs3),
                   recordingGcal(calls, fail = false).some
                 )
        _     <- f.runner.add(obs1)
        _     <- f.runner.add(obs2)
        _     <- f.runner.add(obs3)
        tap   <- onAllExecuting(Set(obs1, obs2), f.runner)(
                   _.abortDaytimeQueue(observer, user, clientId)
                 )
        _     <- f.runner.runUntilState(
                   _.runDaytimeQueue(observer, user, clientId),
                   allIn(QueueItemStatus.Aborted, obs1, obs2),
                   tap
                 )
        st    <- f.runner.drain
        sent  <- calls.get
      yield
        assertEquals(statusOf(st, obs1), QueueItemStatus.Aborted.some)
        assertEquals(statusOf(st, obs2), QueueItemStatus.Aborted.some)
        assertEquals(statusOf(st, obs3), QueueItemStatus.Waiting.some)
        assertEquals(st.daytimeQueue.dispatchState, QueueDispatchState.Halted)
        assert(!st.sequences.contains(obs3), "The third observation must not be loaded")
        assertEquals(sent, Nil)

  test(
    "[DQ-39, DQ-42] the countdown starts when the queue finishes by a completion or failure, not an abort"
  ):
    assert(DaytimeQueueOps.shouldTurnLampsOff(QueueSyncResult(true, true, false)))
    assert(!DaytimeQueueOps.shouldTurnLampsOff(QueueSyncResult(true, true, true)))
    assert(!DaytimeQueueOps.shouldTurnLampsOff(QueueSyncResult(true, false, false)))
    assert(!DaytimeQueueOps.shouldTurnLampsOff(QueueSyncResult(false, true, false)))

  test("[DQ-43, DQ-44] the countdown is dropped when the lamps are needed again"):
    val northSlot: monocle.Lens[EngineState[IO], Option[SequenceData[IO]]] =
      EngineState.instrumentLoaded(Instrument.GmosNorth)
    val at: Timestamp                                                      = Timestamp.Min
    val counting: EngineState[IO]                                          =
      EngineState
        .daytimeQueue[IO]
        .andThen(DaytimeQueue.lampsOffAt)
        .replace(at.some)(
          EngineState.default[IO]
        )
    val loaded: EngineState[IO]                                            =
      loadSequenceWithSubsystems(
        seqObsId1,
        Set[Subsystem](Instrument.GmosNorth, ObservatorySubsystem.Gcal),
        northSlot
      ).apply(counting)
    val usingGcal: EngineState[IO]                                         =
      EngineState
        .sequenceStateAt[IO](seqObsId1)
        .andThen(SequenceState.status[IO])
        .replace(SequenceStatus.Running.Init)(loaded)
    val moreToDo: EngineState[IO]                                          =
      EngineState.daytimeQueue[IO].modify(_.append(waiting))(counting)
    assert(!DaytimeQueueOps.gcalNeeded(loaded))
    assert(DaytimeQueueOps.gcalNeeded(usingGcal))
    assertEquals(DaytimeQueueOps.sync(Timestamp.Min)(counting)._1.daytimeQueue.lampsOffAt, at.some)
    assertEquals(DaytimeQueueOps.sync(Timestamp.Min)(loaded)._1.daytimeQueue.lampsOffAt, at.some)
    assertEquals(DaytimeQueueOps.sync(Timestamp.Min)(usingGcal)._1.daytimeQueue.lampsOffAt, none)
    assertEquals(DaytimeQueueOps.sync(Timestamp.Min)(moreToDo)._1.daytimeQueue.lampsOffAt, none)

  test("[DQ-49] queue commands issued back to back are applied in the order received"):
    TestControl.executeEmbed:
      for
        f  <- fixture(
                List(
                  gmosSouthObs(obs1, 1, darkConfig),
                  gmosNorthObs(obs2, 1, darkConfig),
                  gmosSouthObs(obs3, 1, darkConfig)
                ),
                daytime(obs1, obs2, obs3)
              )
        oe  = f.runner.oe
        _  <- oe.addToDaytimeQueue(obs1, user, clientId)
        _  <- oe.addToDaytimeQueue(obs2, user, clientId)
        _  <- oe.addToDaytimeQueue(obs3, user, clientId)
        _  <- oe.moveInDaytimeQueue(QueueItemId.Obs(obs3), 0, user, clientId)
        _  <- oe.removeFromDaytimeQueue(QueueItemId.Obs(obs2), user, clientId)
        st <- f.runner.drain
      yield assertEquals(st.daytimeQueue.obsItems.map(_.obsId), List(obs3, obs1))

  test("[DQ-15] switching to jump ahead while the queue runs starts a lower runnable observation"):
    TestControl.executeEmbed:
      for
        f      <- fixture(
                    List(
                      gmosNorthObs(obs1, 5, scienceConfig),
                      gmosNorthObs(obs2, 1, darkConfig),
                      gmosSouthObs(obs3, 3, darkConfig)
                    ),
                    daytime(obs1, obs2, obs3)
                  )
        _      <- f.runner.add(obs1)
        _      <- f.runner.add(obs2)
        _      <- f.runner.add(obs3)
        _      <- f.runner.run(
                    _.setDaytimeQueueOrderingMode(QueueOrderingMode.Strict, user, clientId),
                    (r, _) => isQueueUpdate(r)
                  )
        seen   <- Ref.of[IO, Option[Option[QueueItemStatus]]](none)
        tap     = (result: EventResult, st: EngineState[IO]) =>
                    seen.get
                      .map(_.isEmpty && isExecuting(obs1)(result))
                      .ifM(
                        seen.set(statusOf(st, obs3).some) >>
                          f.runner.oe
                            .setDaytimeQueueOrderingMode(QueueOrderingMode.JumpAhead, user, clientId),
                        IO.unit
                      )
        st     <- f.runner.runUntilState(
                    _.runDaytimeQueue(observer, user, clientId),
                    s => !statusOf(s, obs3).contains_(QueueItemStatus.Waiting),
                    tap
                  )
        before <- seen.get
      yield
        assertEquals(before, Some(QueueItemStatus.Waiting.some))
        assertEquals(statusOf(st, obs2), QueueItemStatus.Waiting.some)

  test("[DQ-15] an observation added while the queue runs is dispatched without another run"):
    TestControl.executeEmbed:
      for
        f   <- fixture(
                 List(gmosNorthObs(obs1, 5, scienceConfig), gmosSouthObs(obs3, 1, darkConfig)),
                 daytime(obs1, obs3)
               )
        _   <- f.runner.add(obs1)
        tap <- onFirstExecuting(obs1, f.runner)(_.addToDaytimeQueue(obs3, user, clientId))
        st  <- f.runner.runUntilState(
                 _.runDaytimeQueue(observer, user, clientId),
                 allIn(QueueItemStatus.Completed, obs3),
                 tap
               )
      yield assertEquals(statusOf(st, obs3), QueueItemStatus.Completed.some)

  test("[DQ-15] removing the blocked top item while the queue runs starts the next one"):
    TestControl.executeEmbed:
      for
        f   <- fixture(
                 List(
                   gmosNorthObs(obs1, 5, scienceConfig),
                   gmosNorthObs(obs2, 1, darkConfig),
                   gmosSouthObs(obs3, 1, darkConfig)
                 ),
                 daytime(obs1, obs2, obs3)
               )
        _   <- f.runner.add(obs1)
        _   <- f.runner.add(obs2)
        _   <- f.runner.add(obs3)
        _   <- f.runner.run(
                 _.setDaytimeQueueOrderingMode(QueueOrderingMode.Strict, user, clientId),
                 (r, _) => isQueueUpdate(r)
               )
        tap <- onFirstExecuting(obs1, f.runner)(
                 _.removeFromDaytimeQueue(QueueItemId.Obs(obs2), user, clientId)
               )
        st  <- f.runner.runUntilState(
                 _.runDaytimeQueue(observer, user, clientId),
                 allIn(QueueItemStatus.Completed, obs3),
                 tap
               )
      yield assertEquals(st.daytimeQueue.obsItems.map(_.obsId), List(obs1, obs3))

  test(
    "[DQ-26] hold after the current observations leaves a waiting observation sharing a subsystem"
  ):
    TestControl.executeEmbed:
      for
        f    <- fixture(
                  List(gmosSouthObs(obs1, 3, darkConfig), gmosSouthObs(obs2, 1, darkConfig)),
                  daytime(obs1, obs2)
                )
        _    <- f.runner.add(obs1)
        _    <- f.runner.add(obs2)
        tap  <- onFirstExecuting(obs1, f.runner)(
                  _.holdDaytimeQueueAfterCurrentObservations(user, clientId)
                )
        _    <- f.runner.runUntilState(
                  _.runDaytimeQueue(observer, user, clientId),
                  allIn(QueueItemStatus.Completed, obs1),
                  tap
                )
        held <- f.runner.drain
      yield
        assertEquals(statusOf(held, obs2), QueueItemStatus.Waiting.some)
        assertEquals(held.daytimeQueue.dispatchState, QueueDispatchState.Halted)
        assert(!held.sequences.get(obs2).exists(_.seq.status.isRunning))

  test("[DQ-30] a retried failed observation is dispatched again by the running queue"):
    TestControl.executeEmbed:
      for
        calls   <- Ref.of[IO, List[GcalController.GcalConfig]](Nil)
        failing <- Ref.of[IO, Boolean](true)
        f       <- fixture(
                     List(gmosNorthObs(obs1, 1, scienceConfig), gmosSouthObs(obs2, 8, darkConfig)),
                     daytime(obs1, obs2),
                     switchableGcal(calls, failing).some
                   )
        _       <- f.runner.add(obs1)
        _       <- f.runner.add(obs2)
        // Ending the run cancels what is in flight, so the failed step's other actions end first.
        failed  <- f.runner.runUntilState(
                     _.runDaytimeQueue(observer, user, clientId),
                     st =>
                       allIn(QueueItemStatus.Failed, obs1)(st) &&
                         !st.sequences.get(obs1).exists(ObserveEngine.hasActionsInFlight)
                   )
        _       <- failing.set(false)
        done    <- f.runner.runUntilState(
                     _.retryDaytimeQueueItem(obs1, user, clientId),
                     allIn(QueueItemStatus.Completed, obs1)
                   )
      yield
        assertEquals(failed.daytimeQueue.dispatchState, QueueDispatchState.Running)
        assertEquals(statusOf(done, obs1), QueueItemStatus.Completed.some)

  test("[DQ-21] running one observation below a pause starts only that one"):
    TestControl.executeEmbed:
      for
        f  <- fixture(
                List(gmosSouthObs(obs1, 1, darkConfig), gmosNorthObs(obs2, 1, darkConfig)),
                daytime(obs1, obs2)
              )
        _  <- f.runner.add(obs1)
        _  <- f.runner.add(obs2)
        _  <- f.runner.run(_.addDaytimeQueuePause(1, user, clientId), (r, _) => isQueueUpdate(r))
        _  <- f.runner.runUntilState(
                _.runDaytimeQueueItem(obs2, observer, user, clientId),
                allIn(QueueItemStatus.Completed, obs2)
              )
        st <- f.runner.drain
      yield
        assertEquals(statusOf(st, obs1), QueueItemStatus.Waiting.some)
        assertEquals(st.daytimeQueue.dispatchState, QueueDispatchState.Idle)
        assert(!st.sequences.contains(obs1))
        assertEquals(st.daytimeQueue.items.size, 3)

  test("[DQ-19] starting a queue observation reads only its next atom"):
    TestControl.executeEmbed:
      for
        f     <- fixture(List(gmosSouthObs(obs1, 1, darkConfig)), daytime(obs1))
        _     <- f.runner.add(obs1)
        _     <- f.runner.runUntilState(
                   _.runDaytimeQueue(observer, user, clientId),
                   allIn(QueueItemStatus.Completed, obs1)
                 )
        reads <- f.odb.readLimits
      yield assertEquals(reads.find(_._1 === obs1).map(_._2), OdbProxy.NextAtomOnly.some)

  test("[DQ-11] the queue leaves alone an instrument holding an observation loaded outside it"):
    TestControl.executeEmbed:
      for
        f       <- fixture(
                     List(gmosNorthObs(obs1, 1, darkConfig), gmosNorthObs(obs2, 1, darkConfig)),
                     daytime(obs1)
                   )
        _       <- loadManually(f, Instrument.GmosNorth, obs2)
        _       <- f.runner.add(obs1)
        _       <- f.runner.run(
                     _.runDaytimeQueue(observer, user, clientId),
                     (r, _) => isQueueUpdate(r)
                   )
        blocked <- f.runner.drain
        freed   <- f.runner.runUntilState(
                     _.unloadSequence(obs2, user, clientId),
                     allIn(QueueItemStatus.Completed, obs1)
                   )
      yield
        assertEquals(statusOf(blocked, obs1), QueueItemStatus.Waiting.some)
        assertEquals(blocked.selected.gmosNorth.map(_.obsId), obs2.some)
        assertEquals(blocked.daytimeQueue.dispatchState, QueueDispatchState.Running)
        assert(!freed.sequences.contains(obs2))

  /** obs1 is refused at start, when its claim grows to the busy TCS; true if obs2 started. */
  private def startsBelowRefused(mode: QueueOrderingMode): IO[Boolean] =
    val outside: Observation.Id     = observationId(9)
    val stale: DaytimeQueueItem.Obs = waiting.copy(obsId = obs1)
    val initial: EngineState[IO]    =
      runningWith(
        outside,
        Instrument.Flamingos2,
        Set[Subsystem](Instrument.Flamingos2, ObservatorySubsystem.Tcs)
      )
        .apply(EngineState.default[IO])
        .copy(daytimeQueue = DaytimeQueue.Empty.copy(items = List(stale), orderingMode = mode))
    for
      f     <- fixture(
                 List(gmosNorthObs(obs1, 1, scienceConfig), gmosSouthObs(obs2, 1, darkConfig)),
                 daytime(obs1, obs2),
                 initial = initial
               )
      _     <- f.runner.add(obs2)
      // The start reads the remaining steps again, which need the busy TCS: it is refused.
      _     <- f.runner.run(
                 _.runDaytimeQueue(observer, user, clientId),
                 (r, st) => isQueueUpdate(r) && statusOf(st, obs1).contains_(QueueItemStatus.Waiting)
               )
      st    <- f.runner.drain
      reads <- f.odb.readLog
    yield
      assertEquals(statusOf(st, obs1), QueueItemStatus.Waiting.some)
      // The queue now knows it needs TCS, so it is neither loaded nor read again until TCS is free.
      assert(st.daytimeQueue.obs(obs1).exists(_.subsystems.contains(ObservatorySubsystem.Tcs)))
      assert(!st.sequences.contains(obs1))
      assertEquals(reads.count(_ === TestOdbProxy.OdbRead.RemainingSteps(obs1)), 1)
      st.sequences.contains(obs2)

  test("[DQ-22] run on an observation that now needs a busy subsystem says which one"):
    TestControl.executeEmbed:
      val outside: Observation.Id     = observationId(9)
      val stale: DaytimeQueueItem.Obs = waiting.copy(obsId = obs1)
      val initial: EngineState[IO]    =
        runningWith(
          outside,
          Instrument.Flamingos2,
          Set[Subsystem](Instrument.Flamingos2, ObservatorySubsystem.Tcs)
        )
          .apply(EngineState.default[IO])
          .copy(daytimeQueue = DaytimeQueue.Empty.copy(items = List(stale)))
      for
        f       <- fixture(
                     List(gmosNorthObs(obs1, 1, scienceConfig)),
                     daytime(obs1),
                     initial = initial
                   )
        refused <- f.runner.notification(_.runDaytimeQueueItem(obs1, observer, user, clientId))
        st      <- f.runner.state.get
      yield
        assertEquals(
          refused,
          Notification
            .SubsystemConflict(obs1, List[Subsystem](ObservatorySubsystem.Tcs), List(outside))
            .some
        )
        assert(!st.sequences.contains(obs1))

  test("[DQ-35] an observation the queue starts records when, with the estimate of what is left"):
    TestControl.executeEmbed:
      for
        f   <- fixture(List(gmosSouthObs(obs1, 2, darkConfig)), daytime(obs1))
        _   <- f.runner.add(obs1)
        _   <- IO.sleep(5.minutes)
        now <- IO.realTimeInstant
        st  <- f.runner.runUntilState(
                 _.runDaytimeQueue(observer, user, clientId),
                 allIn(QueueItemStatus.Running, obs1)
               )
      yield
        val item: Option[DaytimeQueueItem.Obs] = st.daytimeQueue.obs(obs1)
        assertEquals(item.flatMap(_.startedAt).map(_.toInstant), now.some)
        assertEquals(item.map(_.estimate), 20.secondTimeSpan.some)

  test("[DQ-13] strict ordering starts nothing below an observation refused at start"):
    TestControl.executeEmbed:
      assertIO(startsBelowRefused(QueueOrderingMode.Strict), false)

  test("[DQ-13] jump ahead starts an observation below one refused at start"):
    TestControl.executeEmbed:
      assertIO(startsBelowRefused(QueueOrderingMode.JumpAhead), true)

// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.model

import cats.syntax.all.*
import lucuma.core.enums.Instrument
import lucuma.core.model.Observation
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import munit.FunSuite
import observe.model.DaytimeQueue
import observe.model.DaytimeQueueItem
import observe.model.DaytimeQueueView
import observe.model.QueueItemId
import observe.model.QueuePauseId
import observe.model.Subsystem
import observe.model.enums.ObservatorySubsystem
import observe.model.enums.QueueDispatchState
import observe.model.enums.QueueItemStatus
import observe.model.enums.QueueOrderingMode

import java.util.UUID

class DaytimeQueueRowsSuite extends FunSuite:
  private val Start: Timestamp = Timestamp.Epoch +| 1.hourTimeSpan

  private val Gmos: Instrument    = Instrument.GmosNorth
  private val Gnirs: Instrument   = Instrument.Gnirs
  private val Gcal: Subsystem     = ObservatorySubsystem.Gcal
  private val Tcs: Subsystem      = ObservatorySubsystem.Tcs
  private val Pause: QueuePauseId = QueuePauseId(new UUID(0L, 1L))

  private def obsId(index: Long): Observation.Id = Observation.Id.fromLong(index).get

  private def obs(
    index:      Long,
    status:     QueueItemStatus,
    instrument: Instrument = Gmos,
    others:     Set[Subsystem] = Set.empty,
    estimate:   TimeSpan = 10.minuteTimeSpan,
    startedAt:  Option[Timestamp] = none,
    errorMsg:   Option[String] = none,
    total:      Option[TimeSpan] = none
  ): DaytimeQueueItem.Obs =
    DaytimeQueueItem.Obs(
      obsId(index),
      instrument,
      status,
      others + instrument,
      estimate,
      errorMsg,
      abortRequested = false,
      startedAt,
      total.getOrElse(estimate)
    )

  private def queue(
    items: List[DaytimeQueueItem],
    state: QueueDispatchState = QueueDispatchState.Running,
    mode:  QueueOrderingMode = QueueOrderingMode.JumpAhead
  ): DaytimeQueue =
    DaytimeQueue(items, mode, state, none, none)

  private def view(q: DaytimeQueue, busy: Set[Subsystem] = Set.empty): DaytimeQueueView =
    DaytimeQueueView(q, busy)

  private def obsRows(v: DaytimeQueueView, now: Timestamp): List[DaytimeQueueObsRow] =
    DaytimeQueueRows.rows(v, Map.empty, now).collect { case DaytimeQueueRow.Obs(row) => row }

  private val subsystemName: Subsystem => String =
    case Gcal  => "GCAL"
    case Tcs   => "TCS"
    case other => other.toString

  test("elapsed counts from startedAt and is none without it"):
    val running: DaytimeQueueItem.Obs = obs(1, QueueItemStatus.Running, startedAt = Start.some)
    assertEquals(
      DaytimeQueueRows.elapsed(running, Start +| 4.minuteTimeSpan),
      4.minuteTimeSpan.some
    )
    assertEquals(DaytimeQueueRows.elapsed(obs(1, QueueItemStatus.Running), Start), none)

  test("elapsed is zero when the clock is behind startedAt"):
    val running: DaytimeQueueItem.Obs = obs(1, QueueItemStatus.Running, startedAt = Start.some)
    assertEquals(
      DaytimeQueueRows.elapsed(running, Start -| 1.minuteTimeSpan),
      TimeSpan.Zero.some
    )

  test("elapsed is none for an observation that is not running"):
    val stopped: DaytimeQueueItem.Obs = obs(1, QueueItemStatus.Stopped, startedAt = Start.some)
    assertEquals(DaytimeQueueRows.elapsed(stopped, Start +| 4.minuteTimeSpan), none)

  test("remaining is the estimate minus the elapsed time, floored at zero"):
    val running: DaytimeQueueItem.Obs = obs(1, QueueItemStatus.Running, startedAt = Start.some)
    assertEquals(
      DaytimeQueueRows.remaining(running, Start +| 4.minuteTimeSpan),
      6.minuteTimeSpan
    )
    assertEquals(
      DaytimeQueueRows.remaining(running, Start +| 25.minuteTimeSpan),
      TimeSpan.Zero
    )
    assertEquals(
      DaytimeQueueRows.remaining(obs(1, QueueItemStatus.Running), Start),
      10.minuteTimeSpan
    )

  test("runningRemaining only has the running observations"):
    val q: DaytimeQueue = queue(
      List(
        obs(1, QueueItemStatus.Running, startedAt = Start.some),
        obs(2, QueueItemStatus.Waiting, Gnirs),
        obs(3, QueueItemStatus.Completed)
      )
    )
    assertEquals(
      DaytimeQueueRows.runningRemaining(q, Start +| 3.minuteTimeSpan),
      Map(obsId(1) -> 7.minuteTimeSpan)
    )

  test("doneTime leaves out what a prediction does not count as remaining"):
    val q: DaytimeQueue = queue(
      List(
        obs(1,
            QueueItemStatus.Waiting,
            estimate = 6.minuteTimeSpan,
            total = 10.minuteTimeSpan.some
        ),
        obs(2, QueueItemStatus.Failed, estimate = 7.minuteTimeSpan, total = 10.minuteTimeSpan.some),
        obs(3,
            QueueItemStatus.Stopped,
            estimate = 8.minuteTimeSpan,
            total = 10.minuteTimeSpan.some
        ),
        DaytimeQueueItem.Pause(Pause),
        obs(4,
            QueueItemStatus.Waiting,
            estimate = 9.minuteTimeSpan,
            total = 10.minuteTimeSpan.some
        ),
        obs(5, QueueItemStatus.Completed, estimate = TimeSpan.Zero, total = 5.minuteTimeSpan.some)
      )
    )
    assertEquals(DaytimeQueueRows.doneTime(q, Start), 9.minuteTimeSpan)

  test("doneTime adds what every observation has done of its total"):
    val q: DaytimeQueue = queue(
      List(
        obs(1, QueueItemStatus.Completed, estimate = TimeSpan.Zero, total = 5.minuteTimeSpan.some),
        obs(2, QueueItemStatus.Running, Gnirs, startedAt = Start.some),
        obs(3, QueueItemStatus.Failed),
        obs(4, QueueItemStatus.Waiting)
      )
    )
    assertEquals(DaytimeQueueRows.doneTime(q, Start +| 2.minuteTimeSpan), 7.minuteTimeSpan)

  test("fraction is done over done plus remaining, and zero for nothing"):
    assertEquals(DaytimeQueueRows.fraction(1.minuteTimeSpan, 3.minuteTimeSpan), 0.25)
    assertEquals(DaytimeQueueRows.fraction(TimeSpan.Zero, TimeSpan.Zero), 0.0)
    assertEquals(DaytimeQueueRows.fraction(1.minuteTimeSpan, TimeSpan.Zero), 1.0)

  test("no prediction when nothing is running or waiting above the first pause"):
    val q: DaytimeQueue = queue(
      List(
        obs(1, QueueItemStatus.Completed),
        obs(2, QueueItemStatus.Stopped),
        DaytimeQueueItem.Pause(Pause),
        obs(3, QueueItemStatus.Waiting)
      )
    )
    assertEquals(DaytimeQueueRows.prediction(q, Start), none)

  test("prediction simulates the queue from the remaining time of the running observations"):
    val q: DaytimeQueue    = queue(
      List(
        obs(1, QueueItemStatus.Completed, estimate = TimeSpan.Zero, total = 5.minuteTimeSpan.some),
        obs(2, QueueItemStatus.Running, startedAt = Start.some),
        obs(3, QueueItemStatus.Waiting, estimate = 20.minuteTimeSpan),
        DaytimeQueueItem.Pause(Pause),
        obs(4, QueueItemStatus.Waiting)
      )
    )
    val now: Timestamp     = Start +| 5.minuteTimeSpan
    // 5 min left to the running one, then 20 min of the one waiting for the same instrument.
    val expected: TimeSpan = 25.minuteTimeSpan
    assertEquals(
      DaytimeQueueRows.prediction(q, now),
      DaytimeQueuePrediction(expected, 10.0 / 35.0, (now +| expected).some).some
    )

  test("a queue that is not being run has no finish time"):
    val items: List[DaytimeQueueItem] = List(obs(1, QueueItemStatus.Waiting))
    assertEquals(
      DaytimeQueueRows.prediction(queue(items, QueueDispatchState.Idle), Start),
      DaytimeQueuePrediction(10.minuteTimeSpan, 0.0, none).some
    )
    assertEquals(
      DaytimeQueueRows.prediction(queue(items, QueueDispatchState.Halted), Start).map(_.finishAt),
      none.some
    )

  test("prediction runs observations with different subsystems in parallel"):
    val q: DaytimeQueue = queue(
      List(
        obs(1, QueueItemStatus.Waiting, Gmos, estimate = 10.minuteTimeSpan),
        obs(2, QueueItemStatus.Waiting, Gnirs, estimate = 15.minuteTimeSpan)
      )
    )
    assertEquals(DaytimeQueueRows.prediction(q, Start).map(_.remaining), 15.minuteTimeSpan.some)
    assertEquals(DaytimeQueueRows.prediction(q, Start).map(_.fraction), 0.0.some)

  test("lampsOffIn counts down to lampsOffAt and stops at zero"):
    val q: DaytimeQueue =
      DaytimeQueue.lampsOffAt.replace((Start +| 60.secondTimeSpan).some)(queue(Nil))
    assertEquals(DaytimeQueueRows.lampsOffIn(queue(Nil), Start), none)
    assertEquals(
      DaytimeQueueRows.lampsOffIn(q, Start +| 15.secondTimeSpan),
      45.secondTimeSpan.some
    )
    assertEquals(DaytimeQueueRows.lampsOffIn(q, Start +| 2.minuteTimeSpan), TimeSpan.Zero.some)

  test("a failed observation shows its error"):
    val failed: DaytimeQueueItem.Obs   = obs(1, QueueItemStatus.Failed, errorMsg = "Boom".some)
    val note: Option[DaytimeQueueNote] = DaytimeQueueRows.note(view(queue(List(failed))), failed)
    assertEquals(note, DaytimeQueueNote.Failed("Boom").some)
    assertEquals(note.map(_.text(subsystemName)), "Boom".some)

  test("a waiting observation names the busy subsystems it needs"):
    val waiting: DaytimeQueueItem.Obs  = obs(1, QueueItemStatus.Waiting, others = Set(Tcs, Gcal))
    val v: DaytimeQueueView            = view(queue(List(waiting)), busy = Set(Gcal, Tcs, Gnirs))
    val note: Option[DaytimeQueueNote] = DaytimeQueueRows.note(v, waiting)
    assertEquals(note, DaytimeQueueNote.BlockedBy(List(Tcs, Gcal)).some)
    assertEquals(note.map(_.text(subsystemName)), "Waiting for TCS, GCAL".some)

  test("a waiting observation below the first pause says so"):
    val waiting: DaytimeQueueItem.Obs = obs(1, QueueItemStatus.Waiting)
    val v: DaytimeQueueView           = view(queue(List(DaytimeQueueItem.Pause(Pause), waiting)))
    assertEquals(DaytimeQueueRows.note(v, waiting), DaytimeQueueNote.BelowPause.some)
    assertEquals(DaytimeQueueNote.BelowPause.text(subsystemName), "Below pause")

  test("a waiting observation the dispatcher does not start waits for its turn"):
    val first: DaytimeQueueItem.Obs  = obs(1, QueueItemStatus.Waiting, Gmos)
    val second: DaytimeQueueItem.Obs = obs(2, QueueItemStatus.Waiting, Gmos)
    val v: DaytimeQueueView          = view(queue(List(first, second)))
    assertEquals(DaytimeQueueRows.note(v, first), none)
    assertEquals(DaytimeQueueRows.note(v, second), DaytimeQueueNote.WaitingForTurn.some)
    assertEquals(DaytimeQueueNote.WaitingForTurn.text(subsystemName), "Waiting for its turn")

  test("a waiting observation has no note while the dispatcher is not running"):
    val waiting: DaytimeQueueItem.Obs = obs(1, QueueItemStatus.Waiting, others = Set(Gcal))
    List(QueueDispatchState.Idle, QueueDispatchState.Halted).foreach: state =>
      val v: DaytimeQueueView =
        view(queue(List(DaytimeQueueItem.Pause(Pause), waiting), state), busy = Set(Gcal))
      assertEquals(DaytimeQueueRows.note(v, waiting), none)

  test("actions follow the status"):
    def row(status: QueueItemStatus): DaytimeQueueObsRow =
      obsRows(view(queue(List(obs(1, status)))), Start).head

    def actions(status: QueueItemStatus): List[Boolean] =
      val r: DaytimeQueueObsRow = row(status)
      List(r.canRun, r.canPause, r.canAbort, r.canRetry, r.canRemove)

    assertEquals(actions(QueueItemStatus.Waiting), List(true, false, false, false, true))
    assertEquals(actions(QueueItemStatus.Running), List(false, true, true, false, false))
    assertEquals(actions(QueueItemStatus.Stopped), List(true, false, false, false, true))
    assertEquals(actions(QueueItemStatus.Completed), List(false, false, false, false, true))
    assertEquals(actions(QueueItemStatus.Failed), List(true, false, false, true, true))
    assertEquals(actions(QueueItemStatus.Aborted), List(true, false, false, true, true))

  test("run is disabled while a subsystem the observation needs is busy"):
    val v: DaytimeQueueView     =
      view(queue(List(obs(1, QueueItemStatus.Stopped, others = Set(Gcal)))), busy = Set(Gcal))
    val row: DaytimeQueueObsRow = obsRows(v, Start).head
    assert(row.runApplies)
    assert(!row.canRun)
    assertEquals(row.blockedBy, List(Gcal))

  test("abort is disabled once it was requested"):
    val aborting: DaytimeQueueItem.Obs =
      obs(1, QueueItemStatus.Running).copy(abortRequested = true)
    val row: DaytimeQueueObsRow        = obsRows(view(queue(List(aborting))), Start).head
    assert(!row.canAbort)
    assert(row.canPause)

  test("only running observations have progress"):
    val v: DaytimeQueueView                          = view(
      queue(
        List(
          obs(1, QueueItemStatus.Running, Gmos, startedAt = Start.some),
          obs(2, QueueItemStatus.Running, Gnirs),
          obs(3, QueueItemStatus.Waiting, startedAt = Start.some)
        )
      )
    )
    val progress: List[Option[DaytimeQueueProgress]] =
      obsRows(v, Start +| 4.minuteTimeSpan).map(_.progress)
    assertEquals(
      progress,
      List(
        DaytimeQueueProgress(4.minuteTimeSpan, 10.minuteTimeSpan).some,
        DaytimeQueueProgress(TimeSpan.Zero, 10.minuteTimeSpan).some,
        none
      )
    )
    assertEquals(progress.map(_.map(_.fraction)), List(0.4.some, 0.0.some, none))

  test("an observation that stopped part way keeps its progress"):
    val stopped: DaytimeQueueItem.Obs   =
      obs(1, QueueItemStatus.Stopped, estimate = 6.minuteTimeSpan, total = 10.minuteTimeSpan.some)
    val untouched: DaytimeQueueItem.Obs = obs(2, QueueItemStatus.Waiting)
    val done: DaytimeQueueItem.Obs      =
      obs(3, QueueItemStatus.Completed, estimate = TimeSpan.Zero, total = 10.minuteTimeSpan.some)
    assertEquals(
      DaytimeQueueRows.progress(stopped, Start),
      DaytimeQueueProgress(4.minuteTimeSpan, 10.minuteTimeSpan).some
    )
    assertEquals(DaytimeQueueRows.progress(untouched, Start), none)
    assertEquals(DaytimeQueueRows.progress(done, Start), none)

  test("progress fraction stops at one"):
    assertEquals(DaytimeQueueProgress(15.minuteTimeSpan, 10.minuteTimeSpan).fraction, 1.0)

  test("progress has what is left of the total, never negative"):
    assertEquals(
      DaytimeQueueProgress(4.minuteTimeSpan, 10.minuteTimeSpan).remaining,
      6.minuteTimeSpan
    )
    assertEquals(
      DaytimeQueueProgress(15.minuteTimeSpan, 10.minuteTimeSpan).remaining,
      TimeSpan.Zero
    )

  test("rows keep the queue order, mark the first pause and lock running observations"):
    val second: QueuePauseId        = QueuePauseId(new UUID(0L, 2L))
    val v: DaytimeQueueView         = view(
      queue(
        List(
          obs(1, QueueItemStatus.Running),
          DaytimeQueueItem.Pause(Pause),
          obs(2, QueueItemStatus.Waiting),
          DaytimeQueueItem.Pause(second)
        )
      )
    )
    val rows: List[DaytimeQueueRow] = DaytimeQueueRows.rows(v, Map.empty, Start)
    assertEquals(
      rows.map(_.itemId),
      List(
        QueueItemId.Obs(obsId(1)),
        QueueItemId.Pause(Pause),
        QueueItemId.Obs(obsId(2)),
        QueueItemId.Pause(second)
      )
    )
    assertEquals(rows.map(_.isLocked), List(true, false, false, false))
    assertEquals(
      rows.collect { case DaytimeQueueRow.Pause(_, isFirst) => isFirst },
      List(true, false)
    )

  test("queue controls"):
    val idle: DaytimeQueue    =
      queue(List(obs(1, QueueItemStatus.Waiting)), QueueDispatchState.Idle)
    val running: DaytimeQueue =
      queue(List(obs(1, QueueItemStatus.Running), obs(2, QueueItemStatus.Completed)))
    val empty: DaytimeQueue   = queue(Nil, QueueDispatchState.Idle)
    val manual: DaytimeQueue  =
      queue(List(obs(1, QueueItemStatus.Running)), QueueDispatchState.Halted)

    assert(DaytimeQueueRows.canRunQueue(idle))
    assert(!DaytimeQueueRows.canRunQueue(running))
    assert(
      DaytimeQueueRows.canRunQueue:
        queue(List(obs(1, QueueItemStatus.Stopped)), QueueDispatchState.Running)
    )
    assert(!DaytimeQueueRows.canRunQueue(empty))
    assert(!DaytimeQueueRows.canHalt(idle))
    assert(DaytimeQueueRows.canHalt(running))
    assert(DaytimeQueueRows.canHalt(manual))
    assert(!DaytimeQueueRows.hasRunning(idle))
    assert(DaytimeQueueRows.hasRunning(running))
    assert(!DaytimeQueueRows.hasCompleted(idle))
    assert(DaytimeQueueRows.hasCompleted(running))

  private val PauseItem: DaytimeQueueItem = DaytimeQueueItem.Pause(Pause)

  private def waiting(index: Long): DaytimeQueueItem = obs(index, QueueItemStatus.Waiting)

  private def running(index: Long): DaytimeQueueItem = obs(index, QueueItemStatus.Running)

  test("a new pause goes after the first observation"):
    assertEquals(
      DaytimeQueueRows.newPauseIndex(queue(List(waiting(1), waiting(2), waiting(3)))),
      1.some
    )

  test("a new pause skips a place that already has one"):
    assertEquals(
      DaytimeQueueRows.newPauseIndex(queue(List(waiting(1), PauseItem, waiting(2), waiting(3)))),
      3.some
    )

  test("a new pause goes below the running observations"):
    assertEquals(
      DaytimeQueueRows.newPauseIndex(
        queue(List(running(1), waiting(2), running(3), waiting(4), waiting(5)))
      ),
      3.some
    )
    assertEquals(
      DaytimeQueueRows.newPauseIndex(queue(List(running(1), waiting(2), waiting(3)))),
      1.some
    )

  test("a new pause goes below an observation that is yet to finish"):
    val completed: DaytimeQueueItem   = obs(1, QueueItemStatus.Completed)
    val stopped: DaytimeQueueItem     = obs(2, QueueItemStatus.Stopped)
    val items: List[DaytimeQueueItem] = List(completed, stopped, waiting(3), waiting(4))
    List(QueueDispatchState.Running, QueueDispatchState.Idle).foreach: state =>
      val q: DaytimeQueue      = queue(items, state)
      assertEquals(DaytimeQueueRows.newPauseIndex(q), 3.some)
      // Neither held at the new pause, nor does Run Queue take it away.
      val paused: DaytimeQueue = q.insertPause(Pause, 3)
      assert(!paused.isAtPause)
      assertEquals(paused.run.items, paused.resetStopped.items)
    assertEquals(DaytimeQueueRows.newPauseIndex(queue(List(completed, waiting(3)))), none)

  test("a pause cannot be moved to where it holds a running queue at once"):
    val pauseId: QueueItemId          = QueueItemId.Pause(Pause)
    val items: List[DaytimeQueueItem] = List(running(1), waiting(2), PauseItem, waiting(3))
    val q: DaytimeQueue               = queue(items)
    assert(DaytimeQueueRows.pauseMoveHalts(q, pauseId, 0))
    assert(!DaytimeQueueRows.pauseMoveHalts(q, pauseId, 1))
    assert(!DaytimeQueueRows.pauseMoveHalts(queue(items, QueueDispatchState.Idle), pauseId, 0))
    assert(!DaytimeQueueRows.pauseMoveHalts(q, QueueItemId.Obs(obsId(3)), 0))
    val blocked: DaytimeQueue         =
      queue(List(obs(1, QueueItemStatus.Completed), waiting(2), PauseItem, waiting(3)))
    assert(DaytimeQueueRows.pauseMoveHalts(blocked, pauseId, 1))
    assert(!DaytimeQueueRows.pauseMoveHalts(blocked, pauseId, 3))

  test("no new pause without two observations together below the running ones"):
    assertEquals(DaytimeQueueRows.newPauseIndex(queue(Nil)), none)
    assertEquals(DaytimeQueueRows.newPauseIndex(queue(List(waiting(1)))), none)
    assertEquals(
      DaytimeQueueRows.newPauseIndex(queue(List(waiting(1), PauseItem, waiting(2)))),
      none
    )
    assertEquals(
      DaytimeQueueRows.newPauseIndex(queue(List(waiting(1), waiting(2), running(3)))),
      none
    )

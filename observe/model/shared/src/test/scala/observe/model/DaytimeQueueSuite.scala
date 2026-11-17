// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model

import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosLong
import lucuma.core.enums.Instrument
import lucuma.core.syntax.timespan.*
import observe.model.enums.*

import java.util.UUID

class DaytimeQueueSuite extends munit.FunSuite:
  def oid(n: Long): Observation.Id =
    lucuma.core.model.Observation.Id(PosLong.unsafeFrom(n))

  def pid(n: Long): QueuePauseId = QueuePauseId(new UUID(0L, n))

  def obs(
    n:      Long,
    status: QueueItemStatus = QueueItemStatus.Waiting
  ): DaytimeQueueItem.Obs =
    DaytimeQueueItem.Obs(
      oid(n),
      Instrument.GmosNorth,
      status,
      Set[Subsystem](Instrument.GmosNorth),
      60.secondTimeSpan,
      none,
      false,
      none
    )

  def pause(n: Long): DaytimeQueueItem.Pause = DaytimeQueueItem.Pause(pid(n))

  def queue(items: DaytimeQueueItem*): DaytimeQueue =
    DaytimeQueue.Empty.copy(items = items.toList)

  def ids(q: DaytimeQueue): List[QueueItemId] = q.items.map(_.itemId)

  def obsIds(n: Long*): List[QueueItemId] = n.toList.map(i => QueueItemId.Obs(oid(i)))

  test("[DQ-13] empty defaults"):
    assertEquals(DaytimeQueue.Empty.orderingMode, QueueOrderingMode.JumpAhead)
    assertEquals(DaytimeQueue.Empty.dispatchState, QueueDispatchState.Idle)
    assert(DaytimeQueue.Empty.isFinished)

  test("[DQ-1] append adds at the end"):
    assertEquals(ids(queue(obs(1)).append(obs(2))), obsIds(1, 2))

  test("[DQ-5] append ignores duplicates in any status"):
    val q: DaytimeQueue = queue(obs(1, QueueItemStatus.Completed))
    assertEquals(q.append(obs(1)), q)

  test("[DQ-8, DQ-9] remove removes obs and pause"):
    val q: DaytimeQueue = queue(obs(1), pause(1), obs(2))
    assertEquals(ids(q.remove(QueueItemId.Obs(oid(1)))),
                 List(QueueItemId.Pause(pid(1))) ++ obsIds(2)
    )
    assertEquals(ids(q.remove(QueueItemId.Pause(pid(1)))), obsIds(1, 2))

  test("[DQ-7] remove cannot remove running"):
    val q: DaytimeQueue = queue(obs(1, QueueItemStatus.Running))
    assertEquals(q.remove(QueueItemId.Obs(oid(1))), q)

  test("remove unknown id is a no-op"):
    val q: DaytimeQueue = queue(obs(1))
    assertEquals(q.remove(QueueItemId.Obs(oid(9))), q)

  test("[DQ-6] move down and up"):
    val q: DaytimeQueue = queue(obs(1), obs(2), obs(3))
    assertEquals(ids(q.move(QueueItemId.Obs(oid(1)), 2)), obsIds(2, 3, 1))
    assertEquals(ids(q.move(QueueItemId.Obs(oid(3)), 0)), obsIds(3, 1, 2))
    assertEquals(ids(q.move(QueueItemId.Obs(oid(1)), 1)), obsIds(2, 1, 3))

  test("[DQ-6] move clamps the index"):
    val q: DaytimeQueue = queue(obs(1), obs(2), obs(3))
    assertEquals(ids(q.move(QueueItemId.Obs(oid(1)), 99)), obsIds(2, 3, 1))
    assertEquals(ids(q.move(QueueItemId.Obs(oid(3)), -5)), obsIds(3, 1, 2))

  test("[DQ-7] move cannot move running but others cross it"):
    val q: DaytimeQueue = queue(obs(1, QueueItemStatus.Running), obs(2), obs(3))
    assertEquals(q.move(QueueItemId.Obs(oid(1)), 2), q)
    assertEquals(ids(q.move(QueueItemId.Obs(oid(3)), 0)), obsIds(3, 1, 2))

  test("[DQ-9] move a pause"):
    val q: DaytimeQueue = queue(pause(1), obs(1), obs(2))
    assertEquals(q.move(QueueItemId.Pause(pid(1)), 2).items.last, pause(1))

  test("[DQ-9] insertPause inserts and clamps"):
    val q: DaytimeQueue = queue(obs(1), obs(2))
    assertEquals(q.insertPause(pid(1), 1).items, List(obs(1), pause(1), obs(2)))
    assertEquals(q.insertPause(pid(1), 99).items, List(obs(1), obs(2), pause(1)))
    assertEquals(q.insertPause(pid(1), -3).items, List(pause(1), obs(1), obs(2)))

  test("[DQ-18] continuePastPause removes the first pause only"):
    val q: DaytimeQueue = queue(obs(1), pause(1), obs(2), pause(2))
    assertEquals(q.continuePastPause.items, List(obs(1), obs(2), pause(2)))
    assertEquals(queue(obs(1)).continuePastPause, queue(obs(1)))

  test("[DQ-38] clearCompleted keeps everything else and pauses"):
    val q: DaytimeQueue =
      queue(
        obs(1, QueueItemStatus.Completed),
        pause(1),
        obs(2, QueueItemStatus.Failed),
        obs(3, QueueItemStatus.Completed),
        obs(4)
      )
    assertEquals(q.clearCompleted.items, List(pause(1), obs(2, QueueItemStatus.Failed), obs(4)))

  test("[DQ-30] retry resets failed and aborted"):
    val failed: DaytimeQueueItem.Obs  =
      obs(1, QueueItemStatus.Failed).copy(errorMsg = "boom".some)
    val aborted: DaytimeQueueItem.Obs =
      obs(2, QueueItemStatus.Aborted).copy(abortRequested = true)
    val q: DaytimeQueue               = queue(failed, aborted)
    assertEquals(q.retry(oid(1)).items.head, obs(1))
    assertEquals(q.retry(oid(2)).items.last, obs(2))

  test("[DQ-30] retry leaves other statuses alone"):
    val q: DaytimeQueue = queue(obs(1, QueueItemStatus.Completed), obs(2, QueueItemStatus.Running))
    assertEquals(q.retry(oid(1)), q)
    assertEquals(q.retry(oid(2)), q)

  test("[DQ-16] resetStopped"):
    val q: DaytimeQueue = queue(obs(1, QueueItemStatus.Stopped), obs(2, QueueItemStatus.Failed))
    assertEquals(q.resetStopped.items, List(obs(1), obs(2, QueueItemStatus.Failed)))

  test("modifyObs"):
    val q: DaytimeQueue = queue(obs(1), obs(2))
    assertEquals(
      q.modifyObs(oid(2))(_.copy(abortRequested = true)).obs(oid(2)).map(_.abortRequested),
      true.some
    )
    assertEquals(q.modifyObs(oid(9))(_.copy(abortRequested = true)), q)

  test("[DQ-11] runningSubsystems and exclusion"):
    val a: DaytimeQueueItem.Obs =
      obs(1, QueueItemStatus.Running).copy(subsystems = Set[Subsystem](Instrument.GmosNorth))
    val b: DaytimeQueueItem.Obs =
      obs(2, QueueItemStatus.Running).copy(subsystems = Set[Subsystem](ObservatorySubsystem.Gcal))
    val w: DaytimeQueueItem.Obs =
      obs(3).copy(subsystems = Set[Subsystem](ObservatorySubsystem.Gems))
    val q: DaytimeQueue         = queue(a, b, w)
    assertEquals(
      q.runningSubsystems(),
      Set[Subsystem](Instrument.GmosNorth, ObservatorySubsystem.Gcal)
    )
    assertEquals(q.runningSubsystems(oid(1).some), Set[Subsystem](ObservatorySubsystem.Gcal))

  test("[DQ-39] isFinished"):
    assert(queue(obs(1, QueueItemStatus.Completed), obs(2, QueueItemStatus.Failed)).isFinished)
    assert(!queue(obs(1, QueueItemStatus.Waiting)).isFinished)
    assert(!queue(obs(1, QueueItemStatus.Running)).isFinished)
    assert(!queue(obs(1, QueueItemStatus.Stopped)).isFinished)
    assert(queue(pause(1)).isFinished)

  test("[DQ-37] obsBlockedBy"):
    val w: DaytimeQueueItem.Obs =
      obs(1).copy(subsystems = Set[Subsystem](Instrument.GmosNorth, ObservatorySubsystem.Gcal))
    val r: DaytimeQueueItem.Obs = obs(2, QueueItemStatus.Running)
    val v: DaytimeQueueView     =
      DaytimeQueueView(queue(w, r), Set[Subsystem](ObservatorySubsystem.Gcal, Instrument.GmosNorth))
    assertEquals(
      v.obsBlockedBy(oid(1)),
      Set[Subsystem](ObservatorySubsystem.Gcal, Instrument.GmosNorth)
    )
    assertEquals(v.obsBlockedBy(oid(2)), Set.empty[Subsystem])
    assertEquals(v.obsBlockedBy(oid(9)), Set.empty[Subsystem])

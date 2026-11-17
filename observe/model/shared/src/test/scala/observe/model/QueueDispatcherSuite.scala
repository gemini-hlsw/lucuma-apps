// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model

import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosLong
import lucuma.core.enums.Instrument
import lucuma.core.util.TimeSpan
import observe.model.arb.ArbDaytimeQueue.given
import observe.model.enums.*
import org.scalacheck.Prop.forAll

class QueueDispatcherSuite extends munit.ScalaCheckSuite:
  val GmosN: Subsystem = Instrument.GmosNorth
  val F2: Subsystem    = Instrument.Flamingos2
  val Gcal: Subsystem  = ObservatorySubsystem.Gcal
  val Tcs: Subsystem   = ObservatorySubsystem.Tcs

  def oid(n: Long): Observation.Id =
    lucuma.core.model.Observation.Id(PosLong.unsafeFrom(n))

  def obs(
    n:          Long,
    subsystems: Set[Subsystem],
    status:     QueueItemStatus = QueueItemStatus.Waiting,
    seconds:    Long = 60L
  ): DaytimeQueueItem.Obs =
    DaytimeQueueItem.Obs(
      oid(n),
      Instrument.GmosNorth,
      status,
      subsystems,
      TimeSpan.unsafeFromMicroseconds(seconds * 1000000L),
      none,
      false,
      none
    )

  def pause: DaytimeQueueItem.Pause =
    DaytimeQueueItem.Pause(
      QueuePauseId(java.util.UUID.fromString("00000000-0000-0000-0000-000000000001"))
    )

  def queue(mode: QueueOrderingMode, items: DaytimeQueueItem*): DaytimeQueue =
    DaytimeQueue.Empty.copy(items = items.toList, orderingMode = mode)

  def ids(n: Long*): List[Observation.Id] = n.toList.map(oid)

  def secs(n: Long): TimeSpan = TimeSpan.unsafeFromMicroseconds(n * 1000000L)

  import QueueOrderingMode.{JumpAhead, Strict}

  test("[DQ-11] disjoint subsystems start together"):
    val q: DaytimeQueue = queue(JumpAhead, obs(1, Set(GmosN)), obs(2, Set(F2)))
    assertEquals(QueueDispatcher.runnable(q, Set.empty), ids(1, 2))

  test("[DQ-11] shared subsystem starts only the first"):
    val q: DaytimeQueue = queue(JumpAhead, obs(1, Set(GmosN, Gcal)), obs(2, Set(F2, Gcal)))
    assertEquals(QueueDispatcher.runnable(q, Set.empty), ids(1))

  test("[DQ-13] strict stops at the first blocked, jump-ahead skips it"):
    val items: List[DaytimeQueueItem.Obs] =
      List(obs(1, Set(Gcal)), obs(2, Set(GmosN, Gcal)), obs(3, Set(F2)))
    assertEquals(QueueDispatcher.runnable(queue(Strict, items*), Set(Gcal)), Nil)
    assertEquals(QueueDispatcher.runnable(queue(JumpAhead, items*), Set(Gcal)), ids(3))
    assertEquals(QueueDispatcher.runnable(queue(Strict, items*), Set.empty), ids(1, 3).take(1))

  test("[DQ-13] jump-ahead skipped obs does not reserve its subsystems"):
    val q: DaytimeQueue  =
      queue(JumpAhead, obs(1, Set(GmosN, Gcal)), obs(2, Set(F2, GmosN)), obs(3, Set(F2)))
    // 1 is blocked by busy Gcal; 2 would claim F2+GmosN but starts; 3 conflicts with 2
    assertEquals(QueueDispatcher.runnable(q, Set(Gcal)), ids(2))
    val q2: DaytimeQueue =
      queue(JumpAhead, obs(1, Set(GmosN, Gcal)), obs(3, Set(GmosN)))
    assertEquals(QueueDispatcher.runnable(q2, Set(Gcal)), ids(3))

  test("[DQ-17] pause is a barrier"):
    val q: DaytimeQueue = queue(JumpAhead, obs(1, Set(GmosN)), pause, obs(2, Set(F2)))
    assertEquals(QueueDispatcher.runnable(q, Set.empty), ids(1))
    assertEquals(QueueDispatcher.runnable(q.continuePastPause, Set.empty), ids(1, 2))

  test("[DQ-12] instrument-only item starts while GCAL is busy"):
    val q: DaytimeQueue = queue(Strict, obs(1, Set(GmosN)))
    assertEquals(QueueDispatcher.runnable(q, Set(Gcal)), ids(1))

  test("[DQ-14] non-waiting items are ignored and never stop the walk"):
    val q: DaytimeQueue =
      queue(
        Strict,
        obs(1, Set(GmosN), QueueItemStatus.Running),
        obs(2, Set(GmosN), QueueItemStatus.Completed),
        obs(3, Set(GmosN), QueueItemStatus.Failed),
        obs(4, Set(GmosN), QueueItemStatus.Aborted),
        obs(5, Set(GmosN), QueueItemStatus.Stopped),
        obs(6, Set(F2))
      )
    assertEquals(QueueDispatcher.runnable(q, Set(GmosN)), ids(6))

  test("[DQ-11] running items block even when busy leaves them out"):
    val q: DaytimeQueue =
      queue(JumpAhead, obs(1, Set(GmosN, Gcal), QueueItemStatus.Running), obs(2, Set(F2, Gcal)))
    assertEquals(QueueDispatcher.runnable(q, Set.empty), ids())

  test("[DQ-11] outside busy subsystems respected"):
    val q: DaytimeQueue = queue(JumpAhead, obs(1, Set(GmosN, Tcs)), obs(2, Set(F2)))
    assertEquals(QueueDispatcher.runnable(q, Set(Tcs)), ids(2))

  test("[DQ-35] predict: disjoint is the max"):
    val q: DaytimeQueue =
      queue(JumpAhead, obs(1, Set(GmosN), seconds = 100), obs(2, Set(F2), seconds = 40))
    assertEquals(QueueDispatcher.predictRemaining(q, Map.empty), secs(100))

  test("[DQ-35] predict: conflicting is the sum"):
    val q: DaytimeQueue =
      queue(JumpAhead, obs(1, Set(Gcal), seconds = 100), obs(2, Set(Gcal), seconds = 40))
    assertEquals(QueueDispatcher.predictRemaining(q, Map.empty), secs(140))

  test("[DQ-35] predict: running uses supplied remaining"):
    val q: DaytimeQueue =
      queue(
        JumpAhead,
        obs(1, Set(Gcal), QueueItemStatus.Running, 100),
        obs(2, Set(Gcal), seconds = 40)
      )
    assertEquals(QueueDispatcher.predictRemaining(q, Map(oid(1) -> secs(10))), secs(50))
    assertEquals(QueueDispatcher.predictRemaining(q, Map.empty), secs(140))

  test("[DQ-35] predict: items below a pause are excluded"):
    val q: DaytimeQueue =
      queue(JumpAhead, obs(1, Set(GmosN), seconds = 30), pause, obs(2, Set(F2), seconds = 500))
    assertEquals(QueueDispatcher.predictRemaining(q, Map.empty), secs(30))

  test("[DQ-35] predict: running obs below a pause still counts and blocks"):
    val q: DaytimeQueue =
      queue(
        JumpAhead,
        obs(1, Set(Gcal), seconds = 30),
        pause,
        obs(2, Set(Gcal), QueueItemStatus.Running, 50)
      )
    assertEquals(QueueDispatcher.predictRemaining(q, Map.empty), secs(80))

  test("[DQ-35] predict: never-startable item does not hang"):
    val q: DaytimeQueue =
      queue(Strict, obs(1, Set(GmosN), seconds = 20), obs(2, Set(F2), QueueItemStatus.Stopped))
    assertEquals(QueueDispatcher.predictRemaining(q, Map.empty), secs(20))
    assertEquals(QueueDispatcher.predictRemaining(DaytimeQueue.Empty, Map.empty), TimeSpan.Zero)

  test("[DQ-35] predict: strict ordering delays later obs"):
    val q: DaytimeQueue =
      queue(
        Strict,
        obs(1, Set(GmosN), QueueItemStatus.Running, 100),
        obs(2, Set(GmosN), seconds = 10),
        obs(3, Set(F2), seconds = 10)
      )
    assertEquals(QueueDispatcher.predictRemaining(q, Map.empty), secs(110))

  property("predict terminates and is bounded by the sum"):
    forAll { (q: DaytimeQueue) =>
      val total: TimeSpan =
        q.obsItems.foldLeft(TimeSpan.Zero)((acc, o) => acc +| o.estimate)
      QueueDispatcher.predictRemaining(q, Map.empty) <= total
    }

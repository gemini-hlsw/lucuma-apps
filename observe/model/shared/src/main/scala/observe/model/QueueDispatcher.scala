// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model

import cats.syntax.all.*
import lucuma.core.util.TimeSpan
import observe.model.enums.QueueItemStatus
import observe.model.enums.QueueOrderingMode

import scala.annotation.tailrec

/** Pure scheduling rules of the daytime queue. */
object QueueDispatcher:

  /**
   * Waiting observations to start now, in queue order. Only items above the first pause are
   * considered. The subsystems of the Running items always count as busy, on top of `busy`.
   */
  def runnable(queue: DaytimeQueue, busy: Set[Subsystem]): List[Observation.Id] =
    val strict: Boolean = queue.orderingMode === QueueOrderingMode.Strict

    @tailrec
    def loop(
      items:   List[DaytimeQueueItem.Obs],
      claimed: Set[Subsystem],
      started: List[Observation.Id]
    ): List[Observation.Id] =
      items match
        case Nil          => started.reverse
        case item :: rest =>
          if item.status =!= QueueItemStatus.Waiting then loop(rest, claimed, started)
          else if claimed.intersect(item.subsystems).isEmpty then
            loop(rest, claimed ++ item.subsystems, item.obsId :: started)
          else if strict then started.reverse
          else loop(rest, claimed, started)

    loop(queue.obsAbovePause, busy ++ queue.runningSubsystems(), List.empty)

  /**
   * Predicted time until the queue stops making progress, simulating the dispatch rules with
   * nothing outside the queue busy. Waiting observations that can never start are left out.
   */
  def predictRemaining(
    queue:            DaytimeQueue,
    runningRemaining: Map[Observation.Id, TimeSpan]
  ): TimeSpan =
    def running(q: DaytimeQueue): List[DaytimeQueueItem.Obs] =
      q.obsItems.filter(_.isRunning)

    @tailrec
    def loop(q: DaytimeQueue, remaining: Map[Observation.Id, TimeSpan], clock: TimeSpan): TimeSpan =
      val toStart: List[Observation.Id]             = runnable(q, Set.empty)
      val started: DaytimeQueue                     =
        toStart.foldLeft(q)((acc, id) =>
          acc.modifyObs(id)(_.copy(status = QueueItemStatus.Running))
        )
      val remaining2: Map[Observation.Id, TimeSpan] =
        remaining ++ toStart.flatMap(id => started.obs(id).map(o => id -> o.estimate))
      val active: List[DaytimeQueueItem.Obs]        = running(started)
      if active.isEmpty then clock
      else
        val left: List[(Observation.Id, TimeSpan)]                                            =
          active.map(o => o.obsId -> remaining2.getOrElse(o.obsId, o.estimate))
        val step: TimeSpan                                                                    = left.map(_._2).reduceLeft(_ min _)
        val (done: List[(Observation.Id, TimeSpan)], going: List[(Observation.Id, TimeSpan)]) =
          left.partition(_._2 === step)
        val next: DaytimeQueue                                                                =
          done.foldLeft(started)((acc, d) =>
            acc.modifyObs(d._1)(_.copy(status = QueueItemStatus.Completed))
          )
        loop(next, going.map((id, t) => id -> (t -| step)).toMap, clock +| step)

    val initial: Map[Observation.Id, TimeSpan] =
      running(queue).map(o => o.obsId -> runningRemaining.getOrElse(o.obsId, o.estimate)).toMap
    loop(queue, initial, TimeSpan.Zero)

// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import lucuma.core.model.Observation
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import observe.model.DaytimeQueue
import observe.model.DaytimeQueueItem
import observe.model.DaytimeQueueView
import observe.model.QueueDispatcher
import observe.model.QueueItemId
import observe.model.QueuePauseId
import observe.model.Subsystem
import observe.model.enums.QueueDispatchState
import observe.model.enums.QueueItemStatus
import observe.model.given

/** Why an observation of the daytime queue is not running. */
enum DaytimeQueueNote derives Eq:
  case Failed(message: String)
  case BlockedBy(subsystems: List[Subsystem])
  case BelowPause
  case WaitingForTurn

  def text(subsystemName: Subsystem => String): String =
    this match
      case Failed(message)       => message
      case BlockedBy(subsystems) => s"Waiting for ${subsystems.map(subsystemName).mkString(", ")}"
      case BelowPause            => "Below pause"
      case WaitingForTurn        => "Waiting for its turn"

/** How far an observation of the daytime queue has got. */
enum DaytimeQueueProgress derives Eq:
  /** Started outside the queue, so there is no start time to measure from. */
  case Indeterminate
  case Done(done: TimeSpan, total: TimeSpan)

  /** Completed fraction, between 0 and 1, when it can be known. */
  def fraction: Option[Double] =
    this match
      case Indeterminate     => none
      case Done(done, total) => DaytimeQueueRows.fraction(done, total -| done).some

/** An observation of the daytime queue, with everything its table row shows. */
case class DaytimeQueueObsRow(
  item:      DaytimeQueueItem.Obs,
  summary:   Option[ObsSummary],
  blockedBy: List[Subsystem],
  note:      Option[DaytimeQueueNote],
  progress:  Option[DaytimeQueueProgress]
) derives Eq:
  val obsId: Observation.Id   = item.obsId
  val status: QueueItemStatus = item.status
  val isRunning: Boolean      = status === QueueItemStatus.Running

  /** Run applies to this status; busy subsystems may still prevent it. */
  val runApplies: Boolean =
    !isRunning && status =!= QueueItemStatus.Completed
  val canRun: Boolean     = runApplies && blockedBy.isEmpty
  val canPause: Boolean   = isRunning
  val canAbort: Boolean   = isRunning && !item.abortRequested
  val canRetry: Boolean   =
    status === QueueItemStatus.Failed || status === QueueItemStatus.Aborted
  val canRemove: Boolean  = !isRunning

enum DaytimeQueueRow derives Eq:
  case Obs(row: DaytimeQueueObsRow)
  case Pause(id: QueuePauseId, isFirst: Boolean)

  def itemId: QueueItemId =
    this match
      case Obs(row)     => QueueItemId.Obs(row.obsId)
      case Pause(id, _) => QueueItemId.Pause(id)

  /** Running observations can be neither moved nor removed. */
  def isLocked: Boolean =
    this match
      case Obs(row)    => row.isRunning
      case Pause(_, _) => false

/** Predicted end of the daytime queue. */
case class DaytimeQueuePrediction(remaining: TimeSpan, fraction: Double, finishAt: Timestamp)
    derives Eq

/** View-model of the daytime queue tab. */
object DaytimeQueueRows:

  /** Part of the whole that is done, between 0 and 1; 0 when there is nothing to do. */
  def fraction(done: TimeSpan, remaining: TimeSpan): Double =
    val totalMicros: Long = (done +| remaining).toMicroseconds
    if totalMicros === 0L then 0.0 else done.toMicroseconds.toDouble / totalMicros.toDouble

  /** Time a Running observation has been running; none when the queue did not start it. */
  def elapsed(item: DaytimeQueueItem.Obs, now: Timestamp): Option[TimeSpan] =
    item.startedAt
      .filter(_ => item.status === QueueItemStatus.Running)
      .map(startedAt => if now > startedAt then TimeSpan.between(startedAt, now) else TimeSpan.Zero)

  /** Time left to a Running observation: its estimate minus its elapsed time, never negative. */
  def remaining(item: DaytimeQueueItem.Obs, now: Timestamp): TimeSpan =
    item.estimate -| elapsed(item, now).getOrElse(TimeSpan.Zero)

  /** Time left to each Running observation, as `QueueDispatcher.predictRemaining` takes it. */
  def runningRemaining(queue: DaytimeQueue, now: Timestamp): Map[Observation.Id, TimeSpan] =
    queue.obsItems
      .filter(_.status === QueueItemStatus.Running)
      .map(item => item.obsId -> remaining(item, now))
      .toMap

  /** Time already spent by every observation: its total minus what it has left. */
  def doneTime(queue: DaytimeQueue, now: Timestamp): TimeSpan =
    queue.obsItems.foldMap(item => item.total -| remaining(item, now))

  private def isObs(item: DaytimeQueueItem): Boolean = item match
    case DaytimeQueueItem.Obs(_, _, _, _, _, _, _, _, _) => true
    case DaytimeQueueItem.Pause(_)                       => false

  private def isRunning(item: DaytimeQueueItem): Boolean = item match
    case DaytimeQueueItem.Obs(status = status) => status === QueueItemStatus.Running
    case DaytimeQueueItem.Pause(_)             => false

  /**
   * Where a new pause goes: the first place with an observation on each side that is below every
   * Running observation, if there is one.
   */
  def newPauseIndex(queue: DaytimeQueue): Option[Int] =
    val firstAllowed: Int = queue.items.lastIndexWhere(isRunning) + 1
    queue.items
      .sliding(2)
      .zipWithIndex
      .collectFirst:
        case (List(above, below), index)
            if index + 1 >= firstAllowed && isObs(above) && isObs(below) =>
          index + 1

  /** There is something to predict: an observation Running, or Waiting above the first pause. */
  def isPredictable(queue: DaytimeQueue): Boolean =
    hasRunning(queue) ||
      queue.obsAbovePause.exists(_.status === QueueItemStatus.Waiting)

  def prediction(queue: DaytimeQueue, now: Timestamp): Option[DaytimeQueuePrediction] =
    Option.when(isPredictable(queue)):
      val remainingTime: TimeSpan =
        QueueDispatcher.predictRemaining(queue, runningRemaining(queue, now))
      DaytimeQueuePrediction(
        remainingTime,
        fraction(doneTime(queue, now), remainingTime),
        now +| remainingTime
      )

  /** Time until the GCAL lamps are turned off, while that countdown is on. */
  def lampsOffIn(queue: DaytimeQueue, now: Timestamp): Option[TimeSpan] =
    queue.lampsOffAt.map: lampsOffAt =>
      if lampsOffAt > now then TimeSpan.between(now, lampsOffAt) else TimeSpan.Zero

  def hasRunning(queue: DaytimeQueue): Boolean =
    queue.obsItems.exists(_.status === QueueItemStatus.Running)

  def hasCompleted(queue: DaytimeQueue): Boolean =
    queue.obsItems.exists(_.status === QueueItemStatus.Completed)

  /**
   * Run Queue has something to do: the dispatcher is not active, or stopped observations wait for
   * it.
   */
  def canRunQueue(queue: DaytimeQueue): Boolean =
    queue.obsItems.nonEmpty &&
      (queue.dispatchState =!= QueueDispatchState.Running ||
        queue.obsItems.exists(_.status === QueueItemStatus.Stopped))

  /** Halting has an effect while the dispatcher is active or something is Running. */
  def canHalt(queue: DaytimeQueue): Boolean =
    queue.dispatchState === QueueDispatchState.Running || hasRunning(queue)

  private def sorted(subsystems: Set[Subsystem]): List[Subsystem] =
    Enumerated[Subsystem].all.filter(subsystems.contains)

  def note(view: DaytimeQueueView, item: DaytimeQueueItem.Obs): Option[DaytimeQueueNote] =
    val queue: DaytimeQueue = view.queue
    item.status match
      case QueueItemStatus.Failed                                                        =>
        item.errorMsg.map(DaytimeQueueNote.Failed(_))
      case QueueItemStatus.Waiting if queue.dispatchState === QueueDispatchState.Running =>
        val blockedBy: Set[Subsystem] = view.obsBlockedBy(item.obsId)
        if blockedBy.nonEmpty then DaytimeQueueNote.BlockedBy(sorted(blockedBy)).some
        else if !queue.obsAbovePause.exists(_.obsId === item.obsId) then
          DaytimeQueueNote.BelowPause.some
        else if !QueueDispatcher.runnable(queue, view.busySubsystems).contains(item.obsId) then
          DaytimeQueueNote.WaitingForTurn.some
        else none
      case _                                                                             => none

  /** Shown while it runs, and afterwards if it stopped part way; nothing once Completed. */
  def progress(item: DaytimeQueueItem.Obs, now: Timestamp): Option[DaytimeQueueProgress] =
    val done: DaytimeQueueProgress =
      DaytimeQueueProgress.Done(item.total -| remaining(item, now), item.total)
    item.status match
      case QueueItemStatus.Completed       => none
      case QueueItemStatus.Running         =>
        item.startedAt.fold(DaytimeQueueProgress.Indeterminate)(_ => done).some
      case _ if item.estimate < item.total => done.some
      case _                               => none

  /** The rows of the queue table, in queue order. */
  def rows(
    view:      DaytimeQueueView,
    summaries: Map[Observation.Id, ObsSummary],
    now:       Timestamp
  ): List[DaytimeQueueRow] =
    val firstPause: Option[QueuePauseId] =
      view.queue.items.collectFirst { case DaytimeQueueItem.Pause(id) => id }

    view.queue.items.map:
      case DaytimeQueueItem.Pause(id)                             =>
        DaytimeQueueRow.Pause(id, firstPause.contains_(id))
      case item @ DaytimeQueueItem.Obs(_, _, _, _, _, _, _, _, _) =>
        DaytimeQueueRow.Obs(
          DaytimeQueueObsRow(
            item,
            summaries.get(item.obsId),
            sorted(view.obsBlockedBy(item.obsId)),
            note(view, item),
            progress(item, now)
          )
        )

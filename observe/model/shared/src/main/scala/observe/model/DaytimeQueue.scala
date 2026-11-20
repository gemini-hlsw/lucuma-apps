// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.Encoder
import lucuma.core.enums.Instrument
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.odb.json.time.transport.given
import monocle.Focus
import monocle.Lens
import observe.model.enums.QueueDispatchState
import observe.model.enums.QueueItemStatus
import observe.model.enums.QueueOrderingMode

enum QueueItemId derives Eq, Encoder.AsObject, Decoder:
  case Obs(obsId: Observation.Id)
  case Pause(pauseId: QueuePauseId)

enum DaytimeQueueItem derives Eq, Encoder.AsObject, Decoder:
  /**
   * An observation. `subsystems` are all the subsystems it claims while running, its instrument
   * included. `estimate` is the time its remaining steps take, as of `startedAt` once the queue has
   * started it.
   */
  case Obs(
    obsId:          Observation.Id,
    instrument:     Instrument,
    status:         QueueItemStatus,
    subsystems:     Set[Subsystem],
    estimate:       TimeSpan,
    errorMsg:       Option[String],
    abortRequested: Boolean,
    startedAt:      Option[Timestamp]
  )

  /** A barrier: nothing below it is dispatched until it is removed. */
  case Pause(id: QueuePauseId)

  def itemId: QueueItemId = this match
    case Obs(obsId = id) => QueueItemId.Obs(id)
    case Pause(id = id)  => QueueItemId.Pause(id)

extension (o: DaytimeQueueItem.Obs)
  /** A Running observation claims its subsystems. */
  def isRunning: Boolean = o.status === QueueItemStatus.Running

case class DaytimeQueue(
  items:         List[DaytimeQueueItem],
  orderingMode:  QueueOrderingMode,
  dispatchState: QueueDispatchState,
  lampsOffError: Option[String],
  lampsOffAt:    Option[Timestamp]
) derives Eq,
      Encoder.AsObject,
      Decoder:

  def obsItems: List[DaytimeQueueItem.Obs] =
    items.collect { case o @ DaytimeQueueItem.Obs(_, _, _, _, _, _, _, _) => o }

  def obs(obsId: Observation.Id): Option[DaytimeQueueItem.Obs] =
    obsItems.find(_.obsId === obsId)

  def contains(obsId: Observation.Id): Boolean =
    obs(obsId).isDefined

  private def isRunning(item: DaytimeQueueItem): Boolean = item match
    case DaytimeQueueItem.Obs(status = s) => s === QueueItemStatus.Running
    case _                                => false

  /** Adds an observation at the end; unchanged if the observation is already in the queue. */
  def append(item: DaytimeQueueItem.Obs): DaytimeQueue =
    if contains(item.obsId) then this else copy(items = items :+ item)

  /** Removes an item; a Running observation cannot be removed. */
  def remove(id: QueueItemId): DaytimeQueue =
    items
      .find(_.itemId === id)
      .filterNot(isRunning)
      .fold(this)(_ => copy(items = items.filterNot(_.itemId === id)))

  /**
   * Moves an item so it ends at `toIndex` (clamped) of the resulting list. A Running observation
   * cannot be moved.
   */
  def move(id: QueueItemId, toIndex: Int): DaytimeQueue =
    items.find(_.itemId === id).filterNot(isRunning).fold(this) { item =>
      val rest: List[DaytimeQueueItem] = items.filterNot(_.itemId === id)
      val idx: Int                     = toIndex.max(0).min(rest.size)
      copy(items = rest.patch(idx, List(item), 0))
    }

  /** Inserts a pause at the (clamped) index. */
  def insertPause(id: QueuePauseId, atIndex: Int): DaytimeQueue =
    val idx: Int = atIndex.max(0).min(items.size)
    copy(items = items.patch(idx, List(DaytimeQueueItem.Pause(id)), 0))

  /** Removes the top-most pause, if any. */
  def continuePastPause: DaytimeQueue =
    val idx: Int = items.indexWhere:
      case DaytimeQueueItem.Pause(_) => true
      case _                         => false
    if idx < 0 then this else copy(items = items.patch(idx, Nil, 1))

  /** Removes all completed observations; pauses stay. */
  def clearCompleted: DaytimeQueue =
    copy(items = items.filter:
      case DaytimeQueueItem.Obs(status = s) => s =!= QueueItemStatus.Completed
      case _                                => true)

  /** Failed or Aborted observation back to Waiting; anything else is unchanged. */
  def retry(obsId: Observation.Id): DaytimeQueue =
    modifyObs(obsId): o =>
      if o.status === QueueItemStatus.Failed || o.status === QueueItemStatus.Aborted then
        o.copy(status = QueueItemStatus.Waiting, errorMsg = none, abortRequested = false)
      else o

  /** Every Stopped observation back to Waiting. */
  def resetStopped: DaytimeQueue =
    copy(items = items.map:
      case o @ DaytimeQueueItem.Obs(status = QueueItemStatus.Stopped) =>
        o.copy(status = QueueItemStatus.Waiting)
      case other                                                      => other)

  def modifyObs(obsId: Observation.Id)(
    f: DaytimeQueueItem.Obs => DaytimeQueueItem.Obs
  ): DaytimeQueue =
    copy(items = items.map:
      case o @ DaytimeQueueItem.Obs(obsId = id) if id === obsId => f(o)
      case other                                                => other)

  /** Subsystems claimed by the Running observations, optionally leaving one out. */
  def runningSubsystems(exclude: Option[Observation.Id] = none): Set[Subsystem] =
    obsItems
      .filter(o => o.isRunning && !exclude.contains(o.obsId))
      .foldMap(_.subsystems)

  /** Observations above the first pause, the only ones the dispatcher considers. */
  def obsAbovePause: List[DaytimeQueueItem.Obs] =
    items
      .takeWhile:
        case DaytimeQueueItem.Pause(_) => false
        case _                         => true
      .collect { case o @ DaytimeQueueItem.Obs(_, _, _, _, _, _, _, _) => o }

  /** True when no observation is Running, Waiting or Stopped. */
  def isFinished: Boolean =
    obsItems.forall: o =>
      !o.isRunning && o.status =!= QueueItemStatus.Waiting &&
        o.status =!= QueueItemStatus.Stopped

object DaytimeQueue:
  val Empty: DaytimeQueue =
    DaytimeQueue(Nil, QueueOrderingMode.Default, QueueDispatchState.Idle, none, none)

  val items: Lens[DaytimeQueue, List[DaytimeQueueItem]]     = Focus[DaytimeQueue](_.items)
  val orderingMode: Lens[DaytimeQueue, QueueOrderingMode]   = Focus[DaytimeQueue](_.orderingMode)
  val dispatchState: Lens[DaytimeQueue, QueueDispatchState] = Focus[DaytimeQueue](_.dispatchState)
  val lampsOffError: Lens[DaytimeQueue, Option[String]]     = Focus[DaytimeQueue](_.lampsOffError)

  /** When the GCAL lamps will be turned off, unless the countdown is cancelled before. */
  val lampsOffAt: Lens[DaytimeQueue, Option[Timestamp]] = Focus[DaytimeQueue](_.lampsOffAt)

case class DaytimeQueueView(queue: DaytimeQueue, busySubsystems: Set[Subsystem])
    derives Eq,
      Encoder.AsObject,
      Decoder:

  /** Busy subsystems this observation needs; empty when it is itself Running. */
  def obsBlockedBy(obsId: Observation.Id): Set[Subsystem] =
    queue.obs(obsId) match
      case Some(o) if o.isRunning                     => Set.empty
      case Some(DaytimeQueueItem.Obs(subsystems = s)) => s.intersect(busySubsystems)
      case None                                       => Set.empty

object DaytimeQueueView:
  val Empty: DaytimeQueueView = DaytimeQueueView(DaytimeQueue.Empty, Set.empty)

  val queue: Lens[DaytimeQueueView, DaytimeQueue]            = Focus[DaytimeQueueView](_.queue)
  val busySubsystems: Lens[DaytimeQueueView, Set[Subsystem]] =
    Focus[DaytimeQueueView](_.busySubsystems)

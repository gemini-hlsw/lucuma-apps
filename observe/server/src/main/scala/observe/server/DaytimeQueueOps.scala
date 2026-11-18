// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import cats.syntax.all.*
import lucuma.core.enums.Instrument
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import observe.model.DaytimeQueue
import observe.model.DaytimeQueueItem
import observe.model.Observation
import observe.model.SequenceStatus
import observe.model.Subsystem
import observe.model.enums.ObservatorySubsystem
import observe.model.enums.QueueDispatchState
import observe.model.enums.QueueItemStatus
import observe.server.odb.OdbProxy
import observe.server.odb.OdbQueueObservation

/** What changed in the daytime queue between two of its states. */
case class QueueSyncResult(
  becameFinished:    Boolean,
  completedOrFailed: Boolean,
  aborted:           Boolean
)

object QueueSyncResult:
  def between(before: DaytimeQueue, after: DaytimeQueue): QueueSyncResult =
    val reached: List[QueueItemStatus] =
      after.obsItems.flatMap: o =>
        before.obs(o.obsId).filter(_.status =!= o.status).as(o.status).toList
    QueueSyncResult(
      becameFinished = !before.isFinished && after.isFinished,
      completedOrFailed =
        reached.exists(s => s === QueueItemStatus.Completed || s === QueueItemStatus.Failed),
      aborted = reached.contains_(QueueItemStatus.Aborted)
    )

/** What a daytime queue observation holds while it runs, and how long it is expected to take. */
case class QueueItemResources(
  instrument: Instrument,
  subsystems: Set[Subsystem],
  estimate:   TimeSpan
)

/** Pure logic of the daytime queue on the engine state. */
object DaytimeQueueOps:

  /**
   * Status of a queue observation given the status of its loaded sequence (None if not loaded). A
   * Running observation follows its sequence; any other observation becomes Running when its
   * sequence is started outside the queue.
   */
  def nextItemStatus(
    item:      DaytimeQueueItem.Obs,
    seqStatus: Option[SequenceStatus],
    now:       Timestamp
  ): DaytimeQueueItem.Obs =
    // Started outside the queue: it runs on from what the queue last knew it had left.
    val running: DaytimeQueueItem.Obs                        =
      item.copy(
        status = QueueItemStatus.Running,
        errorMsg = none,
        abortRequested = false,
        startedAt = now.some
      )
    // It no longer runs: what it had left is kept, so its progress stays where it got to.
    def ended(status: QueueItemStatus): DaytimeQueueItem.Obs =
      item.copy(status = status, estimate = left(item, now), startedAt = none)
    item.status match
      case QueueItemStatus.Running =>
        seqStatus match
          case None                             => ended(QueueItemStatus.Stopped)
          case Some(SequenceStatus.Completed)   =>
            ended(QueueItemStatus.Completed).copy(estimate = TimeSpan.Zero)
          case Some(SequenceStatus.Failed(msg)) =>
            ended(QueueItemStatus.Failed).copy(errorMsg = msg.some)
          case Some(SequenceStatus.Aborted)     => ended(QueueItemStatus.Aborted)
          case Some(s) if s.isRunning           => item
          case Some(_) if item.abortRequested   => ended(QueueItemStatus.Aborted)
          case Some(_)                          => ended(QueueItemStatus.Stopped)
      case _                       =>
        if seqStatus.exists(_.isRunning) then running else item

  /** The estimate of an observation minus the time the queue has been running it. */
  def left(item: DaytimeQueueItem.Obs, now: Timestamp): TimeSpan =
    item.startedAt.fold(item.estimate): startedAt =>
      if now > startedAt then item.estimate -| TimeSpan.between(startedAt, now) else item.estimate

  /**
   * The outcome of a start the queue asked for, which the engine decides in the same step. A
   * refused start leaves the observation Waiting, to be tried again later. A started one holds what
   * its sequence was given, from `now`.
   */
  def afterStart[F[_]](obsId: Observation.Id, now: Timestamp)(st: EngineState[F]): EngineState[F] =
    val outcome: DaytimeQueueItem.Obs => DaytimeQueueItem.Obs =
      item =>
        st.sequences
          .get(obsId)
          .fold(item): seqData =>
            seqData.seq.status match
              case s if s.isRunning           =>
                item.copy(
                  status = QueueItemStatus.Running,
                  subsystems = Set[Subsystem](item.instrument) ++ seqData.sequenceSubsystems,
                  errorMsg = none,
                  abortRequested = false,
                  startedAt = now.some
                )
              case SequenceStatus.Completed   => item.copy(status = QueueItemStatus.Completed)
              case SequenceStatus.Failed(msg) =>
                item.copy(status = QueueItemStatus.Failed, errorMsg = msg.some)
              case _                          => item
    EngineState.daytimeQueue[F].modify(_.modifyObs(obsId)(outcome))(st)

  /**
   * Brings the queue observations in line with their loaded sequences. A Running queue whose
   * observations are all done goes back to Idle.
   */
  def sync[F[_]](now: Timestamp)(st: EngineState[F]): (EngineState[F], QueueSyncResult) =
    val before: DaytimeQueue          = st.daytimeQueue
    val synced: DaytimeQueue          =
      before.copy(items = before.items.map:
        case o @ DaytimeQueueItem.Obs(obsId = obsId) =>
          nextItemStatus(o, st.sequences.get(obsId).map(_.seq.status), now)
        case other                                   => other)
    val done: Boolean                 =
      synced.isFinished && synced.dispatchState === QueueDispatchState.Running
    // The lamps are needed again: the countdown to turn them off is dropped.
    val lampsOffAt: Option[Timestamp] =
      synced.lampsOffAt.filter(_ => synced.isFinished && !gcalNeeded(st))
    val after: DaytimeQueue           =
      synced.copy(
        dispatchState = if done then QueueDispatchState.Idle else synced.dispatchState,
        lampsOffAt = lampsOffAt
      )
    val newState: EngineState[F]      =
      st.copy(
        daytimeQueue = after,
        daytimeQueueObserver = if done then none else st.daytimeQueueObserver
      )
    (newState, QueueSyncResult.between(before, after))

  /**
   * The countdown to turn the GCAL lamps off starts when the queue becomes finished because an
   * observation completed or failed, and none was aborted.
   */
  def shouldTurnLampsOff(result: QueueSyncResult): Boolean =
    result.becameFinished && result.completedOrFailed && !result.aborted

  /**
   * A running sequence holds GCAL, or GCAL is being configured by hand. The actions a failed step
   * left running do not count: they are ending, not using the lamps.
   */
  def gcalNeeded[F[_]](st: EngineState[F]): Boolean =
    ObserveEngine.subsystemsBeingConfigured(st).contains(ObservatorySubsystem.Gcal) ||
      st.sequences.values.exists: s =>
        s.seq.status.isRunning &&
          (s.sequenceSubsystems ++ s.stepSubsystems).contains(ObservatorySubsystem.Gcal)

  private val SupportedInstruments: Set[Instrument] =
    Set(
      Instrument.GmosSouth,
      Instrument.GmosNorth,
      Instrument.Ghost,
      Instrument.Flamingos2,
      Instrument.Igrins2,
      Instrument.Gnirs
    )

  /**
   * The instrument of an observation the queue accepts, or why it does not. Its subsystems and time
   * estimate come from its remaining steps, which are read knowing the instrument.
   */
  def accepted(obs: OdbQueueObservation): Either[String, Instrument] =
    for
      _          <- Either.cond(obs.isDaytime, (), "It is not a daytime calibration")
      instrument <- obs.observingMode
                      .flatMap(_.instrument)
                      .filter(SupportedInstruments.contains)
                      .toRight("It has no observing mode of an instrument Observe supports")
    yield instrument

  /** What the remaining steps of an observation say it holds and how long it takes. */
  def resources(instrument: Instrument, remaining: OdbProxy.RemainingSteps): QueueItemResources =
    QueueItemResources(
      instrument,
      SequenceSubsystems.fromRemainingSteps(instrument, remaining),
      remaining.estimate
    )

  /** A new Waiting queue observation. */
  def waitingItem(obsId: Observation.Id, resources: QueueItemResources): DaytimeQueueItem.Obs =
    DaytimeQueueItem.Obs(
      obsId,
      resources.instrument,
      QueueItemStatus.Waiting,
      resources.subsystems,
      resources.estimate,
      none,
      abortRequested = false,
      startedAt = none,
      total = resources.estimate
    )

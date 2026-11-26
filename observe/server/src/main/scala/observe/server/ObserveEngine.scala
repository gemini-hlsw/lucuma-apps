// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import cats.Endo
import cats.Monad
import cats.data.NonEmptyList
import cats.effect.Async
import cats.effect.MonadCancelThrow
import cats.effect.Ref
import cats.effect.Sync
import cats.effect.Temporal
import cats.effect.syntax.all.*
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import fs2.Pipe
import fs2.Stream
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.ExecutionEnvironment
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.enums.Site
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ImageQuality
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.model.sequence.Step
import lucuma.core.util.Enumerated
import monocle.Lens
import mouse.all.*
import observe.model.*
import observe.model.config.*
import observe.model.enums.ObservatorySubsystem
import observe.model.enums.QueueDispatchState
import observe.model.enums.QueueOrderingMode
import observe.model.enums.RunOverride
import observe.model.given
import observe.server.engine.*
import observe.server.engine.Event
import observe.server.engine.Handle.given
import observe.server.events.*
import observe.server.odb.OdbObservationData
import observe.server.odb.OdbProxy
import org.typelevel.log4cats.Logger
import org.typelevel.otel4s.trace.Tracer

import scala.concurrent.duration.*

// import SeqEvent.*

trait ObserveEngine[F[_]] {

  val services: Services[F]

  def startSequence(
    obsId:       Observation.Id,
    user:        User,
    observer:    Observer,
    clientId:    ClientId,
    runOverride: RunOverride
  ): F[Unit]

  def proceedAfterPrompt(
    obsId:       Observation.Id,
    user:        User,
    observer:    Observer,
    seqType:     SequenceType,
    clientId:    ClientId,
    runOverride: RunOverride
  ): F[Unit]

  /**
   * While the sequence is idle (or in error), load the first science step. Nothing is started.
   */
  def skipAcquisition(
    obsId:    Observation.Id,
    user:     User,
    observer: Observer,
    clientId: ClientId
  ): F[Unit]

  /**
   * While the sequence is idle (or in error), reset the acquisition in the ODB and load its first
   * step. Nothing is started.
   */
  def resetAcquisition(
    obsId:    Observation.Id,
    user:     User,
    observer: Observer,
    clientId: ClientId
  ): F[Unit]

  /**
   * After the `sequenceType` sequence has been replaced in the ODB (all its remaining steps got new
   * ids), take that sequence's breakpoints from the ODB, whatever the status. An idle loaded step
   * of that type is re-read from the ODB; no step is loaded and nothing is started.
   */
  def reloadAfterEdit(
    obsId:        Observation.Id,
    user:         User,
    observer:     Observer,
    sequenceType: SequenceType,
    clientId:     ClientId
  ): F[Unit]

  def requestSequenceHold(
    obsId:    Observation.Id,
    observer: Observer,
    user:     User
  ): F[Unit]

  def cancelSequenceHoldRequest(
    obsId:    Observation.Id,
    observer: Observer,
    user:     User
  ): F[Unit]

  def rewindStep(
    obsId:    Observation.Id,
    observer: Observer,
    user:     User
  ): F[Unit]

  def cancelRewindRequest(
    obsId:    Observation.Id,
    observer: Observer,
    user:     User
  ): F[Unit]

  def setBreakpoints(
    obsId:    Observation.Id,
    user:     User,
    observer: Observer,
    stepId:   Set[Step.Id],
    v:        Breakpoint
  ): F[Unit]

  def setOperator(user: User, name: Operator): F[Unit]

  def setObserver(
    obsId: Observation.Id,
    user:  User,
    name:  Observer
  ): F[Unit]

  // Subsystem overrides
  def setTcsEnabled(
    obsId:    Observation.Id,
    user:     User,
    enabled:  SubsystemEnabled,
    clientId: ClientId
  ): F[Unit]

  def setGcalEnabled(
    obsId:    Observation.Id,
    user:     User,
    enabled:  SubsystemEnabled,
    clientId: ClientId
  ): F[Unit]

  def setInstrumentEnabled(
    obsId:    Observation.Id,
    user:     User,
    enabled:  SubsystemEnabled,
    clientId: ClientId
  ): F[Unit]

  def setDhsEnabled(
    obsId:    Observation.Id,
    user:     User,
    enabled:  SubsystemEnabled,
    clientId: ClientId
  ): F[Unit]

  def loadSequence(
    i:        Instrument,
    obsId:    Observation.Id,
    observer: Observer,
    user:     User,
    clientId: ClientId
  ): F[Unit]

  /** Empties the instrument slot of a loaded observation unless it is running. */
  def unloadSequence(obsId: Observation.Id, user: User, clientId: ClientId): F[Unit]

  def clearLoadedSequences(user: User): F[Unit]

  def resetConditions: F[Unit]

  def setConditions(conditions: CurrentConditions, user: User): F[Unit]

  def setImageQuality(iq: ImageQuality, user: User, clientId: ClientId): F[Unit]

  def setWaterVapor(wv: WaterVapor, user: User, clientId: ClientId): F[Unit]

  def setSkyBackground(sb: SkyBackground, user: User, clientId: ClientId): F[Unit]

  def setCloudExtinction(cc: CloudExtinction, user: User, clientId: ClientId): F[Unit]

  def requestRefresh(clientId: ClientId): F[Unit]

  def stopExposure(
    obsId:    Observation.Id,
    observer: Observer,
    user:     User,
    graceful: Boolean
  ): F[Unit]

  def abortExposure(
    obsId:    Observation.Id,
    observer: Observer,
    user:     User
  ): F[Unit]

  def pauseExposure(
    obsId:    Observation.Id,
    observer: Observer,
    user:     User,
    graceful: Boolean
  ): F[Unit]

  def resumeExposure(
    obsId:    Observation.Id,
    observer: Observer,
    user:     User
  ): F[Unit]

  /**
   * Reads `obsId` from the ODB and appends it to the daytime queue as Waiting. Rejected with a
   * `DaytimeQueueRejected` notification if it is already in the queue or is not a daytime
   * calibration. Nothing is loaded into an instrument slot.
   */
  def addToDaytimeQueue(obsId: Observation.Id, user: User, clientId: ClientId): F[Unit]

  /** Removes an observation or a pause; a Running observation stays. */
  def removeFromDaytimeQueue(itemId: QueueItemId, user: User, clientId: ClientId): F[Unit]

  /** Moves an observation or a pause to `toIndex`; a Running observation stays. */
  def moveInDaytimeQueue(
    itemId:   QueueItemId,
    toIndex:  Int,
    user:     User,
    clientId: ClientId
  ): F[Unit]

  /** Inserts a pause at `atIndex`; nothing below it is dispatched until it is removed. */
  def addDaytimeQueuePause(atIndex: Int, user: User, clientId: ClientId): F[Unit]

  def setDaytimeQueueOrderingMode(
    mode:     QueueOrderingMode,
    user:     User,
    clientId: ClientId
  ): F[Unit]

  /** Removes the Completed observations. */
  def clearCompletedDaytimeQueue(user: User, clientId: ClientId): F[Unit]

  /** Cancels the countdown to turn the GCAL lamps off, which are then left as they are. */
  def cancelDaytimeQueueLampsOff(user: User, clientId: ClientId): F[Unit]

  /** Puts a Failed or Aborted observation back to Waiting. */
  def retryDaytimeQueueItem(obsId: Observation.Id, user: User, clientId: ClientId): F[Unit]

  /**
   * Runs the queue: Stopped observations go back to Waiting and Waiting ones are started, on behalf
   * of `observer`, as their subsystems become available.
   */
  def runDaytimeQueue(observer: Observer, user: User, clientId: ClientId): F[Unit]

  /** Halts the queue and holds every running observation after its current step. */
  def holdDaytimeQueueAfterCurrentSteps(user: User, clientId: ClientId): F[Unit]

  /** Halts the queue, letting the running observations finish. */
  def holdDaytimeQueueAfterCurrentObservations(user: User, clientId: ClientId): F[Unit]

  /** Halts the queue and aborts every running observation. */
  def abortDaytimeQueue(observer: Observer, user: User, clientId: ClientId): F[Unit]

  /**
   * Starts one observation of the queue now, whatever its position, the pauses or the queue state,
   * if its subsystems are free; nothing else is started.
   */
  def runDaytimeQueueItem(
    obsId:    Observation.Id,
    observer: Observer,
    user:     User,
    clientId: ClientId
  ): F[Unit]

  /** Aborts a running observation of the queue; it ends Aborted. */
  def abortDaytimeQueueItem(
    obsId:    Observation.Id,
    observer: Observer,
    user:     User,
    clientId: ClientId
  ): F[Unit]

  /**
   * Triggers the application of a specific step configuration to a system
   */
  def configureSubsystem(
    obsId:     Observation.Id,
    observer:  Observer,
    user:      User,
    stepId:    Step.Id,
    subsystem: Subsystem,
    clientID:  ClientId
  ): F[Unit]

  /**
   * Requests a server restart. `shutdown` is run, inside the engine, only if nothing is running or
   * busy; otherwise the client gets a `RestartRefused` notification and nothing happens.
   */
  def requestRestart(user: User, clientId: ClientId, shutdown: F[Unit]): F[Unit]

  def clientEventStream: Stream[F, TargetedClientEvent]

  // Used by tests
  private[server] def eventResultStream(
    s0: EngineState[F]
  ): Stream[F, (EventResult, EngineState[F])]

  private[server] def loadSequenceMod(
    observer:               Option[Observer],
    odbData:                OdbObservationData,
    instrumentSequenceLens: Lens[EngineState[F], Option[SequenceData[F]]]
  ): Endo[EngineState[F]] =
    ODBSequencesLoader.loadSequenceMod(observer, odbData, instrumentSequenceLens)
}

object ObserveEngine {

  def createTranslator[F[_]: {Async, Logger}](
    site:          Site,
    services:      Services[F],
    conditionsRef: Ref[F, CurrentConditions],
    environment:   ExecutionEnvironment
  ): F[SeqTranslate[F]] =
    SeqTranslate(site, services, conditionsRef, environment)

  private def observations[F[_]](st: EngineState[F]): List[SequenceData[F]] =
    List(
      st.selected.gmosSouth,
      st.selected.gmosNorth,
      st.selected.flamingos2,
      st.selected.igrins2,
      st.selected.gnirs,
      st.selected.ghost
    ).flattenOption

  private[server] def subsystemsBeingConfigured[F[_]](st: EngineState[F]): Set[Subsystem] =
    observations(st)
      .filter(d => d.seq.status.isError || d.seq.status.isIdle)
      .flatMap(s =>
        s.seq.getSingleActionStates
          .filter(_._2.started)
          .keys
          .toList
          .mapFilter(s.subsystemAtCoords)
      )
      .toSet

  /** A failed step's other actions run on until they finish. */
  def hasActionsInFlight[F[_]](s: SequenceData[F]): Boolean =
    s.seq.currentExecution.execution.exists(_.state.runState.started)

  /** A manual configuration of one of its subsystems is still running. */
  def isConfiguring[F[_]](s: SequenceData[F]): Boolean =
    s.seq.getSingleActionStates.exists(_._2.started)

  /** Replacing or removing its state now would forget an action that is still running. */
  def isBusy[F[_]](s: SequenceData[F]): Boolean =
    hasActionsInFlight(s) || isConfiguring(s)

  /** A sequence holds its subsystems while it runs and while actions of a failed step run on. */
  private def isHolding[F[_]](s: SequenceData[F]): Boolean =
    s.seq.status.isRunning || hasActionsInFlight(s)

  /** What a running sequence holds, also while it waits at the user prompt. */
  private def heldSubsystems[F[_]](s: SequenceData[F]): Set[Subsystem] =
    s.sequenceSubsystems ++ s.stepSubsystems

  /**
   * Subsystems in use = Subsystems held by running sequences (their hold plus their loaded step),
   * plus the subsystems that are being configured because a user commanded a manual configuration
   * apply.
   */
  def subsystemsInUse[F[_]](
    st:         EngineState[F],
    excludeObs: Option[Observation.Id] = none
  ): Set[Subsystem] =
    observations(st)
      .filter(s => !excludeObs.contains_(s.obsId) && isHolding(s))
      .foldMap(heldSubsystems(_)) ++
      subsystemsBeingConfigured(st)

  /** The running observations, other than `obsId`, holding any of `subsystems`. */
  def heldByOther[F[_]](
    st:         EngineState[F],
    obsId:      Observation.Id,
    subsystems: Set[Subsystem]
  ): List[Observation.Id] =
    observations(st)
      .filter: s =>
        s.obsId =!= obsId && isHolding(s) && heldSubsystems(s).intersect(subsystems).nonEmpty
      .map(_.obsId)

  /** Subsystems the loaded step of `obsId` needs that other running sequences hold. */
  def stepClash[F[_]](st: EngineState[F], obsId: Observation.Id): Set[Subsystem] =
    st.sequences
      .get(obsId)
      .foldMap(_.stepSubsystems)
      .intersect(subsystemsInUse(st, excludeObs = obsId.some))

  /**
   * Runs `start` unless the loaded step of `obsId` clashes with what others hold. On a clash the
   * sequence is held with its step still loaded and the clash is reported; otherwise the step's
   * subsystems are added to the hold.
   */
  def unlessStepClash[F[_]: {MonadCancelThrow, Logger}](obsId: Observation.Id)(
    start: EngineHandle[F, SeqEvent]
  ): EngineHandle[F, SeqEvent] =
    EngineHandle
      .getState[F]
      .flatMap: st =>
        val clash: Set[Subsystem] = stepClash(st, obsId)
        st.sequences.get(obsId).flatMap(_.loadedStep) match
          case None                        => start
          case Some(step) if clash.isEmpty =>
            EngineHandle.modifyState_[F](
              EngineState
                .atSequence[F](obsId)
                .modify(SequenceData.sequenceSubsystems[F].modify(_ ++ step.subsystems))
            ) >> start
          case Some(step)                  =>
            // The clients show the toast from the event; this is the line in their log panel.
            EngineHandle.liftF(
              Logger[F].warn(
                s"Observation $obsId was held before step ${step.id}: " +
                  s"${clash.toList.map(Enumerated[Subsystem].tag).mkString(", ")} in use by another " +
                  "sequence. Start it again when they are free."
              )
            ) >>
              modifySequenceStatus(obsId)(_ => SequenceStatus.Idle) >>
              EngineHandle.fromSingleEvent(Event.sequenceHeld(obsId)) >>
              EngineHandle.pure(SeqEvent.SubsystemClash(obsId, step.id, clash))

  /**
   * Subsystems that nothing else may start on: those held by running sequences, those of the
   * Running daytime queue observations, and GCAL while the queue is turning its lamps off.
   */
  def busySubsystems[F[_]](
    st:         EngineState[F],
    excludeObs: Option[Observation.Id] = none
  ): Set[Subsystem] =
    val lampsOff: Set[Subsystem] =
      if st.daytimeQueueLampsOff then Set(ObservatorySubsystem.Gcal) else Set.empty
    subsystemsInUse(st, excludeObs) ++ st.daytimeQueue.runningSubsystems(excludeObs) ++ lampsOff

  /**
   * Human readable reasons why the server cannot be restarted right now. None if nothing is running
   * or being configured and the daytime queue is not running.
   */
  def restartBlockers[F[_]](st: EngineState[F]): Option[NonEmptyList[String]] =
    val sequences: List[String]   =
      observations(st).filter(_.seq.status.isRunning).map(s => s"Sequence ${s.obsId} is running")
    val configuring: List[String] =
      subsystemsBeingConfigured(st).toList.map: subsystem =>
        s"${Enumerated[Subsystem].tag(subsystem)} is being configured"
    val queue: List[String]       =
      if st.daytimeQueue.dispatchState === QueueDispatchState.Running then
        List("The daytime queue is running")
      else Nil
    NonEmptyList
      .fromList(sequences ++ configuring ++ queue)
      .orElse:
        // The actions a failed step left running do not count: restarting is the way out of one
        // that never returns.
        Option.when(st.daytimeQueue.runningSubsystems().nonEmpty || st.daytimeQueueLampsOff):
          NonEmptyList.one("The server is busy")

  /**
   * Creates a stream that will follow a heartbeat and raise an error if the heartbeat doesn't get
   * emitted for timeout
   *
   * Credit: Fabio Labella
   * https://gitter.im/functional-streams-for-scala/fs2?at=5e0a6efbfd580457e79aaf0a
   */
  def failIfNoEmitsWithin[F[_]: Async, A](
    timeout: FiniteDuration,
    msg:     String
  ): Pipe[F, A, A] = in => {
    import scala.concurrent.TimeoutException
    def now = Temporal[F].realTime

    Stream.eval(now.flatMap(Ref[F].of)).flatMap { lastActivityAt =>
      in.evalTap(_ => now.flatMap(lastActivityAt.set))
        .concurrently {
          Stream.repeatEval {
            (now, lastActivityAt.get)
              .mapN(_ - _)
              .flatMap { elapsed =>
                val t = timeout - elapsed

                Sync[F]
                  .raiseError[Unit](new TimeoutException(msg))
                  .whenA(t <= 0.nanos) >> Temporal[F].sleep(t)
              }
          }
        }
    }
  }

  private def modifySequenceStatus[F[_]: Monad](
    obsId: Observation.Id
  )(f: SequenceStatus => SequenceStatus): EngineHandle[F, Unit] =
    EngineHandle.modifySequenceState(obsId):
      SequenceState.status.modify(f)

  def onStepComplete[F[_]: {MonadCancelThrow, Logger}](
    odb:                 OdbProxy[F],
    translator:          SeqTranslate[F]
  )(
    executeEngine:       Engine[F],
    obsId:               Observation.Id,
    currentSeqType:      SequenceType,
    completedStepAtomId: Atom.Id // Used to detect acquisition complete
  ): EngineHandle[F, SeqEvent] =
    // 1) Load next step
    // 2) If we are in acquisition and atom changed (or no more atoms), emit AcquisitionCompleted.
    // 3) If we are in science and no more steps, emit SequenceCompleted.
    // 4) Otherwise, continue as normal (like the tryNewStep below but without reloading)
    retrieveStep[F](odb, translator, obsId, currentSeqType.asLeft)
      .filterIn: newStepGen => // If acquisition atom completed, pause for prompt
        currentSeqType === SequenceType.Science || newStepGen.atomId === completedStepAtomId
      .flatMap:
        case None             => // Acquisition Complete or Sequence Complete
          currentSeqType match
            case SequenceType.Acquisition => // For acquisition, signal completion and wait for user prompt.
              modifySequenceStatus(obsId)(
                _.withWaitingUserPrompt(SequenceStatus.IsWaitingUserPrompt.Yes)
              ).as(SeqEvent.AcquisitionCompleted(obsId))
            case SequenceType.Science     => // Sequence complete
              EngineHandle
                .fromSingleEvent(Event.sequenceComplete(obsId))
                .as(SeqEvent.SequenceCompleted(obsId))
        case Some(newStepGen) =>
          unlessStepClash(obsId):
            executeEngine
              .startLoadedStep(obsId)
              .as:
                SeqEvent.NewStepLoaded(
                  obsId,
                  newStepGen.sequenceType,
                  newStepGen.atomId,
                  newStepGen.id
                )

  def tryNewStep[F[_]: {MonadCancelThrow, Logger}](
    odb:           OdbProxy[F],
    translator:    SeqTranslate[F],
    executeEngine: Engine[F],
    obsId:         Observation.Id,
    seqType:       SequenceType
  ): EngineHandle[F, Unit] =
    retrieveStep[F](odb, translator, obsId, seqType.asLeft)
      .flatMap: stepGenOpt =>
        EngineHandle.fromSingleEvent:
          Event.modifyState[F]:
            stepGenOpt
              .map: stepGen =>
                // The observer explicitly requested this step at the "proceed to science?"
                // prompt, so a breakpoint on it must not stop the sequence, the same as a Run
                // click would.
                unlessStepClash(obsId):
                  modifySequenceStatus(obsId)(_.withStarting(true)) *>
                    executeEngine
                      .startLoadedStep(obsId)
                      .as:
                        SeqEvent.NewStepLoaded(
                          obsId,
                          stepGen.sequenceType,
                          stepGen.atomId,
                          stepGen.id
                        )
              .getOrElse:
                EngineHandle
                  .fromSingleEvent(Event.sequenceComplete(obsId))
                  .as(SeqEvent.SequenceCompleted(obsId))

  private[server] def updateStep[F[_]](
    obsId:      Observation.Id,
    stepGen:    Option[StepGen[F]],
    stepIdFrom: Either[SequenceType, Step.Id]
  ): Endo[EngineState[F]] =
    (st: EngineState[F]) =>
      EngineState
        .atSequence[F](obsId)
        .modify { (seqData: SequenceData[F]) =>
          // A step loaded by id comes from a single subsystem configuration run. Configuring a
          // science step is not the same as starting science, so the sequence type is kept: it
          // would otherwise switch to Science, hiding the pending acquisition in the UI and
          // making the next start pick a science step.
          val newSeqType: SequenceType =
            stepIdFrom.fold(
              _ => stepGen.map(_.sequenceType).getOrElse(seqData.seq.currentSequenceType),
              _ => seqData.seq.currentSequenceType
            )

          // Revive sequence if it was completed - or complete if no more steps
          val newStatus: SequenceStatus =
            if seqData.seq.status.isCompleted && stepGen.nonEmpty then SequenceStatus.Idle
            else if stepGen.isEmpty && seqData.seq.currentSequenceType === SequenceType.Science then
              SequenceStatus.Completed
            else seqData.seq.status

          val newSeqState: SequenceState[F] =
            SequenceState
              .init(obsId, newSeqType, seqData.seq.breakpoints, newStatus)
              .withLoadedStepGen(
                stepGen,
                seqData.overrides,
                HeaderExtraData(st.conditions, st.operator, seqData.observer)
              )

          SequenceData.seq.replace(newSeqState)(seqData)
        }(st)

  /**
   * Reads the step at `stepIdFrom` from the ODB and translates it, without touching the engine
   * state. Errors propagate to the caller.
   */
  def readStep[F[_]: MonadCancelThrow](
    odb:        OdbProxy[F],
    translator: SeqTranslate[F],
    obsId:      Observation.Id,
    stepIdFrom: Either[SequenceType, Step.Id]
  ): EngineHandle[F, Option[StepGen[F]]] =
    // Jumping to a chosen step needs the future atoms, moving on to the next one does not.
    val limit = stepIdFrom.fold(_ => OdbProxy.NextAtomOnly, _ => OdbProxy.FullFuture)
    readConfigAndStep(odb, translator, obsId, stepIdFrom, limit).map(_.flatMap(_._2))

  /**
   * Reads the execution config, with `futureLimit` atoms beyond the next one, and translates the
   * step at `stepIdFrom` within it, without touching the engine state. Errors propagate to the
   * caller. None if the observation isn't loaded.
   */
  def readConfigAndStep[F[_]: MonadCancelThrow](
    odb:         OdbProxy[F],
    translator:  SeqTranslate[F],
    obsId:       Observation.Id,
    stepIdFrom:  Either[SequenceType, Step.Id],
    futureLimit: NonNegInt
  ): EngineHandle[F, Option[(InstrumentExecutionConfig, Option[StepGen[F]])]] =
    EngineHandle
      .inspectState[F, Option[SequenceData[F]]](EngineState.sequenceDataAt(obsId).getOption)
      .flatMap:
        _.traverse: sd =>
          EngineHandle
            .liftF(odb.readExecutionConfig(obsId, sd.instrument, futureLimit))
            .map: odbEx =>
              // TODO Do something with warnings? (_1)
              (odbEx, translator.nextStep(OdbObservationData(sd.observation, odbEx), stepIdFrom)._2)

  private def loadStep[F[_]: {MonadCancelThrow, Logger}](
    odb:        OdbProxy[F],
    translator: SeqTranslate[F],
    obsId:      Observation.Id,
    stepIdFrom: Either[SequenceType, Step.Id]
  ): EngineHandle[F, Option[StepGen[F]]] =
    (for
      _       <- modifySequenceStatus(obsId)(_.withWaitingNextStep(true).withWaitingUserPrompt(false))
      stepGen <- readStep(odb, translator, obsId, stepIdFrom)
                   .guarantee(modifySequenceStatus(obsId)(_.withWaitingNextStep(false)))
    yield stepGen).handleErrorWith: e =>
      failLoading(obsId, s"Error loading step for observation [$obsId] from [$stepIdFrom]")(e)
        .as(none)

  /** Fails the sequence after an ODB read for its next step failed. */
  private def failLoading[F[_]: {MonadCancelThrow, Logger}](
    obsId: Observation.Id,
    msg:   String
  )(e: Throwable): EngineHandle[F, Unit] =
    EngineHandle.logError(e)(msg) >>
      EngineHandle
        .modifySequenceState[F](obsId)(_.withNoLoadedStep.withFailedStatus(e.getMessage)) >>
      EngineHandle.fromSingleEvent:
        Event.loadFailed(
          obsId,
          0,
          Result.Error(s"Error updating sequence, cannot continue: ${e.getMessage}")
        )

  def retrieveStep[F[_]: {MonadCancelThrow, Logger}](
    odb:        OdbProxy[F],
    translator: SeqTranslate[F],
    obsId:      Observation.Id,
    stepIdFrom: Either[SequenceType, Step.Id]
  ): EngineHandle[F, Option[StepGen[F]]] =
    for
      _            <- EngineHandle.debug(s"Loading step for observation [$obsId] from [$stepIdFrom]")
      existingStep <- EngineHandle
                        .getSequenceState(obsId)
                        .map(_.flatMap(_.loadedStep).map(_.stepGen))
      stepGen      <- loadStep(odb, translator, obsId, stepIdFrom)
      _            <-
        if ((existingStep, stepGen).tupled.exists(_.isSameAs(_)))
          EngineHandle.debug:
            s"Step for observation [$obsId] is the same as the currently loaded one, not updating state"
        else EngineHandle.modifyState_(updateStep(obsId, stepGen, stepIdFrom))
    yield stepGen

  /**
   * Build Observe and setup epics
   */
  def build[F[_]: {Async, Logger, Tracer}](
    site:        Site,
    services:    Services[F],
    conf:        ObserveEngineConfiguration,
    environment: ExecutionEnvironment
  ): F[ObserveEngine[F]] =
    for
      rc  <- Ref.of[F, CurrentConditions](CurrentConditions.Default)
      tr  <- createTranslator(site, services, rc, environment)
      eng <- Engine.build[F](onStepComplete[F](services.odb, tr))
    yield new ObserveEngineImpl[F](eng, services, conf, tr, rc)
}

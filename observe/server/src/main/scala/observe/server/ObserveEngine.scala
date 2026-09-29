// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import cats.Endo
import cats.Monad
import cats.effect.Async
import cats.effect.MonadCancelThrow
import cats.effect.Ref
import cats.effect.Sync
import cats.effect.Temporal
import cats.effect.syntax.all.*
import cats.syntax.all.*
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
import lucuma.core.model.sequence.Step
import monocle.Lens
import mouse.all.*
import observe.model.*
import observe.model.config.*
import observe.model.enums.BatchExecState
import observe.model.enums.RunOverride
import observe.server.engine.*
import observe.server.engine.Action.ActionState
import observe.server.engine.Event
import observe.server.engine.Handle.given
import observe.server.events.*
import observe.server.odb.OdbObservationData
import observe.server.odb.OdbProxy
import org.typelevel.log4cats.Logger
import org.typelevel.otel4s.trace.Tracer

import scala.annotation.unused
import scala.concurrent.duration.*

// import SeqEvent.*

trait ObserveEngine[F[_]] {

  val systems: Systems[F]

  def start(
    obsId:       Observation.Id,
    user:        User,
    observer:    Observer,
    clientId:    ClientId,
    runOverride: RunOverride
  ): F[Unit]

  def proceedAfterPrompt(
    obsId:    Observation.Id,
    user:     User,
    observer: Observer,
    seqType:  SequenceType
  ): F[Unit]

  def requestPause(
    obsId:    Observation.Id,
    observer: Observer,
    user:     User
  ): F[Unit]

  def requestCancelPause(
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

  // Systems overrides
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

  def clearLoadedSequences(user: User): F[Unit]

  def resetConditions: F[Unit]

  def setConditions(conditions: CurrentConditions, user: User): F[Unit]

  def setImageQuality(iq: ImageQuality, user: User, clientId: ClientId): F[Unit]

  def setWaterVapor(wv: WaterVapor, user: User, clientId: ClientId): F[Unit]

  def setSkyBackground(sb: SkyBackground, user: User, clientId: ClientId): F[Unit]

  def setCloudExtinction(cc: CloudExtinction, user: User, clientId: ClientId): F[Unit]

  def requestRefresh(clientId: ClientId): F[Unit]

  def stopObserve(
    obsId:    Observation.Id,
    observer: Observer,
    user:     User,
    graceful: Boolean
  ): F[Unit]

  def abortObserve(
    obsId:    Observation.Id,
    observer: Observer,
    user:     User
  ): F[Unit]

  def pauseObserve(
    obsId:    Observation.Id,
    observer: Observer,
    user:     User,
    graceful: Boolean
  ): F[Unit]

  def resumeObserve(
    obsId:    Observation.Id,
    observer: Observer,
    user:     User,
    clientId: ClientId
  ): F[Unit]

  def addSequencesToQueue(qid: QueueId, obsIds: List[Observation.Id]): F[Unit]

  def addSequenceToQueue(qid: QueueId, obsId: Observation.Id): F[Unit]

  def removeSequenceFromQueue(qid: QueueId, obsId: Observation.Id): F[Unit]

  def moveSequenceInQueue(
    qid:   QueueId,
    obsId: Observation.Id,
    delta: Int,
    cid:   ClientId
  ): F[Unit]

  def clearQueue(qid: QueueId): F[Unit]

  def startQueue(
    qid:      QueueId,
    observer: Observer,
    user:     User,
    clientId: ClientId
  ): F[Unit]

  def stopQueue(qid: QueueId, clientId: ClientId): F[Unit]

  /**
   * Triggers the application of a specific step configuration to a system
   */
  def configSystem(
    obsId:    Observation.Id,
    observer: Observer,
    user:     User,
    stepId:   Step.Id,
    sys:      Subsystem,
    clientID: ClientId
  ): F[Unit]

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
    systems:       Systems[F],
    conditionsRef: Ref[F, CurrentConditions],
    environment:   ExecutionEnvironment
  ): F[SeqTranslate[F]] =
    SeqTranslate(site, systems, conditionsRef, environment)

  private def observations[F[_]](st: EngineState[F]): List[SequenceData[F]] =
    List(
      st.selected.gmosSouth,
      st.selected.gmosNorth,
      st.selected.flamingos2,
      st.selected.igrins2,
      st.selected.gnirs,
      st.selected.ghost
    ).flattenOption

  private def systemsBeingConfigured[F[_]](st: EngineState[F]): Set[Subsystem] =
    observations(st)
      .filter(d => d.seq.status.isError || d.seq.status.isIdle)
      .flatMap(s =>
        s.seq.getSingleActionStates
          .filter(_._2.started)
          .keys
          .toList
          .mapFilter(s.resourceAtCoords)
      )
      .toSet

  /**
   * Resource in use = Resources used by running sequences, plus the systems that are being
   * configured because a user commanded a manual configuration apply.
   */
  def resourcesInUse[F[_]](
    st:         EngineState[F],
    excludeObs: Option[Observation.Id] = none
  ): Set[Subsystem] =
    observations(st)
      .mapFilter(s =>
        s.resources.some
          .filterNot(_ => excludeObs.contains_(s.obsId))
          .filter(_ => s.seq.status.isRunning)
      )
      .foldK ++
      systemsBeingConfigured(st)

  /**
   * Resources reserved by running queues.
   */
  def resourcesReserved[F[_]](st: EngineState[F]): Set[Subsystem] = {
    def reserved(q: ExecutionQueue): Set[Subsystem] = q.queue.collect {
      case s if !s.state.isCompleted => s.subsystems
    }.foldK

    val runningQs = st.queues.values.filter(_.status.running)

    runningQs.map(reserved).toList.foldK

  }

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

  /**
   * Find the observations in an execution queue that would be run next, taking into account the
   * resources required by each observation and the resources currently in use. The order in the
   * queue defines the priority of the observations. Failed or stopped sequences in the queue keep
   * their instruments taken, preventing that the queue starts other sequences for those
   * instruments.
   * @param qid
   *   The execution queue id
   * @param st
   *   The current engine state
   * @return
   *   The set of all observations in the execution queue `qid` that can be started to run in
   *   parallel.
   */
  def findRunnableObservations[F[_]](qid: QueueId)(st: EngineState[F]): Set[Observation.Id] = {
    // Set of all resources in use
    val used = resourcesInUse(st)
    // For each observation in the queue that is not yet run, retrieve the required resources
    val obs  = st.queues
      .get(qid)
      .map(_.queue.collect {
        case s if !s.state.isRunning && !s.state.isCompleted =>
          s.obsId -> s.subsystems
      })
      .orEmpty

    obs
      .foldLeft((used, Set.empty[Observation.Id])) { case ((u, a), (oid, res)) =>
        if (u.intersect(res).isEmpty)
          (u ++ res, a + oid)
        else (u, a)
      }
      ._2
  }

  /**
   * Find next runnable observations given that a set of resources has just being released
   * @param qid
   *   The execution queue id
   * @param st
   *   The current engine state
   * @param freed
   *   Resources that were freed
   * @return
   *   The set of all observations in the execution queue `qid` that can be started to run in
   *   parallel.
   */
  @unused
  private def nextRunnableObservations[F[_]](qid: QueueId, freed: Set[Subsystem])(
    st: EngineState[F]
  ): Set[Observation.Id] = {
    // Set of all resources in use
    val used = resourcesInUse(st)
    // For each observation in the queue that is not yet run, retrieve the required resources
    val obs  = st.queues
      .get(qid)
      .map(_.queue.collect {
        case s if !s.state.isRunning && !s.state.isCompleted =>
          s.obsId -> s.subsystems
      })
      .orEmpty

    // Calculate instruments reserved by failed sequences in the queue
    val resFailed: Set[Instrument] = st.queues
      .get(qid)
      .map(
        _.queue.mapFilter(s => s.state.isError.option(s.instrument))
      )
      .orEmpty
      .toSet

    obs
      .foldLeft((used ++ resFailed, Set[Observation.Id]())) { case ((u, a), (oid, res)) =>
        if (u.intersect(res).isEmpty && freed.intersect(res).nonEmpty) (u ++ res, a + oid)
        else (u, a)
      }
      ._2
  }

  /**
   * shouldSchedule checks if a set of sequences are candidates for been run in a queue. It is used
   * to check if sequences added to a queue should be started.
   */
  @annotation.unused
  private def shouldSchedule[F[_]](qid: QueueId, sids: Set[Observation.Id])(
    st: EngineState[F]
  ): Set[Observation.Id] =
    findRunnableObservations(qid)(st).intersect(sids)

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

  /** Finds the paused Observe action, if any, in the execution currently loaded for `sd`. */
  private def findPausedObserve[F[_]](sd: SequenceData[F]): Option[ObserveContext[F]] =
    sd.seq.currentExecution.execution
      .find(_.kind === ActionType.Observe)
      .flatMap: a =>
        a.state.runState match
          case ActionState.Paused(c: ObserveContext[F] @unchecked) => c.some
          case _                                                   => none

  /**
   * Handles a `Continue` request while an Observe action is paused mid-exposure.
   *
   * Before blindly resuming the paused action, this re-reads the next atom from the ODB and checks
   * that the loaded step is still the one the ODB expects. If an observer edits a paused step, the
   * ODB abandons it and inserts an edited copy under a new step id; resuming against the old,
   * abandoned step would complete an exposure the ODB no longer counts. When the ODB reports a
   * different (or no) next step, the paused exposure is aborted and the edited step is loaded (but
   * not started) instead, so the observer can review it and explicitly press Run.
   */
  def resumeOrReloadStep[F[_]: {Temporal, Logger}](
    odb:        OdbProxy[F],
    translator: SeqTranslate[F],
    obsId:      Observation.Id,
    clientId:   ClientId
  ): EngineHandle[F, SeqEvent] = {

    def resumePausedAction: EngineHandle[F, SeqEvent] =
      EngineHandle
        .fromSingleEvent(Event.getState(translator.resumePaused(obsId)))
        .as(SeqEvent.NullSeqEvent)

    EngineHandle
      .inspectState[F, Option[SequenceData[F]]](EngineState.sequenceDataAt(obsId).getOption)
      .flatMap: seqDataOpt =>
        val checkInfo: Option[(SequenceData[F], LoadedStep[F], ObserveContext[F])] =
          for
            sd         <- seqDataOpt
            loadedStep <- sd.seq.loadedStep
            obsCtx     <- findPausedObserve(sd)
          yield (sd, loadedStep, obsCtx)

        checkInfo match
          case None                           => resumePausedAction
          case Some((sd, loadedStep, obsCtx)) =>
            EngineHandle
              .liftF(odb.readExecutionConfig(obsId, sd.instrument, OdbProxy.NextAtomOnly))
              .attempt
              .flatMap:
                case Left(e)      =>
                  val msg =
                    s"Could not read execution config from the ODB while resuming paused observation [$obsId]"
                  EngineHandle
                    .logError(e)(msg)
                    .as[SeqEvent]:
                      SeqEvent.NotifyUser(
                        Notification.SequenceCheckFailed(
                          obsId,
                          List(
                            "Could not verify the sequence with the ODB, the step is still paused. Press Continue to retry.",
                            e.getMessage
                          )
                        ),
                        clientId
                      )
                case Right(odbEx) =>
                  val expectedStepId: Option[Step.Id] =
                    translator
                      .nextStep(
                        OdbObservationData(sd.observation, odbEx),
                        sd.seq.currentSequenceType.asLeft
                      )
                      ._2
                      .map(_.id)

                  if expectedStepId.exists(_ === loadedStep.stepGen.id) then resumePausedAction
                  else
                    val oldStepId = loadedStep.stepGen.id

                    // Abort directly on the paused context's stream instead of going through
                    // Event.actionStop / translator.abortObserve: that path drives the action to
                    // ActionState.Aborted and the sequence's Aborted status, which we don't want -
                    // the step is simply being replaced (at the user's request to continue), not
                    // the sequence aborted. `abortPaused` still records the abort on the old,
                    // abandoned step in the ODB, which is fine.
                    val abortStaleExposure: EngineHandle[F, Unit] =
                      EngineHandle
                        .liftF(obsCtx.abortPaused.compile.drain)
                        .attempt
                        .flatMap:
                          case Left(abortError) =>
                            EngineHandle.logError(abortError):
                              s"Error aborting paused exposure for observation [$obsId], step [$oldStepId]"
                          case Right(_)         =>
                            EngineHandle.unit

                    val editedMsg =
                      s"Step [$oldStepId] for observation [$obsId] was edited in the ODB while paused; " +
                        "discarding the exposure and loading the updated step"

                    // Load the edited step but don't start it: the observer asked to continue an
                    // exposure that no longer exists, not to run whatever replaced it. Leave the
                    // sequence Idle with the new step loaded so they can review it and press Run.
                    val loadEditedStep: EngineHandle[F, Unit] =
                      retrieveStep(odb, translator, obsId, sd.seq.currentSequenceType.asLeft)
                        .flatMap:
                          case Some(_) =>
                            EngineHandle.modifySequenceState[F](obsId)(_.withIdleStatus)
                          case None    => EngineHandle.unit

                    abortStaleExposure *>
                      EngineHandle.modifySequenceState[F](obsId)(_.withNoLoadedStep) *>
                      EngineHandle.liftF(Logger[F].info(editedMsg)) *>
                      loadEditedStep *>
                      EngineHandle.pure[F, SeqEvent]:
                        SeqEvent.NotifyUser(Notification.StepEdited(obsId, oldStepId), clientId)
  }

  private def updateStep[F[_]](
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

  private def loadStep[F[_]: {MonadCancelThrow, Logger}](
    odb:        OdbProxy[F],
    translator: SeqTranslate[F],
    obsId:      Observation.Id,
    stepIdFrom: Either[SequenceType, Step.Id]
  ): EngineHandle[F, Option[StepGen[F]]] =
    (for
        _       <- modifySequenceStatus(obsId)(_.withWaitingNextStep(true).withWaitingUserPrompt(false))
        seqData <- EngineHandle.inspectState[F, Option[SequenceData[F]]](
                     EngineState.sequenceDataAt(obsId).getOption
                   )
        // Jumping to a chosen step needs the future atoms, moving on to the next one does not.
        limit    = stepIdFrom.fold(_ => OdbProxy.NextAtomOnly, _ => OdbProxy.FullFuture)
        stepGen <-
          seqData
            .traverse: sd =>
              EngineHandle
                .liftF(odb.readExecutionConfig(obsId, sd.instrument, limit))
                .map: odbEx =>
                  translator.nextStep(OdbObservationData(sd.observation, odbEx), stepIdFrom)._2
            .guarantee(modifySequenceStatus(obsId)(_.withWaitingNextStep(false)))
      yield stepGen.flatten // TODO Do something with warnings? (_1)
    ).handleErrorWith: e =>
      EngineHandle.logError(e)(
        s"Error loading step for observation [$obsId] from [$stepIdFrom]"
      ) >>
        EngineHandle
          .modifySequenceState[F](obsId)(_.withNoLoadedStep.withFailedStatus(e.getMessage)) >>
        EngineHandle
          .fromSingleEvent:
            Event.loadFailed(
              obsId,
              0,
              Result.Error(s"Error updating sequence, cannot continue: ${e.getMessage}")
            )
          .as(none)

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
    systems:     Systems[F],
    conf:        ObserveEngineConfiguration,
    environment: ExecutionEnvironment
  ): F[ObserveEngine[F]] =
    for
      rc  <- Ref.of[F, CurrentConditions](CurrentConditions.Default)
      tr  <- createTranslator(site, systems, rc, environment)
      eng <- Engine.build[F](onStepComplete[F](systems.odb, tr))
    yield new ObserveEngineImpl[F](eng, systems, conf, tr, rc)
}

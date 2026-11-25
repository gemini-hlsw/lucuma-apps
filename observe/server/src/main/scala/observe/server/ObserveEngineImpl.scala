// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import cats.Applicative
import cats.Endo
import cats.data.NonEmptyList
import cats.effect.Async
import cats.effect.Clock
import cats.effect.Ref
import cats.effect.std.SecureRandom
import cats.effect.std.UUIDGen
import cats.syntax.all.*
import coulomb.integrations.cats.all.given
import fs2.Stream
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ImageQuality
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.model.sequence.Step
import lucuma.core.refined.given
import lucuma.core.util.Timestamp
import monocle.Lens
import mouse.all.*
import observe.cats.given
import observe.common.ObsQueriesGql.ObsQuery.Data.Observation.TargetEnvironment as OdbTargetEnvironment
import observe.model.*
import observe.model.UserPrompt.Discrepancy
import observe.model.UserPrompt.ObsConditionsCheckOverride
import observe.model.UserPrompt.SeqCheck
import observe.model.UserPrompt.TargetCheckOverride
import observe.model.config.*
import observe.model.enums.ObservatorySubsystem
import observe.model.enums.ObserveLogLevel
import observe.model.enums.PendingObserveCmd
import observe.model.enums.PendingObserveCmd.*
import observe.model.enums.QueueDispatchState
import observe.model.enums.QueueItemStatus
import observe.model.enums.QueueOrderingMode
import observe.model.enums.RunOverride
import observe.model.events.*
import observe.server.engine.EngineStep
import observe.server.engine.EventResult.*
import observe.server.engine.Handle.given
import observe.server.engine.Result.Partial
import observe.server.engine.{EngineStep as _, *}
import observe.server.events.*
import observe.server.gcal.GcalController
import observe.server.odb.OdbObservationData
import observe.server.odb.OdbProxy
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.syntax.*

import java.util.concurrent.TimeUnit
import scala.annotation.unused
import scala.concurrent.duration.*

import SeqEvent.*
import ClientEvent.*

private class ObserveEngineImpl[F[_]: {Async, Logger as L}](
  executeEngine:         Engine[F],
  override val services: Services[F],
  settings:              ObserveEngineConfiguration,
  translator:            SeqTranslate[F],
  @unused conditionsRef: Ref[F, CurrentConditions]
) extends ObserveEngine[F] {

  /** The subsystems `obsId` needs that are busy. */
  private def subsystemConflict(
    obsId:  Observation.Id,
    needed: Set[Subsystem]
  )(st: EngineState[F]): Set[Subsystem] =
    needed.intersect(ObserveEngine.busySubsystems(st, excludeObs = obsId.some))

  private def busy(
    st:       EngineState[F],
    obsId:    Observation.Id,
    clientId: ClientId,
    conflict: Set[Subsystem]
  ): SeqEvent =
    Busy(obsId, clientId, conflict.toList, ObserveEngine.heldByOther(st, obsId, conflict))

  /** A failed step's actions still running keep their sequence loaded, or nothing would hold. */
  /** Why the state of an idle or failed sequence cannot be replaced yet, if it cannot. */
  private def stillRunningReason(seqData: SequenceData[F]): Option[String] =
    if ObserveEngine.hasActionsInFlight(seqData) then
      "an action of its previous step is still running".some
    else if ObserveEngine.isConfiguring(seqData) then "a subsystem is still being configured".some
    else none

  private def canLeaveSlot(seqData: SequenceData[F]): Boolean =
    executeEngine.canUnload(seqData.seq) && !ObserveEngine.isBusy(seqData)

  private def storeSequenceSubsystems(
    obsId:      Observation.Id,
    subsystems: Set[Subsystem]
  ): EngineHandle[F, Unit] =
    EngineHandle.modifyState_[F](
      EngineState
        .atSequence[F](obsId)
        .modify(SequenceData.sequenceSubsystems[F].replace(subsystems))
    )

  /**
   * Check if the target on the TCS matches the Observe target.
   *
   * For an acquisition sequence of an observation that uses a blind offset the telescope must be
   * pointing at the blind-offset target, so that is the expected one. Otherwise, including the
   * science sequence of a blind-offset observation (once the observer has moved to the science
   * target), the expected target is the ODB base position: Navigate slews to it and writes its name
   * to the TCS, and for multi-target asterisms that name is a composite of the target names. The
   * first science target is only a fallback for when the ODB cannot compute the base.
   *
   * Names are compared trimmed, since the TCS record may pad them. With a simulated TCS there is no
   * real target to compare against, so the check is skipped.
   *
   * @return
   *   an F that returns an optional TargetCheckOverride if the targets don't match
   */
  private def sequenceTcsTargetMatch(
    seqData: SequenceData[F],
    seqType: SequenceType
  ): F[Option[TargetCheckOverride]] =
    val targetEnv: OdbTargetEnvironment   = seqData.targetEnvironment
    val scienceTarget: Option[String]     =
      targetEnv.basePosition
        .map(_.name.value)
        .orElse(targetEnv.firstScienceTarget.map(_.targetName.value))
    val blindOffsetTarget: Option[String] = targetEnv.blindOffsetTarget.map(_.targetName.value)
    val expectedTarget: Option[String]    =
      if seqType === SequenceType.Acquisition && targetEnv.useBlindOffset then
        blindOffsetTarget.orElse(scienceTarget)
      else scienceTarget
    expectedTarget
      .filter(_ => settings.subsystemControlModes.tcs.realKeywords)
      .map: seqTarget =>
        services.tcsKeywordReader.sourceATarget.objectName.map: tcsTarget =>
          (seqTarget.trim =!= tcsTarget.trim).option:
            TargetCheckOverride(UserPrompt.Discrepancy(tcsTarget, seqTarget))
      .getOrElse(none.pure[F])

  // Only on-sky steps care about the TCS target and the sky conditions. A day-cal science step
  // takes light from the sky, but with the telescope not pointing at a target.
  private def stepRequiresChecks(kind: StepKind): Boolean =
    kind match
      case StepKind.CelestialObject(_) | StepKind.NodAndShuffle(_) | StepKind.Gems(_) |
          StepKind.AltairObs(_) =>
        true
      case StepKind.DayCalObject(_) | StepKind.FlatOrArc(_, _) | StepKind.NightFlatOrArc(_, _) |
          StepKind.DarkOrBias(_) | StepKind.DarkOrBiasNS(_) | StepKind.ExclusiveDarkOrBias(_) =>
        false

  private def checkCloudCover(
    actual:    Option[CloudExtinction],
    requested: CloudExtinction.Preset
  ): Boolean =
    actual.forall(_ <= requested.toCloudExtinction)

  private def checkImageQuality(
    actual:    Option[ImageQuality],
    requested: ImageQuality.Preset
  ): Boolean =
    actual.forall(_ <= requested.toImageQuality)

  private def checkSkyBackground(
    actual:    Option[SkyBackground],
    requested: SkyBackground
  ): Boolean =
    actual.forall(_ <= requested)

  private def checkWaterVapor(actual: Option[WaterVapor], requested: WaterVapor): Boolean =
    actual.forall(_ <= requested)

  private def observingConditionsMatch(
    actualObsConditions:   CurrentConditions,
    requiredObsConditions: ConstraintSet
  ): Option[ObsConditionsCheckOverride] = {

    val UnknownStr: String = "Unknown"
    val reqCE              = requiredObsConditions.cloudExtinction
    val reqIQ              = requiredObsConditions.imageQuality
    val reqSB              = requiredObsConditions.skyBackground
    val reqWV              = requiredObsConditions.waterVapor

    val ccCmp = (!checkCloudCover(actualObsConditions.ce, reqCE))
      .option(
        Discrepancy(
          actualObsConditions.ce.fold(UnknownStr)(_.label),
          reqCE.toCloudExtinction.label
        )
      )

    val iqCmp = (!checkImageQuality(actualObsConditions.iq, reqIQ))
      .option(
        Discrepancy(
          actualObsConditions.iq.fold(UnknownStr)(_.label),
          reqIQ.toImageQuality.label
        )
      )

    val sbCmp = (!checkSkyBackground(actualObsConditions.sb, reqSB))
      .option(Discrepancy(actualObsConditions.sb.fold(UnknownStr)(_.label), reqSB.label))

    val wvCmp = (!checkWaterVapor(actualObsConditions.wv, reqWV))
      .option(Discrepancy(actualObsConditions.wv.fold(UnknownStr)(_.label), reqWV.label))

    (ccCmp.nonEmpty || iqCmp.nonEmpty || sbCmp.nonEmpty || wvCmp.nonEmpty)
      .option(ObsConditionsCheckOverride(ccCmp, iqCmp, sbCmp, wvCmp))
  }

  private def clearObsCmd(obsId: Observation.Id): EngineHandle[F, SeqEvent] =
    EngineHandle.modifyState:
      EngineState
        .atSequence[F](obsId)
        .andThen(SequenceData.pendingObsCmd)
        .replace(None)
        .withEvent:
          SeqEvent.NullSeqEvent: SeqEvent

  private def setObsCmd(obsId: Observation.Id, cmd: PendingObserveCmd): EngineHandle[F, SeqEvent] =
    EngineHandle.modifyState:
      EngineState
        .atSequence[F](obsId)
        .andThen(SequenceData.pendingObsCmd)
        .replace(cmd.some)
        .withEvent:
          SeqEvent.NullSeqEvent: SeqEvent

  // Produce a Handle that will send a SequenceStart notification to the ODB, and produces the (sequenceId, stepId)
  // if there is a valid sequence with a valid current step.
  private def sequenceStart(
    obsId: Observation.Id
  ): EngineHandle[F, Option[(Observation.Id, Step.Id)]] =
    EngineHandle.getState.flatMap { s =>
      EngineState
        .atSequence(obsId)
        .getOption(s)
        .flatMap { seq =>
          val startVisit: EngineHandle[F, SeqEvent] =
            if (!seq.visitStartDone)
              EngineHandle
                .fromEventStream:
                  Stream.eval[F, Event[F]]:
                    services.odb
                      .visitStart(obsId)
                      .as:
                        Event.modifyState:
                          EngineHandle
                            .modifyState_ :
                              EngineState.atSequence[F](obsId).modify(_.withCompleteVisitStart)
                            .as(SeqEvent.NullSeqEvent)
                .as(SeqEvent.NullSeqEvent)
            else
              EngineHandle.pure(SeqEvent.NullSeqEvent)

          seq.seq.loadedStep.map: curStep =>
            (startVisit *>
              Handle
                .fromEventStream[F, EngineState[F], Event[F]](
                  Stream.eval[F, Event[F]](
                    services.odb
                      .sequenceStart(obsId)
                      .as(Event.nullEvent)
                  )
                )).as((obsId, curStep.id).some)
        }
        .getOrElse(EngineHandle.pure(none[(Observation.Id, Step.Id)]))
    }

  private def startAfterCheck(
    startAction: EngineHandle[F, Unit],
    id:          Observation.Id
  ): EngineHandle[F, SeqEvent] =
    startAction.reversedStreamFlatMap: _ =>
      sequenceStart(id).map:
        _.map: (sid, stepId) =>
          SequenceStart(sid, stepId)
        .getOrElse(NullSeqEvent)

  private def startChecks(
    startAction:    EngineHandle[F, Unit],
    obsId:          Observation.Id,
    clientId:       Option[ClientId],
    @unused stepId: Option[Step.Id],
    runOverride:    RunOverride,
    knownHold:      Option[Set[Subsystem]] = none
  ): EngineHandle[F, SeqEvent] =
    EngineHandle.getState.flatMap { st =>
      EngineState
        .atSequence(obsId)
        .getOption(st)
        // A completed sequence may have been given new steps (e.g. edited in Explore). Starting
        // it reads the next step from the ODB, which runs it, or completes the sequence again
        // if there is none.
        .filter: seqData =>
          seqData.seq.status.isIdle || seqData.seq.status.isError ||
            seqData.seq.status.isCompleted
        .map: seq =>
          // Read before the step, in the same handle, so no other start comes in between.
          def startHolding(hold: Set[Subsystem]): EngineHandle[F, SeqEvent] =
            for {
              startingStep   <-
                ObserveEngine.retrieveStep(
                  services.odb,
                  translator,
                  seq.obsId,
                  seq.currentSequenceType.asLeft
                )
              checkedStepOpt  = startingStep.filter(step => stepRequiresChecks(step.kind))
              (stpg, checks) <-
                EngineHandle.liftF:
                  checkedStepOpt
                    .map: checkedStep =>
                      sequenceTcsTargetMatch(seq, seq.currentSequenceType).map: tchk =>
                        (checkedStep.some,
                         List(
                           tchk,
                           observingConditionsMatch(st.conditions, seq.constraintSet)
                         ).flattenOption
                        )
                    .getOrElse((none[StepGen[F]], List.empty[SeqCheck]).pure[F])
              resultEvent    <-
                EngineHandle.getState
                  .flatMap: newState =>
                    val needed: Set[Subsystem]   =
                      hold ++ newState.sequences.get(obsId).foldMap(_.stepSubsystems)
                    val conflict: Set[Subsystem] = subsystemConflict(obsId, needed)(newState)
                    (conflict.isEmpty, stpg, checks, runOverride) match
                      // Resource check fails
                      case (false, _, _, _)                           =>
                        EngineHandle
                          .modifySequenceState[F](obsId)(_.withNoLoadedStep.withIdleStatus)
                          .as[SeqEvent]:
                            clientId.fold(NullSeqEvent)(busy(newState, obsId, _, conflict))
                      // Target check fails and no override
                      case (_, Some(_), x :: xs, RunOverride.Default) =>
                        EngineHandle
                          .modifySequenceState[F](obsId)(_.withNoLoadedStep.withIdleStatus)
                          .as[SeqEvent](
                            clientId.fold(NullSeqEvent): cid =>
                              RequestConfirmation(
                                UserPrompt.ChecksOverride(
                                  obsId,
                                  UserPrompt.ChecksOverride.Action.StartSequence,
                                  NonEmptyList(x, xs)
                                ),
                                cid
                              )
                          )
                      // Allowed to run
                      case _                                          =>
                        storeSequenceSubsystems(obsId, needed) >>
                          startAfterCheck(startAction, obsId)
            } yield resultEvent

          (for {
            _           <- EngineHandle.modifySequenceState[F](obsId): seq =>
                             SequenceState.status.replace(SequenceStatus.Running.Starting)(seq.rollback)
            // The daytime queue has just read what the sequence holds, in this same handle.
            holdRead    <- knownHold.fold(
                             EngineHandle.liftF:
                               SequenceSubsystems.read(services.odb, obsId, seq.instrument).attempt
                           )(hold => EngineHandle.pure(hold.asRight[Throwable]))
            resultEvent <-
              holdRead.fold(
                // Nothing has run yet, so the start is refused and the sequence is left idle.
                e =>
                  EngineHandle.logError(e)(s"Error reading the steps left in [$obsId]") >>
                    EngineHandle
                      .modifySequenceState[F](obsId)(_.withIdleStatus)
                      .as[SeqEvent]:
                        clientId.fold(NullSeqEvent): cid =>
                          SeqEvent.NotifyUser(
                            Notification.ActionFailed(obsId, "start the sequence", e.getMessage),
                            cid
                          )
                ,
                startHolding
              )
          } yield resultEvent).handleErrorWith(e =>
            EngineHandle
              .modifySequenceState[F](obsId)(_.withNoLoadedStep.withFailedStatus(e.getMessage))
              .as[SeqEvent](
                clientId.fold(NullSeqEvent): client =>
                  SeqEvent.NotifyUser(
                    Notification.LoadingFailed(
                      obsId,
                      List(s"Error loading sequence for observation $obsId", e.getMessage)
                    ),
                    client
                  )
              )
          )
        .getOrElse: // Trying to run a sequence that does not exist. This should never happen.
          EngineHandle.unit.as[SeqEvent](NullSeqEvent)
    }

  // Stars a sequence from the first non executed step. The method checks for subsystems conflict.
  override def startSequence(
    obsId:       Observation.Id,
    user:        User,
    observer:    Observer,
    clientId:    ClientId,
    runOverride: RunOverride
  ): F[Unit] =
    logInfoEvent(s"Sequence $obsId: Start requested by ${user.displayName}") *>
      executeEngine.offer:
        Event.modifyState[F]:
          EngineHandle.getState.flatMap: st =>
            // Starting again would run the step's actions next to the ones still in flight.
            val stillRunning: Option[String] =
              st.sequences
                .get(obsId)
                .filter: s =>
                  s.seq.status.isIdle || s.seq.status.isError || s.seq.status.isCompleted
                .flatMap(stillRunningReason)
            stillRunning.fold(
              setObserver(obsId, observer) *>
                clearObsCmd(obsId) *>
                startChecks(
                  executeEngine.startLoadedStep(obsId),
                  obsId,
                  clientId.some,
                  none,
                  runOverride
                )
            ): reason =>
              EngineHandle.pure[F, SeqEvent]:
                NotifyUser(Notification.ActionFailed(obsId, "start the sequence", reason), clientId)

  override def proceedAfterPrompt(
    id:          Observation.Id,
    user:        User,
    observer:    Observer,
    seqType:     SequenceType,
    clientId:    ClientId,
    runOverride: RunOverride
  ): F[Unit] =
    setObserver(id, user, observer) *>
      executeEngine.offer:
        Event.modifyState[F]:
          clearObsCmd(id) *>
            userNextStep(id, seqType, clientId, runOverride)

  /**
   * Proceeds from the user prompt (typically after the acquisition). When going on to science the
   * target is checked first, since for blind-offset observations the observer must have moved to
   * the science target by then. As in `startChecks`, only steps that require checks are checked, so
   * a science sequence opening with a flat or arc proceeds without a prompt. If the check fails the
   * sequence keeps waiting at the prompt and the client is asked for confirmation.
   */
  def userNextStep(
    obsId:       Observation.Id,
    seqType:     SequenceType,
    clientId:    ClientId,
    runOverride: RunOverride
  ): EngineHandle[F, SeqEvent] =
    EngineHandle.getState.flatMap: st =>
      EngineState
        .atSequence(obsId)
        .getOption(st)
        .map: seqData => // Sequence exists in memory, proceed
          // Without the prompt check, proceedAfterPrompt from Idle silently reloads the step.
          if seqData.seq.status.isWaitingUserPrompt then
            // The step is only peeked at here; tryNewStep loads it for real.
            val nextStepRequiresChecks: EngineHandle[F, Boolean]          =
              ObserveEngine
                .readStep(services.odb, translator, obsId, SequenceType.Science.asLeft)
                .map(_.exists(step => stepRequiresChecks(step.kind)))
                .handleErrorWith(_ => EngineHandle.pure(false))
            val targetCheck: EngineHandle[F, Option[TargetCheckOverride]] =
              if seqType === SequenceType.Science && runOverride === RunOverride.Default then
                nextStepRequiresChecks.flatMap: requiresChecks =>
                  if requiresChecks then
                    EngineHandle.liftF(sequenceTcsTargetMatch(seqData, SequenceType.Science))
                  else EngineHandle.pure(none[TargetCheckOverride])
              else EngineHandle.pure(none[TargetCheckOverride])
            targetCheck.flatMap:
              case Some(check) =>
                EngineHandle.pure[F, SeqEvent](
                  RequestConfirmation(
                    UserPrompt.ChecksOverride(
                      obsId,
                      UserPrompt.ChecksOverride.Action.ProceedToScience,
                      NonEmptyList.one(check)
                    ),
                    clientId
                  )
                )
              case None        =>
                ObserveEngine
                  .tryNewStep(services.odb, translator, executeEngine, obsId, seqType)
                  .as[SeqEvent](SeqEvent.NullSeqEvent)
          else EngineHandle.unit.as[SeqEvent](SeqEvent.NullSeqEvent)
        .getOrElse(EngineHandle.unit.as[SeqEvent](SeqEvent.NullSeqEvent))

  /**
   * Replaces the loaded step with the next step of `seqType` without starting it. This is how the
   * acquisition is skipped (`seqType` = Science) or restarted (`seqType` = Acquisition with
   * `resetAcquisition`).
   *
   * Only allowed while the sequence is idle or in error; otherwise the client gets a
   * `SequenceNotIdle` notification and nothing changes. If the ODB reset or the step read fails, or
   * there is no step of `seqType`, the sequence is left as it was and an `ActionFailed` is
   * reported.
   *
   * Engine events run one at a time, so the idle check and the state change below cannot race with
   * a sequence start. The ODB reset must therefore happen here and not in the caller.
   */
  private def loadNextStepWhileIdle(
    obsId:            Observation.Id,
    seqType:          SequenceType,
    resetAcquisition: Boolean,
    clientId:         ClientId,
    actionName:       String
  ): EngineHandle[F, SeqEvent] =
    EngineHandle.getState.flatMap: st =>
      EngineState
        .atSequence(obsId)
        .getOption(st)
        .map: seqData =>
          if !(seqData.seq.status.isIdle || seqData.seq.status.isError) then
            EngineHandle.pure[F, SeqEvent]:
              SeqEvent.NotifyUser(Notification.SequenceNotIdle(obsId, actionName), clientId)
          else if ObserveEngine.isBusy(seqData) then
            // Checked before the ODB reset: loading another step replaces the sequence state.
            EngineHandle.pure[F, SeqEvent]:
              SeqEvent.NotifyUser(
                Notification.ActionFailed(obsId, actionName, stillRunningReason(seqData).orEmpty),
                clientId
              )
          else
            def failed(msg: String): SeqEvent =
              SeqEvent.NotifyUser(Notification.ActionFailed(obsId, actionName, msg), clientId)

            // A reset regenerates the acquisition steps with new ids, so their breakpoints are not
            // in the sequence's set yet (which is only built on load). The regenerated acquisition
            // is read in the same round trip as the step to load, and its breakpoints are
            // registered only once that step is known, so a failure leaves the sequence as it was.
            val read: EngineHandle[F, (Option[StepGen[F]], Option[InstrumentExecutionConfig])] =
              if resetAcquisition then
                EngineHandle.liftF(services.odb.resetAcquisition(obsId)) >>
                  ObserveEngine
                    .readConfigAndStep(
                      services.odb,
                      translator,
                      obsId,
                      seqType.asLeft,
                      OdbProxy.FullFuture
                    )
                    .map(_.fold((none, none))((config, stepGen) => (stepGen, config.some)))
              else
                // `readStep` (unlike `retrieveStep`) neither swallows errors nor touches the state,
                // so a failure can be reported without having changed anything.
                ObserveEngine
                  .readStep(services.odb, translator, obsId, seqType.asLeft)
                  .map((_, none))

            read
              .flatMap:
                // The translator falls back to science when there's no acquisition; that's no reset.
                case (Some(stepGen), config) if stepGen.sequenceType === seqType =>
                  for
                    _ <- EngineHandle.modifyState_(
                           ObserveEngine.updateStep(obsId, stepGen.some, seqType.asLeft)
                         )
                    _ <- config.traverse_ { c =>
                           EngineHandle.modifySequenceState[F](obsId) { s =>
                             s.copy(breakpoints = Breakpoints.withAcquisitionFrom(s.breakpoints, c))
                           }
                         }
                  yield SeqEvent.SequenceTypeChanged(obsId): SeqEvent
                case _                                                           =>
                  EngineHandle.pure[F, SeqEvent](failed(s"There is no ${seqType.tag} step to load"))
              .handleErrorWith: e =>
                EngineHandle.pure[F, SeqEvent](failed(e.getMessage))
        .getOrElse(EngineHandle.pure[F, SeqEvent](NullSeqEvent))

  override def skipAcquisition(
    obsId:    Observation.Id,
    user:     User,
    observer: Observer,
    clientId: ClientId
  ): F[Unit] =
    logInfoEvent(s"Sequence $obsId: Skip acquisition requested by ${user.displayName}") *>
      setObserver(obsId, user, observer) *>
      executeEngine.offer:
        Event.modifyState[F]:
          clearObsCmd(obsId) *>
            loadNextStepWhileIdle(
              obsId,
              SequenceType.Science,
              resetAcquisition = false,
              clientId,
              "skip acquisition"
            )

  override def resetAcquisition(
    obsId:    Observation.Id,
    user:     User,
    observer: Observer,
    clientId: ClientId
  ): F[Unit] =
    logInfoEvent(s"Sequence $obsId: Reset acquisition requested by ${user.displayName}") *>
      setObserver(obsId, user, observer) *>
      executeEngine.offer:
        Event.modifyState[F]:
          clearObsCmd(obsId) *>
            loadNextStepWhileIdle(
              obsId,
              SequenceType.Acquisition,
              resetAcquisition = true,
              clientId,
              "reset acquisition"
            )

  /**
   * Takes the breakpoints of the `editedType` sequence, just replaced in the ODB, from it. The
   * loaded step is left alone: Start reads the step to run from the ODB anyway. On a failed read
   * the sequence is left as it was.
   */
  private def reloadBreakpointsAfterEdit(
    obsId:      Observation.Id,
    editedType: SequenceType,
    clientId:   ClientId
  ): EngineHandle[F, SeqEvent] =
    val actionName: String = "reload the edited sequence"
    EngineHandle.getState.flatMap: st =>
      EngineState
        .atSequence(obsId)
        .getOption(st)
        .map: seqData =>
          val status: SequenceStatus = seqData.seq.status
          if !(status.isIdle || status.isError || status.isCompleted) then
            EngineHandle.pure[F, SeqEvent]:
              SeqEvent.NotifyUser(Notification.SequenceNotIdle(obsId, actionName), clientId)
          else
            EngineHandle
              .liftF:
                services.odb.readExecutionConfig(obsId, seqData.instrument, OdbProxy.FullFuture)
              .flatMap: config =>
                EngineHandle
                  .modifySequenceState[F](obsId): s =>
                    s.copy(breakpoints =
                      Breakpoints.withSequenceFrom(s.breakpoints, config, editedType)
                    )
                  .as[SeqEvent](SeqEvent.SequenceTypeChanged(obsId))
              .handleErrorWith: e =>
                EngineHandle.pure[F, SeqEvent]:
                  SeqEvent.NotifyUser(
                    Notification.ActionFailed(obsId, actionName, e.getMessage),
                    clientId
                  )
        .getOrElse(EngineHandle.pure[F, SeqEvent](NullSeqEvent))

  override def reloadAfterEdit(
    obsId:        Observation.Id,
    user:         User,
    observer:     Observer,
    sequenceType: SequenceType,
    clientId:     ClientId
  ): F[Unit] =
    logInfoEvent(
      s"Sequence $obsId: Reload after editing the ${sequenceType.tag} sequence requested by ${user.displayName}"
    ) *>
      setObserver(obsId, user, observer) *>
      executeEngine.offer:
        Event.modifyState[F]:
          reloadBreakpointsAfterEdit(obsId, sequenceType, clientId)

  override def requestSequenceHold(
    obsId:    Observation.Id,
    observer: Observer,
    user:     User
  ): F[Unit] =
    logInfoEvent(s"Sequence $obsId: Hold requested by ${user.displayName}") *>
      setObserver(obsId, user, observer) *>
      executeEngine.offer(Event.requestSequenceHold(obsId, user))

  override def cancelSequenceHoldRequest(
    obsId:    Observation.Id,
    observer: Observer,
    user:     User
  ): F[Unit] =
    logInfoEvent(s"Sequence $obsId: Continue requested by ${user.displayName}") *>
      setObserver(obsId, user, observer) *>
      executeEngine.offer(Event.cancelSequenceHoldRequest(obsId, user))

  override def rewindStep(
    obsId:    Observation.Id,
    observer: Observer,
    user:     User
  ): F[Unit] =
    logInfoEvent(s"Sequence $obsId: Step rewind requested by ${user.displayName}") *>
      setObserver(obsId, user, observer) *>
      executeEngine.offer(Event.requestStepRewind(obsId, user))

  override def cancelRewindRequest(
    obsId:    Observation.Id,
    observer: Observer,
    user:     User
  ): F[Unit] =
    logInfoEvent(s"Sequence $obsId: Rewind request cancelled by ${user.displayName}") *>
      setObserver(obsId, user, observer) *>
      executeEngine.offer(Event.cancelStepRewindRequest(obsId, user))

  override def setBreakpoints(
    obsId:    Observation.Id,
    user:     User,
    observer: Observer,
    steps:    Set[Step.Id],
    v:        Breakpoint
  ): F[Unit] =
    // Set the observer after the breakpoints are set to do optimistic updates on the UI
    executeEngine.offer(Event.breakpoints(obsId, user, steps, v)) *>
      setObserver(obsId, user, observer)

  override def setOperator(user: User, name: Operator): F[Unit] =
    logDebugEvent(s"ObserveEngine: Setting Operator name to '$name' by ${user.displayName}") *>
      executeEngine.offer:
        Event.modifyState:
          EngineHandle.modifyState:
            (EngineState.operator[F].replace(name.some) >>> refreshSequences)
              .withEvent(SetOperator(name, user.some))

  private def setObserver(
    id:       Observation.Id,
    observer: Observer,
    event:    SeqEvent = SeqEvent.NullSeqEvent
  ): EngineHandle[F, SeqEvent] =
    EngineHandle.modifyState:
      EngineState
        .atSequence[F](id)
        .andThen(SequenceData.observer)
        .replace(observer.some)
        .withEvent(event)

  override def setObserver(
    obsId: Observation.Id,
    user:  User,
    name:  Observer
  ): F[Unit] =
    logDebugEvent(
      s"ObserveEngine: Setting Observer name to '$name' for sequence '$obsId' by ${user.displayName}"
    ) *>
      executeEngine.offer:
        Event.modifyState:
          EngineHandle.modifyState:
            (
              EngineState
                .atSequence(obsId)
                .modify(SequenceData.observer.replace(name.some)) >>>
                refreshSequence(obsId)
            ).withEvent(SetObserver(obsId, user.some, name))

  override def loadSequence(
    i:        Instrument,
    obsId:    Observation.Id,
    observer: Observer,
    user:     User,
    clientId: ClientId
  ): F[Unit] =
    val author = s", by '${user.displayName}' on client ${clientId.value}."
    executeEngine.inject(
      // We want the acquisition sequence to reset whenever we load the observation.
      services.odb.resetAcquisition(obsId) >>
        services.odb
          .read(obsId)
          .map: odbData =>
            val (errs: List[Throwable], stepGen: Option[StepGen[F]]) =
              translator.nextStep(odbData, SequenceType.Acquisition.asLeft)
            (errs, odbData, stepGen)
          .attempt
          .flatMap(
            _.fold(
              e =>
                L.warn(e)(s"Error loading observation $obsId$author")
                  .as(
                    Event.pure(
                      SeqEvent.NotifyUser(
                        Notification.LoadingFailed(
                          obsId,
                          List(s"Error loading observation $obsId", e.getMessage)
                        ),
                        clientId
                      )
                    )
                  ),
              { case (errs, odbData, stepGen) =>
                errs.isEmpty
                  .fold(
                    info"Loaded observation $obsId$author",
                    warn"Loaded observation $obsId with warnings: ${errs.mkString}$author"
                  )
                  .as(
                    Event.modifyState {
                      EngineHandle.modifyState { (st: EngineState[F]) =>
                        val instrumentSequenceLens = EngineState.instrumentLoaded[F](i)
                        if (instrumentSequenceLens.get(st).forall(canLeaveSlot)) {
                          (loadObservationMod(
                             odbData,
                             stepGen,
                             observer.some,
                             instrumentSequenceLens
                           )(st),
                           LoadSequence(obsId, clientId)
                          )
                        } else {
                          (
                            st,
                            SeqEvent
                              .NotifyUser(
                                Notification.LoadingFailed(
                                  obsId,
                                  List(
                                    s"Error loading observation $obsId",
                                    s"A sequence is running on instrument $i"
                                  )
                                ),
                                clientId
                              ): SeqEvent
                          )
                        }
                      }
                    }
                  )
              }
            )
          )
    )

  /**
   * Puts the observation read from the ODB into its instrument slot: a new one replaces what was
   * there, while one that is already loaded is reloaded keeping its observer and overrides.
   */
  private def loadObservationMod(
    odbData:                OdbObservationData,
    stepGen:                Option[StepGen[F]],
    observer:               Option[Observer],
    instrumentSequenceLens: Lens[EngineState[F], Option[SequenceData[F]]]
  ): Endo[EngineState[F]] =
    (st: EngineState[F]) =>
      st.sequences
        .get(odbData.observation.id)
        .fold(ODBSequencesLoader.loadSequenceMod(observer, odbData, instrumentSequenceLens))(_ =>
          ODBSequencesLoader.reloadSequenceMod(
            observer,
            stepGen,
            Breakpoints.fromInstrumentExecutionConfig(odbData.executionConfig),
            instrumentSequenceLens
          )
        )(st)

  private def logDebugEvent(msg: String): F[Unit] = L.debug(msg)

  private def logInfoEvent(msg: String): F[Unit] = L.info(msg)

  private def logDebugEvent(msg: String, user: User, clientId: ClientId): F[Unit] =
    L.debug(s"$msg, by ${user.displayName} from client $clientId")

  override def requestRestart(user: User, clientId: ClientId, shutdown: F[Unit]): F[Unit] =
    logInfoEvent(s"Restart requested by ${user.displayName}") *>
      executeEngine.offer:
        Event.modifyState[F]:
          EngineHandle.getState.flatMap: st =>
            ObserveEngine.restartBlockers(st) match
              case None          =>
                EngineHandle.liftF[F, Unit](logInfoEvent("Restarting the server")) >>
                  EngineHandle.liftF[F, Unit](shutdown).as(SeqEvent.NullSeqEvent)
              case Some(reasons) =>
                EngineHandle.liftF[F, Unit](
                  L.warn:
                    s"Restart requested by ${user.displayName} refused: ${reasons.mkString_(", ")}"
                ) >>
                  EngineHandle.pure[F, SeqEvent]:
                    SeqEvent.NotifyUser(Notification.RestartRefused(reasons), clientId)

  override def unloadSequence(obsId: Observation.Id, user: User, clientId: ClientId): F[Unit] =
    L.info(
      s"Sequence $obsId: Unload requested by ${user.displayName} on client ${clientId.value}"
    ) *>
      executeEngine.offer:
        Event.modifyState[F]:
          EngineHandle.getState.flatMap: st =>
            st.sequencesByInstrument.values
              .find(_.obsId === obsId)
              .fold(EngineHandle.pure[F, SeqEvent](SeqEvent.NullSeqEvent)): seqData =>
                if canLeaveSlot(seqData) then
                  EngineHandle.modifyState_[F](
                    EngineState.instrumentLoaded[F](seqData.instrument).replace(none)
                  ) >>
                    dispatchDaytimeQueue.as[SeqEvent](SeqEvent.UnloadSequence(obsId))
                else
                  EngineHandle.pure[F, SeqEvent]:
                    SeqEvent.NotifyUser(Notification.SequenceNotIdle(obsId, "unload"), clientId)

  override def clearLoadedSequences(user: User): F[Unit] =
    logDebugEvent("ObserveEngine: Updating loaded sequences") *>
      executeEngine.offer:
        Event.modifyState:
          EngineHandle.modifyState:
            EngineState
              .selected[F]
              .replace(Selected.none)
              .withEvent(ClearLoadedSequences(user.some))

  override def resetConditions: F[Unit] = logDebugEvent("ObserveEngine: Reset conditions") *>
    executeEngine.offer:
      Event.modifyState:
        EngineHandle.modifyState:
          (EngineState.conditions[F].replace(CurrentConditions.Default) >>> refreshSequences)
            .withEvent(SetConditions(CurrentConditions.Default, None))

  override def setConditions(
    conditions: CurrentConditions,
    user:       User
  ): F[Unit] =
    logDebugEvent("ObserveEngine: Setting conditions") *>
      executeEngine.offer:
        Event.modifyState:
          EngineHandle.modifyState:
            (EngineState.conditions[F].replace(conditions) >>> refreshSequences)
              .withEvent(SetConditions(conditions, user.some))

  override def setImageQuality(iq: ImageQuality, user: User, clientId: ClientId): F[Unit] =
    logDebugEvent(s"ObserveEngine: Setting image quality to $iq", user, clientId) *>
      executeEngine.offer:
        Event.modifyState:
          EngineHandle.modifyState:
            (EngineState
              .conditions[F]
              .andThen(CurrentConditions.iq)
              .replace(iq.some) >>> refreshSequences)
              .withEvent(SetImageQuality(iq, user.some))

  override def setWaterVapor(wv: WaterVapor, user: User, clientId: ClientId): F[Unit] =
    logDebugEvent(s"ObserveEngine: Setting water vapor to $wv", user, clientId) *>
      executeEngine.offer:
        Event.modifyState:
          EngineHandle.modifyState:
            (EngineState
              .conditions[F]
              .andThen(CurrentConditions.wv)
              .replace(wv.some) >>> refreshSequences)
              .withEvent(SetWaterVapor(wv, user.some))

  override def setSkyBackground(sb: SkyBackground, user: User, clientId: ClientId): F[Unit] =
    logDebugEvent(s"ObserveEngine: Setting sky background to $sb", user, clientId) *>
      executeEngine.offer:
        Event.modifyState:
          EngineHandle.modifyState:
            (EngineState
              .conditions[F]
              .andThen(CurrentConditions.sb)
              .replace(sb.some) >>> refreshSequences)
              .withEvent(SetSkyBackground(sb, user.some))

  override def setCloudExtinction(ce: CloudExtinction, user: User, clientId: ClientId): F[Unit] =
    logDebugEvent(s"ObserveEngine: Setting cloud cover to $ce", user, clientId) *>
      executeEngine.offer:
        Event.modifyState:
          EngineHandle.modifyState:
            (EngineState
              .conditions[F]
              .andThen(CurrentConditions.ce)
              .replace(ce.some) >>> refreshSequences)
              .withEvent(SetCloudExtinction(ce, user.some))

  override def requestRefresh(clientId: ClientId): F[Unit] =
    executeEngine.offer(Event.poll(clientId))

  private val heartbeatPeriod: FiniteDuration = FiniteDuration(10, TimeUnit.SECONDS)

  private def heartbeatStream: Stream[F, Event[F]] = {
    // If there is no heartbeat in 5 periods throw an error
    val noHeartbeatDetection =
      ObserveEngine.failIfNoEmitsWithin[F, Event[F]](
        50 * heartbeatPeriod, // TODO REVERT TO 5 * heartbeatPeriod
        "Engine heartbeat not detected"
      )
    Stream
      .awakeDelay[F](heartbeatPeriod)
      .as(Event.nullEvent: Event[F])
      .through(noHeartbeatDetection.andThen(_.recoverWith { case _ =>
        Stream.eval[F, Event[F]](Event.logErrorMsgF("Observe engine heartbeat undetected"))
      }))
  }

  private def singleActionClientEvent(
    c:            ActionCoords,
    qState:       EngineState[F],
    clientAction: ClientEvent.SingleActionState,
    errorMsg:     Option[String] = none
  ): Option[TargetedClientEvent] =
    qState.sequences
      .get(c.obsId)
      .flatMap(_.subsystemAtCoords(c.actCoords))
      .map(res => SingleActionEvent(c.obsId, c.actCoords.stepId, res, clientAction, errorMsg))

  private def viewSequence(obsSeq: SequenceData[F]): SequenceView = {
    val sequenceState: SequenceState[F] = obsSeq.seq
    val instrument: Instrument          = obsSeq.instrument
    val seqType: SequenceType           = sequenceState.currentSequenceType

    val engineLoadedStep: Option[ObserveStep] =
      sequenceState.loadedStep.map: ls =>
        val stepSubsystems =
          ls.subsystems.toList.mapFilter: x =>
            obsSeq
              .configActionCoord(ls.id, x)
              .map(i => (x, sequenceState.getSingleState(i).actionStatus))

        StepsView
          .stepsView(instrument)
          .stepView(
            ls.stepGen,
            ls.executionZipper.toEngineStep,
            stepSubsystems,
            obsSeq.pendingObsCmd
          )

    val engStep: Option[ObserveStep] = engineLoadedStep

    // TODO: Implement willStopIn
    SequenceView(
      sequenceState.obsId,
      SequenceMetadata(instrument, obsSeq.observer), // , obsSeq.obsData.title),
      sequenceState.status,
      obsSeq.overrides,
      seqType,
      engStep,
      None,
      sequenceState.breakpoints.value
    )
  }

  private def buildObserveStateStream(
    svs:      => SequencesQueue[SequenceView],
    odbProxy: OdbProxy[F]
  ): Stream[F, TargetedClientEvent] =
    Stream.eval:
      odbProxy.getCurrentRecordedIds.map: recordedIds =>
        ObserveState.fromSequenceViewQueue(svs, recordedIds)

  private def toClientModifyEventStream(
    v:        SeqEvent,
    svs:      => SequencesQueue[SequenceView],
    odbProxy: OdbProxy[F]
  ): Stream[F, TargetedClientEvent] =
    v match
      case RequestConfirmation(c @ UserPrompt.ChecksOverride(_, _, _), clientId) =>
        Stream.emit(ClientEvent.ChecksOverrideEvent(c).forClient(clientId))
      // case RequestConfirmation(m, clientId)        => Stream.emit(UserPromptNotification(m, clientId))
      case StartSubsystemConfig(oid, stepId, res)                                =>
        Stream.emit[F, TargetedClientEvent](
          SingleActionEvent(oid, stepId, res, ClientEvent.SingleActionState.Started, none)
        ) ++ buildObserveStateStream(svs, odbProxy)
      // The state follows a refusal so the client stops waiting for the command to take effect.
      case NotifyUser(memo, clientId)                                            =>
        Stream.emit[F, TargetedClientEvent](UserNotification(memo).forClient(clientId)) ++
          buildObserveStateStream(svs, odbProxy)
      case SeqEvent.SubsystemClash(obsId, stepId, subsystems)                    =>
        Stream.emit[F, TargetedClientEvent](
          ClientEvent.SubsystemClash(obsId, stepId, subsystems.toList)
        ) ++ buildObserveStateStream(svs, odbProxy)
      case Busy(obsId, clientId, subsystems, heldBy)                             =>
        Stream.emit[F, TargetedClientEvent](
          ClientEvent
            .UserNotification(Notification.SubsystemConflict(obsId, subsystems, heldBy))
            .forClient(clientId)
        ) ++ buildObserveStateStream(svs, odbProxy)
      case LoadSequence(obsId, clientId)                                         =>
        buildObserveStateStream(svs, odbProxy) ++
          svs.loaded
            .collectFirst:
              case (instrument, oId) if oId === obsId => instrument
            .foldMap: instrument =>
              Stream.emit:
                ClientEvent.ObsLoaded(instrument).forClient(clientId)
      case SubsystemBusy(obsId, stepId, subsystem, clientId)                     =>
        Stream.emit:
          UserNotification(Notification.SubsystemBusy(obsId, stepId, subsystem)).forClient(clientId)
      case NewStepLoaded(obsId, sequenceType, atomId, stepId)                    =>
        Stream.emit[F, TargetedClientEvent](
          ClientEvent.StepLoaded(obsId, sequenceType, atomId, stepId)
        ) ++
          buildObserveStateStream(svs, odbProxy)
      case AcquisitionCompleted(obsId)                                           =>
        Stream.emit[F, TargetedClientEvent](ClientEvent.AcquisitionPromptReached(obsId)) ++
          buildObserveStateStream(svs, odbProxy)
      case e if e.isModelUpdate                                                  =>
        buildObserveStateStream(svs, odbProxy)
      case _                                                                     => Stream.empty

  private def toClientEvent(
    ev:       EventResult,
    qState:   EngineState[F],
    odbProxy: OdbProxy[F]
  ): Stream[F, TargetedClientEvent] = {
    val sequences: List[SequenceView] =
      qState.sequences.view.values.map(viewSequence).toList

    // Building the view is a relatively expensive operation
    // By putting it into a def we only incur that cost if the message requires it
    def svs: SequencesQueue[SequenceView] =
      SequencesQueue(
        List(
          qState.selected.gmosSouth.map(x => Instrument.GmosSouth -> x.obsId),
          qState.selected.gmosNorth.map(x => Instrument.GmosNorth -> x.obsId),
          qState.selected.ghost.map(x => Instrument.Ghost -> x.obsId),
          qState.selected.flamingos2.map(x => Instrument.Flamingos2 -> x.obsId),
          qState.selected.igrins2.map(x => Instrument.Igrins2 -> x.obsId),
          qState.selected.gnirs.map(x => Instrument.Gnirs -> x.obsId)
        ).flattenOption.toMap,
        qState.conditions,
        qState.operator,
        DaytimeQueueView(qState.daytimeQueue, queueBusySubsystems(qState)),
        sequences
      )

    ev match
      case UserCommandResponse(ue, _, uev) =>
        ue match
          case UserEvent.Pure(NotifyUser(m, cid)) =>
            Stream.emit(UserNotification(m).forClient(cid))
          case UserEvent.ModifyState(_)           =>
            toClientModifyEventStream(uev.getOrElse(NullSeqEvent), svs, odbProxy)
          case e if e.isModelUpdate               => buildObserveStateStream(svs, odbProxy)
          case UserEvent.LogInfo(m, ts)           =>
            Stream.emit(LogEvent(LogMessage(ObserveLogLevel.Info, ts, m)))
          case UserEvent.LogWarning(m, ts)        =>
            Stream.emit(LogEvent(LogMessage(ObserveLogLevel.Warning, ts, m)))
          case UserEvent.LogError(m, ts)          =>
            Stream.emit(LogEvent(LogMessage(ObserveLogLevel.Error, ts, m)))
          case UserEvent.LogDebug(m, ts)          =>
            Stream.emit(LogEvent(LogMessage(ObserveLogLevel.Debug, ts, m)))
          case _                                  => Stream.empty
      case EngineUpdate(ee, _)             =>
        ee match
          // TODO: Sequence completed event not emitted by engine.
          case EngineEvent.PartialResult(i, s, _, Partial(ObsProgress(t, r, v)))   =>
            Stream.emit(
              ProgressEvent(ObservationProgress(i, StepProgress.Regular(s, t, r.value, v)))
            )
          case EngineEvent.PartialResult(i, s, _, Partial(NsProgress(t, r, v, u))) =>
            Stream.emit(
              ProgressEvent(ObservationProgress(i, StepProgress.NodAndShuffle(s, t, r.value, v, u)))
            )
          // case EngineEvent.Busy(id, clientId)                                       =>
          //   Stream.emit(UserNotification(SubsystemConflict(id), clientId))
          case EngineEvent.SingleRunCompleted(c, _)                                =>
            Stream.emits(
              singleActionClientEvent(
                c,
                qState,
                ClientEvent.SingleActionState.Completed
              ).toList
            ) ++ buildObserveStateStream(svs, odbProxy)
          case EngineEvent.SingleRunFailed(c, Result.Error(msg))                   =>
            Stream.emits(
              singleActionClientEvent(
                c,
                qState,
                ClientEvent.SingleActionState.Failed,
                msg.some
              ).toList
            ) ++
              Stream.emit(SequenceFailed(c.obsId, msg): TargetedClientEvent) ++
              buildObserveStateStream(svs, odbProxy)
          case EngineEvent.StepComplete(obsId)                                     =>
            Stream.emit(StepComplete(obsId): TargetedClientEvent) ++
              buildObserveStateStream(svs, odbProxy)
          case EngineEvent.SequenceHeld(obsId)                                     =>
            Stream.emit(SequenceHeld(obsId): TargetedClientEvent) ++
              buildObserveStateStream(svs, odbProxy)
          case EngineEvent.BreakpointReached(obsId)                                =>
            Stream.emit(BreakpointReached(obsId): TargetedClientEvent) ++
              buildObserveStateStream(svs, odbProxy)
          case EngineEvent.SequenceComplete(obsId)                                 =>
            Stream.emit(ClientEvent.SequenceComplete(obsId): TargetedClientEvent) ++
              buildObserveStateStream(svs, odbProxy)
          case EngineEvent.Failed(obsId, _, _, Result.Error(msg))                  =>
            Stream.emit(SequenceFailed(obsId, msg): TargetedClientEvent) ++
              buildObserveStateStream(svs, odbProxy)
          case EngineEvent.LoadFailed(obsId, _, Result.Error(msg))                 =>
            Stream.emit(SequenceFailed(obsId, msg): TargetedClientEvent) ++
              buildObserveStateStream(svs, odbProxy)
          case e if e.isModelUpdate                                                =>
            buildObserveStateStream(svs, odbProxy)
          case _                                                                   => Stream.empty
  }

  override val clientEventStream: Stream[F, TargetedClientEvent] =
    heartbeatStream
      .as[TargetedClientEvent](BaDum)
      .merge:
        eventResultStream(EngineState.default[F])
          .flatMap: (result, qState) =>
            toClientEvent(result, qState, services.odb).merge:
              Stream
                .eval:
                  notifyODB(result, qState)
                    .as(none)
                    .handleErrorWith: e =>
                      LogMessage
                        .now(ObserveLogLevel.Error, s"Error notifying ODB: ${e.getMessage}")
                        .map(LogEvent(_): TargetedClientEvent)
                        .map(_.some)
                .unNone
      .merge:
        // Broadcast the current guide configuration whenever it changes.
        services.guideDb.discrete.map: gcs =>
          ClientEvent.GuideConfigEvent(gcs.config): TargetedClientEvent

  override def eventResultStream(s0: EngineState[F]): Stream[F, (EventResult, EngineState[F])] =
    executeEngine.process(onEngineEvent)(s0)

  override def stopExposure(
    obsId:    Observation.Id,
    observer: Observer,
    user:     User,
    graceful: Boolean
  ): F[Unit] =
    logInfoEvent(s"Sequence $obsId: Stop requested by ${user.displayName}") *>
      setObserver(obsId, user, observer) *>
      executeEngine.offer(
        Event.getState: engineState =>
          EngineState
            .atSequence(obsId)
            .andThen(SequenceData.seq)
            .getOption(engineState)
            .flatMap(_.loadedStep)
            .foldMap(step => Stream.eval(services.odb.stepStop(obsId, step.id)))
            .as(Event.nullEvent)
      ) *>
      executeEngine
        .offer(Event.modifyState(setObsCmd(obsId, StopGracefully)))
        .whenA(graceful) *>
      executeEngine.offer:
        Event.actionStop(obsId, translator.stopExposure(obsId, graceful))

  override def abortExposure(
    obsId:    Observation.Id,
    observer: Observer,
    user:     User
  ): F[Unit] =
    logInfoEvent(s"Sequence $obsId: Abort requested by ${user.displayName}") *>
      setObserver(obsId, user, observer) *>
      executeEngine.offer:
        Event.actionStop(obsId, translator.abortExposure(obsId))

  override def pauseExposure(
    obsId:    Observation.Id,
    observer: Observer,
    user:     User,
    graceful: Boolean
  ): F[Unit] =
    logInfoEvent(s"Sequence $obsId: Pause requested by ${user.displayName}") *>
      setObserver(obsId, user, observer) *>
      executeEngine
        .offer:
          Event.modifyState(setObsCmd(obsId, PauseGracefully))
        .whenA(graceful) *>
      executeEngine.offer:
        Event.actionStop(obsId, translator.pauseExposure(obsId, graceful))

  override def resumeExposure(
    obsId:    Observation.Id,
    observer: Observer,
    user:     User
  ): F[Unit] =
    logInfoEvent(s"Sequence $obsId: Continue requested by ${user.displayName}") *>
      executeEngine.offer(Event.modifyState(clearObsCmd(obsId))) *>
      setObserver(obsId, user, observer) *>
      executeEngine.offer:
        Event.getState(translator.resumePaused(obsId))

  // Daytime queue

  /** How long the user has to cancel before the queue turns the GCAL lamps off. */
  private val LampsOffCountdown: FiniteDuration = 60.seconds

  private def modifyDaytimeQueue(f: Endo[DaytimeQueue]): EngineHandle[F, Unit] =
    EngineHandle.modifyState_(EngineState.daytimeQueue[F].modify(f))

  private def modifyQueueItem(obsId: Observation.Id)(
    f: DaytimeQueueItem.Obs => DaytimeQueueItem.Obs
  ): EngineHandle[F, Unit] =
    modifyDaytimeQueue(_.modifyObs(obsId)(f))

  private def syncDaytimeQueue: EngineHandle[F, QueueSyncResult] =
    EngineHandle
      .liftF(Clock[F].realTimeInstant)
      .flatMap: now =>
        EngineHandle.modifyState:
          DaytimeQueueOps.sync[F](Timestamp.fromInstantTruncatedAndBounded(now))

  private def errorMessage(e: Throwable): String =
    Option(e.getMessage).getOrElse(e.toString)

  private def failQueueItem(obsId: Observation.Id, msg: String): EngineHandle[F, Unit] =
    EngineHandle.liftF(L.warn(s"Daytime queue: observation $obsId failed to start: $msg")) >>
      modifyQueueItem(obsId)(
        _.copy(status = QueueItemStatus.Failed, errorMsg = msg.some, abortRequested = false)
      )

  /**
   * The queue only takes a slot that is empty, completed or holds a queue observation, and never
   * one whose failed step still has actions running. A paused (Stopped) queue observation stays
   * loaded: only `forObs`, the observation itself, may take its slot back.
   */
  private def slotAvailable(
    st:         EngineState[F],
    instrument: Instrument,
    forObs:     Option[Observation.Id]
  ): Boolean =
    EngineState
      .instrumentLoaded[F](instrument)
      .get(st)
      .forall: s =>
        val replaceable: Boolean =
          s.seq.status.isCompleted || forObs.contains_(s.obsId) ||
            st.daytimeQueue.obs(s.obsId).exists(_.status =!= QueueItemStatus.Stopped)
        canLeaveSlot(s) && replaceable

  /** Busy subsystems plus the instruments whose slot the queue may not take. */
  private def queueBusySubsystems(st: EngineState[F]): Set[Subsystem] =
    ObserveEngine.busySubsystems(st) ++
      st.sequencesByInstrument.keySet
        .filterNot(slotAvailable(st, _, none))
        .map(instrument => instrument: Subsystem)

  /**
   * Starts a queue observation unattended (no confirmation prompts), and gives the subsystems it
   * needs that are busy, if that is why it did not start.
   *
   * What it needs is read again, since it may have changed since it was added, and the queue keeps
   * the latest: an observation that cannot have its subsystems stays as it is, is not loaded, and
   * the dispatcher leaves it alone until they are free. Otherwise it is loaded (only its next atom
   * is read) and started as a manual start would, in this same engine step. The ODB already gives
   * the remaining steps of a partly executed observation, so it continues from its first incomplete
   * step.
   */
  private def startQueueItem(
    obsId:    Observation.Id,
    observer: Observer
  ): EngineHandle[F, Set[Subsystem]] =
    val nothing: Set[Subsystem] = Set.empty

    def loadAndStart(instrument: Instrument, hold: Set[Subsystem]): EngineHandle[F, Unit] =
      EngineHandle
        .liftF(services.odb.read(obsId, OdbProxy.NextAtomOnly).attempt)
        .flatMap:
          case Left(e)        => failQueueItem(obsId, errorMessage(e))
          case Right(odbData) =>
            val stepGen: Option[StepGen[F]] =
              translator.nextStep(odbData, SequenceType.Acquisition.asLeft)._2
            EngineHandle.liftF(L.info(s"Daytime queue: starting observation $obsId")) >>
              EngineHandle.modifyState_(
                loadObservationMod(
                  odbData,
                  stepGen,
                  observer.some,
                  EngineState.instrumentLoaded(instrument)
                )
              ) >>
              setObserver(obsId, observer) >>
              clearObsCmd(obsId) >>
              startChecks(
                executeEngine.startLoadedStep(obsId),
                obsId,
                none,
                none,
                RunOverride.Override,
                hold.some
              ) >>
              EngineHandle
                .liftF(Clock[F].realTimeInstant)
                .flatMap: now =>
                  EngineHandle.modifyState_(
                    DaytimeQueueOps
                      .afterStart[F](obsId, Timestamp.fromInstantTruncatedAndBounded(now))
                  )

    EngineHandle.getState.flatMap: st0 =>
      st0.daytimeQueue
        .obs(obsId)
        .filter(item => slotAvailable(st0, item.instrument, obsId.some))
        .fold(EngineHandle.pure[F, Set[Subsystem]](nothing)): item =>
          EngineHandle
            .liftF(services.odb.readRemainingSteps(obsId, item.instrument).attempt)
            .flatMap:
              case Left(e)          => failQueueItem(obsId, errorMessage(e)).as(nothing)
              case Right(remaining) =>
                val resources: QueueItemResources =
                  DaytimeQueueOps.resources(item.instrument, remaining)
                // The total only grows, so the progress of a resumed observation does not restart.
                modifyQueueItem(obsId)(queued =>
                  queued.copy(
                    subsystems = resources.subsystems,
                    estimate = resources.estimate,
                    total = queued.total.max(resources.estimate)
                  )
                ) >>
                  EngineHandle.getState.flatMap: st =>
                    val conflict: Set[Subsystem] =
                      subsystemConflict(obsId, resources.subsystems)(st)
                    if conflict.nonEmpty then EngineHandle.pure[F, Set[Subsystem]](conflict)
                    else loadAndStart(item.instrument, resources.subsystems).as(nothing)

  /**
   * Starts the Waiting observations the dispatch rules allow, one at a time. In strict mode an
   * observation that could not be started stops the pass, so nothing below it jumps ahead.
   */
  private def startRunnableQueueItems(tried: Set[Observation.Id]): EngineHandle[F, Unit] =
    EngineHandle.getState.flatMap: st =>
      st.daytimeQueueObserver
        .filter(_ => st.daytimeQueue.dispatchState === QueueDispatchState.Running)
        .flatMap: observer =>
          val runnable: List[Observation.Id] =
            QueueDispatcher.runnable(st.daytimeQueue, queueBusySubsystems(st))
          val next: Option[Observation.Id]   =
            if st.daytimeQueue.orderingMode === QueueOrderingMode.Strict then
              runnable.headOption.filterNot(tried.contains)
            else runnable.find(obsId => !tried.contains(obsId))
          next
            .map: obsId =>
              startQueueItem(obsId, observer) >> startRunnableQueueItems(tried + obsId)
        .getOrElse(EngineHandle.unit)

  /**
   * Turns the GCAL lamps off (IR lamp untouched) without blocking the engine, recording the outcome
   * in the queue. GCAL is busy until the command ends, so nothing else commands it meanwhile.
   */
  private def turnLampsOff: EngineHandle[F, Unit] =
    EngineHandle.liftF(L.info("Daytime queue: turning the GCAL lamps off")) >>
      EngineHandle.modifyState_(EngineState.daytimeQueueLampsOff[F].replace(true)) >>
      EngineHandle.fromEventStream:
        Stream.eval:
          services.gcal
            .applyConfig(GcalController.GcalConfig.GcalOffIgnoringIr)
            .attempt
            .flatTap:
              case Left(e) => L.error(e)("Daytime queue: unable to turn the GCAL lamps off")
              case _       => Applicative[F].unit
            .map: result =>
              Event.modifyState[F]:
                EngineHandle.modifyState_(EngineState.daytimeQueueLampsOff[F].replace(false)) >>
                  modifyDaytimeQueue(
                    DaytimeQueue.lampsOffError.replace(result.left.toOption.map(errorMessage))
                  ) >>
                  // GCAL is free again: what waited for it can start.
                  dispatchDaytimeQueue.as(SeqEvent.DaytimeQueueUpdated)

  /** Starts the countdown to turn the GCAL lamps off, which every client sees and may cancel. */
  private def scheduleLampsOff: EngineHandle[F, Unit] =
    EngineHandle
      .liftF(Clock[F].realTimeInstant)
      .flatMap: now =>
        val at: Timestamp =
          Timestamp.fromInstantTruncatedAndBounded(now.plusMillis(LampsOffCountdown.toMillis))
        EngineHandle.liftF(L.info(s"Daytime queue finished: the GCAL lamps go off at $at")) >>
          modifyDaytimeQueue(DaytimeQueue.lampsOffAt.replace(at.some)) >>
          EngineHandle.fromEventStream:
            Stream.sleep[F](LampsOffCountdown).as(Event.modifyState[F](lampsOffDue(at)))

  /** The countdown that ended at `at` turns the lamps off, unless it was cancelled meanwhile. */
  private def lampsOffDue(at: Timestamp): EngineHandle[F, SeqEvent] =
    EngineHandle.getState.flatMap: st =>
      if !st.daytimeQueue.lampsOffAt.contains_(at) then
        EngineHandle.pure[F, SeqEvent](SeqEvent.NullSeqEvent)
      else if ObserveEngine.busySubsystems(st).contains(ObservatorySubsystem.Gcal) then
        // An action that is ending still has GCAL: the lamps wait for it.
        scheduleLampsOff.as(SeqEvent.DaytimeQueueUpdated)
      else
        modifyDaytimeQueue(DaytimeQueue.lampsOffAt.replace(none)) >>
          turnLampsOff.as(SeqEvent.DaytimeQueueUpdated)

  /**
   * Syncs the queue with the loaded sequences, starts what can be started if the queue is running,
   * and starts the countdown to turn the GCAL lamps off when the queue has just finished.
   */
  private def dispatchDaytimeQueue: EngineHandle[F, Unit] =
    for
      before <- EngineHandle.inspectState[F, DaytimeQueue](_.daytimeQueue)
      _      <- syncDaytimeQueue
      _      <- startRunnableQueueItems(Set.empty)
      _      <- syncDaytimeQueue
      st     <- EngineHandle.getState[F]
      _      <-
        if DaytimeQueueOps.shouldTurnLampsOff(QueueSyncResult.between(before, st.daytimeQueue))
        then scheduleLampsOff
        else EngineHandle.unit
    yield ()

  /**
   * Run after the engine events that can change which observations are running or which subsystems
   * are free. An action that ends counts: it may be the last one a failed step left running, or a
   * manual configuration.
   */
  private def onEngineEvent: PartialFunction[EngineEvent, EngineHandle[F, Unit]] =
    case EngineEvent.Executing(_) | EngineEvent.SequenceComplete(_) |
        EngineEvent.Failed(_, _, _, _) | EngineEvent.LoadFailed(_, _, _) |
        EngineEvent.Aborted(_, _, _, _) | EngineEvent.SequenceHeld(_) |
        EngineEvent.BreakpointReached(_) | EngineEvent.Completed(_, _, _, _) |
        EngineEvent.StopCompleted(_, _, _, _) | EngineEvent.SingleRunCompleted(_, _) |
        EngineEvent.SingleRunFailed(_, _) =>
      dispatchDaytimeQueue

  private def daytimeQueueCommand(description: String, user: User, clientId: ClientId)(
    handle: EngineHandle[F, SeqEvent]
  ): F[Unit] =
    logInfoEvent(
      s"Daytime queue: $description requested by ${user.displayName} from client ${clientId.value}"
    ) *>
      executeEngine.offer(Event.modifyState[F](handle))

  private def modifyQueueAndDispatch(f: Endo[DaytimeQueue]): EngineHandle[F, SeqEvent] =
    modifyDaytimeQueue(f) >> dispatchDaytimeQueue.as(SeqEvent.DaytimeQueueUpdated)

  private def runningQueueItems(st: EngineState[F]): List[Observation.Id] =
    st.daytimeQueue.obsItems.filter(_.status === QueueItemStatus.Running).map(_.obsId)

  /** What the queue needs of an observation, or why it does not accept it. */
  private def readQueueItem(obsId: Observation.Id): F[Either[String, QueueItemResources]] =
    services.odb
      .readQueueObservation(obsId)
      .flatMap: obs =>
        DaytimeQueueOps
          .accepted(obs)
          .traverse: instrument =>
            services.odb
              .readRemainingSteps(obsId, instrument)
              .map(DaytimeQueueOps.resources(instrument, _))

  private def addToDaytimeQueueHandle(
    obsId:    Observation.Id,
    read:     Either[Throwable, Either[String, QueueItemResources]],
    clientId: ClientId
  ): EngineHandle[F, SeqEvent] =
    def reject(reason: String): EngineHandle[F, SeqEvent] =
      EngineHandle
        .liftF(L.info(s"Daytime queue: observation $obsId rejected: $reason"))
        .as(SeqEvent.NotifyUser(Notification.DaytimeQueueRejected(obsId, reason), clientId))

    read match
      // Not a loading failure: clients take that as the loaded observation being broken.
      case Left(e)     =>
        EngineHandle
          .liftF(L.warn(e)(s"Daytime queue: error reading observation $obsId"))
          .as:
            SeqEvent.NotifyUser(
              Notification
                .DaytimeQueueRejected(obsId, s"It could not be read: ${errorMessage(e)}"),
              clientId
            )
      case Right(item) =>
        EngineHandle.getState.flatMap: st =>
          if st.daytimeQueue.contains(obsId) then reject("It is already in the daytime queue")
          else
            item.fold(
              reject,
              resources =>
                modifyQueueAndDispatch(_.append(DaytimeQueueOps.waitingItem(obsId, resources)))
            )

  override def addToDaytimeQueue(
    obsId:    Observation.Id,
    user:     User,
    clientId: ClientId
  ): F[Unit] =
    // Read before the engine is asked, so the wait for the ODB stalls nothing.
    readQueueItem(obsId).attempt.flatMap: read =>
      daytimeQueueCommand(s"adding $obsId", user, clientId):
        addToDaytimeQueueHandle(obsId, read, clientId)

  override def removeFromDaytimeQueue(
    itemId:   QueueItemId,
    user:     User,
    clientId: ClientId
  ): F[Unit] =
    daytimeQueueCommand(s"removing $itemId", user, clientId):
      modifyQueueAndDispatch(_.remove(itemId))

  override def moveInDaytimeQueue(
    itemId:   QueueItemId,
    toIndex:  Int,
    user:     User,
    clientId: ClientId
  ): F[Unit] =
    daytimeQueueCommand(s"moving $itemId to $toIndex", user, clientId):
      modifyQueueAndDispatch(_.move(itemId, toIndex))

  override def addDaytimeQueuePause(atIndex: Int, user: User, clientId: ClientId): F[Unit] =
    for
      given SecureRandom[F] <- SecureRandom.javaSecuritySecureRandom[F]
      uuid                  <- UUIDGen.fromSecureRandom[F].randomUUID
      _                     <- daytimeQueueCommand(s"adding a pause at $atIndex", user, clientId):
                                 modifyQueueAndDispatch(_.insertPause(QueuePauseId(uuid), atIndex))
    yield ()

  override def setDaytimeQueueOrderingMode(
    mode:     QueueOrderingMode,
    user:     User,
    clientId: ClientId
  ): F[Unit] =
    daytimeQueueCommand(s"setting the ordering mode to ${mode.tag}", user, clientId):
      modifyQueueAndDispatch(DaytimeQueue.orderingMode.replace(mode))

  override def clearCompletedDaytimeQueue(user: User, clientId: ClientId): F[Unit] =
    daytimeQueueCommand("clearing the completed observations", user, clientId):
      modifyQueueAndDispatch(_.clearCompleted)

  override def cancelDaytimeQueueLampsOff(user: User, clientId: ClientId): F[Unit] =
    daytimeQueueCommand("cancelling the lamps off", user, clientId):
      modifyDaytimeQueue(DaytimeQueue.lampsOffAt.replace(none)).as(SeqEvent.DaytimeQueueUpdated)

  override def retryDaytimeQueueItem(
    obsId:    Observation.Id,
    user:     User,
    clientId: ClientId
  ): F[Unit] =
    daytimeQueueCommand(s"retrying $obsId", user, clientId):
      modifyQueueAndDispatch(_.retry(obsId))

  override def runDaytimeQueue(observer: Observer, user: User, clientId: ClientId): F[Unit] =
    daytimeQueueCommand("run", user, clientId):
      EngineHandle.modifyState_(EngineState.daytimeQueueObserver[F].replace(observer.some)) >>
        modifyQueueAndDispatch(_.run.copy(lampsOffError = none))

  override def holdDaytimeQueueAfterCurrentSteps(user: User, clientId: ClientId): F[Unit] =
    daytimeQueueCommand("hold after the current steps", user, clientId):
      EngineHandle.getState.flatMap: st =>
        runningQueueItems(st).traverse_(executeEngine.requestSequenceHold) >>
          modifyQueueAndDispatch(DaytimeQueue.dispatchState.replace(QueueDispatchState.Halted))

  override def holdDaytimeQueueAfterCurrentObservations(user: User, clientId: ClientId): F[Unit] =
    daytimeQueueCommand("hold after the current observations", user, clientId):
      modifyQueueAndDispatch(DaytimeQueue.dispatchState.replace(QueueDispatchState.Halted))

  /**
   * Holds the sequence, so that it stops at the step boundary if there is nothing to abort, and
   * aborts the exposure in progress. Either way the queue labels the observation Aborted.
   */
  private def abortQueueItem(obsId: Observation.Id, observer: Observer): EngineHandle[F, Unit] =
    modifyQueueItem(obsId)(_.copy(abortRequested = true)) >>
      setObserver(obsId, observer).void >>
      executeEngine.requestSequenceHold(obsId) >>
      EngineHandle.fromSingleEvent(Event.actionStop(obsId, translator.abortExposure(obsId)))

  override def abortDaytimeQueue(observer: Observer, user: User, clientId: ClientId): F[Unit] =
    daytimeQueueCommand("abort", user, clientId):
      EngineHandle.getState.flatMap: st =>
        runningQueueItems(st).traverse_(abortQueueItem(_, observer)) >>
          modifyQueueAndDispatch(DaytimeQueue.dispatchState.replace(QueueDispatchState.Halted))

  override def runDaytimeQueueItem(
    obsId:    Observation.Id,
    observer: Observer,
    user:     User,
    clientId: ClientId
  ): F[Unit] =
    daytimeQueueCommand(s"running $obsId", user, clientId):
      EngineHandle.getState.flatMap: st =>
        st.daytimeQueue.obs(obsId) match
          case Some(item @ DaytimeQueueItem.Obs(status = status))
              if !item.isRunning && status =!= QueueItemStatus.Completed =>
            if !slotAvailable(st, item.instrument, obsId.some) then
              EngineHandle.pure[F, SeqEvent]:
                SeqEvent.NotifyUser(Notification.InstrumentInUse(obsId, item.instrument), clientId)
            else
              startQueueItem(obsId, observer).flatMap: conflict =>
                syncDaytimeQueue >>
                  EngineHandle.inspectState[F, SeqEvent]: st1 =>
                    if conflict.isEmpty then SeqEvent.DaytimeQueueUpdated
                    else
                      SeqEvent.NotifyUser(
                        Notification.SubsystemConflict(
                          obsId,
                          conflict.toList,
                          ObserveEngine.heldByOther(st1, obsId, conflict)
                        ),
                        clientId
                      )
          case _ => EngineHandle.pure[F, SeqEvent](SeqEvent.NullSeqEvent)

  override def abortDaytimeQueueItem(
    obsId:    Observation.Id,
    observer: Observer,
    user:     User,
    clientId: ClientId
  ): F[Unit] =
    daytimeQueueCommand(s"aborting $obsId", user, clientId):
      EngineHandle.getState.flatMap: st =>
        if runningQueueItems(st).contains_(obsId) then
          abortQueueItem(obsId, observer) >> dispatchDaytimeQueue.as(SeqEvent.DaytimeQueueUpdated)
        else EngineHandle.pure[F, SeqEvent](SeqEvent.NullSeqEvent)

  private def configureSubsystemCheck(
    subsystem: Subsystem,
    st:        EngineState[F]
  ): Boolean =
    !ObserveEngine.busySubsystems(st).contains(subsystem)

  private def configureSubsystemHandle(
    obsId:     Observation.Id,
    stepId:    Step.Id,
    subsystem: Subsystem,
    clientId:  ClientId
  ): EngineHandle[F, SeqEvent] =
    EngineHandle.getState.flatMap { st0 =>
      // Checked before the step is read: reading it replaces the state of the sequence.
      val canConfigure: Boolean           =
        st0.sequences
          .get(obsId)
          .map(_.seq.status)
          .forall(status => status.isIdle || status.isError || status.isCompleted)
      // Another subsystem of the step being configured can be configured without reading it.
      val sameStep: Boolean               =
        st0.sequences
          .get(obsId)
          .exists: s =>
            ObserveEngine.isConfiguring(s) && s.seq.loadedStep.exists(_.id === stepId)
      // Reading another step, or resetting a failed one, forgets the actions still running.
      val stillRunning: Option[String]    =
        st0.sequences
          .get(obsId)
          .filter(s => ObserveEngine.hasActionsInFlight(s) || !sameStep)
          .flatMap(stillRunningReason)
      val readStep: EngineHandle[F, Unit] =
        if sameStep then EngineHandle.unit
        else ObserveEngine.retrieveStep(services.odb, translator, obsId, stepId.asRight).void
      if (!canConfigure)
        EngineHandle.pure[F, SeqEvent]:
          NotifyUser(Notification.SequenceNotIdle(obsId, "configure a subsystem"), clientId)
      else if (stillRunning.isDefined)
        EngineHandle.pure[F, SeqEvent]:
          NotifyUser(
            Notification.ActionFailed(obsId, "configure a subsystem", stillRunning.orEmpty),
            clientId
          )
      else if (configureSubsystemCheck(subsystem, st0))
        readStep                               // Load new step and reload state
          .flatMap(_ => EngineHandle.getState) // A new step may have loaded, so reload state
          .flatMap: st =>
            st.sequences
              .get(obsId)
              .flatMap(_.configActionCoord(stepId, subsystem))
              .map: c =>
                executeEngine
                  .startSingle(ActionCoords(obsId, c))
                  .map[SeqEvent]:
                    case EventResult.Outcome.Ok => StartSubsystemConfig(obsId, stepId, subsystem)
                    case _                      => NullSeqEvent
              .getOrElse:
                EngineHandle.pure(NullSeqEvent)
      else EngineHandle.pure(SubsystemBusy(obsId, stepId, subsystem, clientId))
    }

    // EngineHandle.getState.flatMap { st0 =>
    //   if (configureSubsystemCheck(subsystem, st0))
    //     ObserveEngine.loadStep(services.odb, translator, obsId, stepId.asRight) >>
    //       // We have to reread the state after loading
    //       EngineHandle.getState.flatMap { st =>
    //         st.sequences
    //           .get(obsId)
    //           .flatMap(_.configActionCoord(stepId, subsystem))
    //           .map: c =>
    //             executeEngine
    //               .startSingle(ActionCoords(obsId, c))
    //               .map[SeqEvent]:
    //                 case EventResult.Outcome.Ok => StartSubsystemConfig(obsId, stepId, subsystem)
    //                 case _                      => NullSeqEvent
    //           .getOrElse(
    //             EngineHandle.pure(NullSeqEvent)
    //           )
    //       }
    //   else
    //     EngineHandle.pure(SubsystemBusy(obsId, stepId, subsystem, clientId))
    // }

  /**
   * Triggers the application of a specific step configuration to a system
   */
  override def configureSubsystem(
    obsId:     Observation.Id,
    observer:  Observer,
    user:      User,
    stepId:    Step.Id,
    subsystem: Subsystem,
    clientId:  ClientId
  ): F[Unit] =
    setObserver(obsId, user, observer) >>
      executeEngine.offer:
        Event.modifyState:
          configureSubsystemHandle(obsId, stepId, subsystem, clientId)

  private def notifyOdbSequenceHeld(obsId: Observation.Id): F[Unit] =
    services.odb
      .obsPause(obsId)
      .ensure(
        ObserveFailure
          .Unexpected("Unable to send ObservationPaused message to ODB.")
      )(identity)
      .void

  def notifyODB(
    i: (EventResult, EngineState[F])
  ): F[(EventResult, EngineState[F])] =
    (i match {
      // A failure is not an abort, so no step event is sent.
      case (EngineUpdate(EngineEvent.SequenceHeld(obsId), _), _)                 =>
        notifyOdbSequenceHeld(obsId)
      case (EngineUpdate(EngineEvent.BreakpointReached(obsId), _), _)            =>
        notifyOdbSequenceHeld(obsId)
      case (EngineUpdate(EngineEvent.LoadFailed(obsId, _, e), _), _)             =>
        Logger[F].error(s"Error loading $obsId due to $e") <*
          services.odb
            .obsStop(obsId)
            .ensure(
              ObserveFailure
                .Unexpected("Unable to send ObservationLoadFailed message to ODB.")
            )(identity)
      case (UserCommandResponse(UserEvent.ModifyState(_), _, Some(seqEvent)), _) =>
        seqEvent match
          case AcquisitionCompleted(obsId)                        =>
            notifyOdbSequenceHeld(obsId)
          case NewStepLoaded(obsId, sequenceType, atomId, stepId) =>
            services.odb
              .obsContinue(obsId)
              .ensure(
                ObserveFailure
                  .Unexpected("Unable to send ObservationContinued message to ODB.")
              )(identity)
              .void
          case _                                                  =>
            Applicative[F].unit
      case _                                                                     => Applicative[F].unit
    }).as(i)

  private def updateSequenceEndo(
    conditions: CurrentConditions,
    operator:   Option[Operator]
  ): Endo[SequenceData[F]] =
    (sd: SequenceData[F]) =>
      val newStep: Option[EngineStep[F]] =
        sd.loadedStep.map: ls =>
          generateStep(
            ls.stepGen,
            sd.overrides,
            HeaderExtraData(conditions, operator, sd.observer)
          )._1

      SequenceData.seq.modify(
        executeEngine.updateStep(newStep)
      )(sd)

  private def refreshSequence(id: Observation.Id): Endo[EngineState[F]] = (st: EngineState[F]) =>
    EngineState.atSequence(id).modify(updateSequenceEndo(st.conditions, st.operator))(st)

  private def refreshSequences: Endo[EngineState[F]] = (st: EngineState[F]) =>
    val f: Endo[EngineState[F]] =
      List(
        EngineState.gmosNorthSequence[F],
        EngineState.gmosSouthSequence[F],
        EngineState.flamingos2Sequence[F],
        EngineState.igrins2Sequence[F],
        EngineState.gnirsSequence[F]
      ).map(_.modify(updateSequenceEndo(st.conditions, st.operator))).combineAll
    f(st)

  private def toggleOverride(
    subsystem: String,
    modify:    (SubsystemEnabled, SubsystemOverrides) => SubsystemOverrides,
    event:     SeqEvent,
    obsId:     Observation.Id,
    user:      User,
    enabled:   SubsystemEnabled,
    clientId:  ClientId
  ): F[Unit] =
    logDebugEvent(
      s"ObserveEngine: Setting $subsystem enabled flag to '$enabled' for sequence '$obsId'",
      user,
      clientId
    ) *>
      executeEngine.offer:
        Event.modifyState:
          EngineHandle.modifyState:
            (
              EngineState
                .atSequence(obsId)
                .modify(SequenceData.overrides.modify(x => modify(enabled, x)))
                >>> refreshSequence(obsId)
            ).withEvent(event)

  override def setTcsEnabled(
    obsId:    Observation.Id,
    user:     User,
    enabled:  SubsystemEnabled,
    clientId: ClientId
  ): F[Unit] =
    toggleOverride(
      ObservatorySubsystem.Tcs.tag,
      (enabled, x) => if (enabled.value) x.enableTcs else x.disableTcs,
      SetTcsEnabled(obsId, user.some, enabled),
      obsId,
      user,
      enabled,
      clientId
    )

  override def setGcalEnabled(
    obsId:    Observation.Id,
    user:     User,
    enabled:  SubsystemEnabled,
    clientId: ClientId
  ): F[Unit] =
    toggleOverride(
      ObservatorySubsystem.Gcal.tag,
      (enabled, x) => if (enabled.value) x.enableGcal else x.disableGcal,
      SetGcalEnabled(obsId, user.some, enabled),
      obsId,
      user,
      enabled,
      clientId
    )

  override def setInstrumentEnabled(
    obsId:    Observation.Id,
    user:     User,
    enabled:  SubsystemEnabled,
    clientId: ClientId
  ): F[Unit] =
    toggleOverride(
      "Instrument",
      (enabled, x) => if (enabled.value) x.enableInstrument else x.disableInstrument,
      SetInstrumentEnabled(obsId, user.some, enabled),
      obsId,
      user,
      enabled,
      clientId
    )

  override def setDhsEnabled(
    obsId:    Observation.Id,
    user:     User,
    enabled:  SubsystemEnabled,
    clientId: ClientId
  ): F[Unit] =
    toggleOverride(
      "DHS",
      (enabled, x) => if (enabled.value) x.enableDhs else x.disableDhs,
      SetDhsEnabled(obsId, user.some, enabled),
      obsId,
      user,
      enabled,
      clientId: ClientId
    )

}

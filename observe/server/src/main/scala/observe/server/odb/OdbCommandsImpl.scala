// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.odb

import cats.Endo
import cats.effect.Async
import cats.effect.Sync
import cats.effect.kernel.Ref
import cats.effect.std.Mutex
import cats.effect.std.UUIDGen
import cats.syntax.all.*
import clue.FetchClientWithPars
import clue.data.syntax.*
import clue.syntax.*
import lucuma.core.enums.DatasetStage
import lucuma.core.enums.SequenceCommand
import lucuma.core.enums.StepStage
import lucuma.core.model.Observation
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.core.util.IdempotencyKey
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Scalars.VisitId
import lucuma.schemas.ObservationDB.Types.AddDatasetEventInput
import lucuma.schemas.ObservationDB.Types.AddEventBatchEntryInput
import lucuma.schemas.ObservationDB.Types.AddStepEventInput
import lucuma.schemas.odb.input.clientTimeNow
import observe.common.EventsGQL.*
import observe.model.dhs.*
import observe.model.odb.ObsRecordedIds
import org.http4s.Header
import org.http4s.Request
import org.http4s.headers.`Idempotency-Key`
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.syntax.*

case class OdbCommandsImpl[F[_]: {UUIDGen as U, Logger as L, Async as F}](
  idTracker:     Ref[F, ObsRecordedIds],
  eventBatching: Boolean,
  pendingEvents: Ref[F, PendingEvents],
  flushMutex:    Mutex[F]
)(using client: FetchClientWithPars[F, Request[F], ObservationDB]) extends OdbCommands[F]
    with IdTrackerOps[F](idTracker)
    with OdbEventBufferOps[F](pendingEvents, flushMutex) {

  private val FitsFileExtension: String = ".fits"

  private def normalizeFilename(fileName: String): String =
    if (fileName.endsWith(FitsFileExtension)) fileName
    else fileName + FitsFileExtension

  private def newIdempotencyKey: F[IdempotencyKey] =
    UUIDGen[F].randomUUID.map(IdempotencyKey(_))

  // The `Idempotency-Key` header marks the request safe for HTTP-level retry.
  private def addIdempotencyKey(idempotencyKey: IdempotencyKey): Endo[Request[F]] = req =>
    req.putHeaders(`Idempotency-Key`(idempotencyKey.toString))

  override def visitStart(obsId: Observation.Id): F[Unit] =
    for
      _   <- debug"Record visit for obsId: [$obsId]"
      vId <- recordVisit(obsId)
      _   <- setCurrentVisitId(obsId, vId.some)
    yield ()

  override def sequenceStart(obsId: Observation.Id): F[Unit] =
    for
      _              <- flushEvents(obsId).whenA(eventBatching)
      visitId        <- getCurrentVisitId(obsId)
      _              <- debug"Send ODB event sequenceStart for obsId: $obsId, visitId: $visitId"
      idempotencyKey <- newIdempotencyKey
      clientTime     <- clientTimeNow
      _              <-
        AddSequenceEventMutation[F]
          .execute(
            visitId,
            SequenceCommand.Start,
            idempotencyKey,
            clientTime,
            addIdempotencyKey(idempotencyKey)
          )
      _              <- debug"ODB event sequenceStart sent for obsId: $obsId"
    yield ()

  private def recordStepEvent(
    obsId:  Observation.Id,
    stepId: Step.Id,
    stage:  StepStage
  ): F[Boolean] =
    for
      visitId        <- getCurrentVisitId(obsId)
      _              <- debug"Send ODB event $stage for obsId: $obsId, step $stepId"
      idempotencyKey <- newIdempotencyKey
      clientTime     <- clientTimeNow
      _              <- AddStepEventMutation[F]
                          .execute(
                            stepId,
                            visitId,
                            stage,
                            idempotencyKey,
                            clientTime,
                            addIdempotencyKey(idempotencyKey)
                          )
      _              <- debug"ODB event for step $stage sent"
    yield true

  private def bufferStepEvent(
    obsId:  Observation.Id,
    stepId: Step.Id,
    stage:  StepStage
  ): F[Boolean] =
    for
      visitId        <- getCurrentVisitId(obsId)
      idempotencyKey <- newIdempotencyKey
      clientTime     <- clientTimeNow
      _              <- appendEvent(
                          obsId,
                          AddEventBatchEntryInput(step =
                            AddStepEventInput(
                              stepId,
                              visitId,
                              stage,
                              clientTime = clientTime,
                              idempotencyKey = idempotencyKey.assign
                            ).assign
                          )
                        )
      _              <- debug"Buffered ODB event $stage for obsId: $obsId, step $stepId"
    yield true

  // Intermediate step events buffer; step-terminating events (EndStep, Abort, Stop, Pause,
  // Continue) buffer and flush.
  private def stepEvent(
    obsId:      Observation.Id,
    stepId:     Step.Id,
    stage:      StepStage,
    flushAfter: Boolean
  ): F[Boolean] =
    if (eventBatching)
      bufferStepEvent(obsId, stepId, stage) <* flushEvents(obsId).whenA(flushAfter)
    else
      recordStepEvent(obsId, stepId, stage)

  override def stepStartStep[D](obsId: Observation.Id, stepId: Step.Id): F[Unit] =
    if (eventBatching)
      // START_STEP is never buffered: the ODB refuses datasets for a step with no recorded
      // event, and this anchors the step's record before anything else can fail. 
      // Also explore needs the starte event to update the UI
      for
        pending        <- pendingEventCount(obsId)
        _              <- (warn"Flushing $pending stale buffered ODB events for obsId: $obsId")
                            .whenA(pending > 0)
        _              <- flushEvents(obsId)
        visitId        <- getCurrentVisitId(obsId)
        _              <- debug"Send ODB event ${StepStage.StartStep} for obsId: $obsId, step $stepId"
        idempotencyKey <- newIdempotencyKey
        clientTime     <- clientTimeNow
        _              <- AddStepEventMutation[F]
                            .execute(
                              stepId,
                              visitId,
                              StepStage.StartStep,
                              idempotencyKey,
                              clientTime,
                              addIdempotencyKey(idempotencyKey)
                            )
                            .raiseGraphQLErrors
      yield ()
    else
      recordStepEvent(obsId, stepId, StepStage.StartStep).void

  override def stepStartConfigure(obsId: Observation.Id, stepId: Step.Id): F[Unit] =
    stepEvent(obsId, stepId, StepStage.StartConfigure, flushAfter = false).void

  override def stepEndConfigure(obsId: Observation.Id, stepId: Step.Id): F[Boolean] =
    stepEvent(obsId, stepId, StepStage.EndConfigure, flushAfter = false)

  override def stepStartObserve(obsId: Observation.Id, stepId: Step.Id): F[Boolean] =
    stepEvent(obsId, stepId, StepStage.StartObserve, flushAfter = false)

  override def datasetStartExposure(
    obsId:  Observation.Id,
    stepId: Step.Id,
    fileId: ImageFileId
  ): F[RecordDatasetMutation.Data.RecordDataset.Dataset] =
    for
      _              <- debug"Send ODB event datasetStartExposure for obsId: $obsId, stepId: $stepId with fileId: $fileId"
      visitId        <- getCurrentVisitId(obsId)
      dataset        <- recordDataset(stepId, visitId, fileId)
      _              <- setCurrentDatasetId(obsId, fileId, dataset.id.some)
      _              <- debug"Recorded dataset id ${dataset.id}"
      idempotencyKey <- newIdempotencyKey
      clientTime     <- clientTimeNow
      _              <-
        if (eventBatching)
          appendEvent(
            obsId,
            AddEventBatchEntryInput(dataset =
              AddDatasetEventInput(
                dataset.id,
                DatasetStage.StartExpose,
                clientTime = clientTime,
                idempotencyKey = idempotencyKey.assign
              ).assign
            )
          )
        else
          AddDatasetEventMutation[F]
            .execute(
              dataset.id,
              DatasetStage.StartExpose,
              idempotencyKey,
              clientTime,
              addIdempotencyKey(idempotencyKey)
            )
            .void
      _              <- debug"ODB event datasetStartExposure sent"
    yield dataset

  private def recordDatasetEvent(
    obsId:  Observation.Id,
    fileId: ImageFileId,
    stage:  DatasetStage
  ): F[Boolean] =
    for
      datasetId      <- getCurrentDatasetId(obsId, fileId)
      _              <- debug"Send ODB event $stage for obsId: $obsId datasetId: $datasetId"
      idempotencyKey <- newIdempotencyKey
      clientTime     <- clientTimeNow
      _              <- AddDatasetEventMutation[F]
                          .execute(
                            datasetId,
                            stage,
                            idempotencyKey,
                            clientTime,
                            addIdempotencyKey(idempotencyKey)
                          )
      _              <- debug"ODB event for dataset $stage sent"
    yield true

  private def bufferDatasetEvent(
    obsId:  Observation.Id,
    fileId: ImageFileId,
    stage:  DatasetStage
  ): F[Boolean] =
    for
      datasetId      <- getCurrentDatasetId(obsId, fileId)
      idempotencyKey <- newIdempotencyKey
      clientTime     <- clientTimeNow
      _              <- appendEvent(
                          obsId,
                          AddEventBatchEntryInput(dataset =
                            AddDatasetEventInput(
                              datasetId,
                              stage,
                              clientTime = clientTime,
                              idempotencyKey = idempotencyKey.assign
                            ).assign
                          )
                        )
      _              <- debug"Buffered ODB event $stage for obsId: $obsId datasetId: $datasetId"
    yield true

  private def datasetEvent(
    obsId:  Observation.Id,
    fileId: ImageFileId,
    stage:  DatasetStage
  ): F[Boolean] =
    if (eventBatching) bufferDatasetEvent(obsId, fileId, stage)
    else recordDatasetEvent(obsId, fileId, stage)

  override def datasetEndExposure(obsId: Observation.Id, fileId: ImageFileId): F[Boolean] =
    datasetEvent(obsId, fileId, DatasetStage.EndExpose)

  override def datasetStartReadout(obsId: Observation.Id, fileId: ImageFileId): F[Boolean] =
    datasetEvent(obsId, fileId, DatasetStage.StartReadout)

  override def datasetEndReadout(obsId: Observation.Id, fileId: ImageFileId): F[Boolean] =
    datasetEvent(obsId, fileId, DatasetStage.EndReadout)

  override def datasetStartWrite(obsId: Observation.Id, fileId: ImageFileId): F[Boolean] =
    datasetEvent(obsId, fileId, DatasetStage.StartWrite)

  override def datasetEndWrite(obsId: Observation.Id, fileId: ImageFileId): F[Boolean] =
    for
      result <- datasetEvent(obsId, fileId, DatasetStage.EndWrite)
      _      <- setCurrentDatasetId(obsId, fileId, none)
    yield result

  override def stepEndObserve(obsId: Observation.Id, stepId: Step.Id): F[Boolean] =
    stepEvent(obsId, stepId, StepStage.EndObserve, flushAfter = false)

  override def stepEndStep(obsId: Observation.Id, stepId: Step.Id): F[Boolean] =
    stepEvent(obsId, stepId, StepStage.EndStep, flushAfter = true)

  override def stepAbort(obsId: Observation.Id, stepId: Step.Id): F[Boolean] =
    stepEvent(obsId, stepId, StepStage.Abort, flushAfter = true)

  override def stepStop(obsId: Observation.Id, stepId: Step.Id): F[Boolean] =
    stepEvent(obsId, stepId, StepStage.Stop, flushAfter = true)

  override def stepPause(obsId: Observation.Id, stepId: Step.Id): F[Boolean] =
    stepEvent(obsId, stepId, StepStage.Pause, flushAfter = true)

  override def stepContinue(obsId: Observation.Id, stepId: Step.Id): F[Boolean] =
    stepEvent(obsId, stepId, StepStage.Continue, flushAfter = true)

  private def recordSequenceEvent(
    obsId:           Observation.Id,
    sequenceCommand: SequenceCommand
  ): F[Boolean] =
    for
      _              <- flushEvents(obsId).whenA(eventBatching)
      _              <- debug"Send ODB event $sequenceCommand for obsId: $obsId"
      visitId        <- getCurrentVisitId(obsId)
      idempotencyKey <- newIdempotencyKey
      clientTime     <- clientTimeNow
      _              <- AddSequenceEventMutation[F]
                          .execute(
                            visitId,
                            sequenceCommand,
                            idempotencyKey,
                            clientTime,
                            addIdempotencyKey(idempotencyKey)
                          )
      _              <- debug"ODB event for sequence $sequenceCommand sent"
    yield true

  override def obsContinue(obsId: Observation.Id): F[Boolean] =
    recordSequenceEvent(obsId, SequenceCommand.Continue)

  override def obsPause(obsId: Observation.Id): F[Boolean] =
    recordSequenceEvent(obsId, SequenceCommand.Pause)

  override def obsStop(obsId: Observation.Id): F[Boolean] =
    for
      result <- recordSequenceEvent(obsId, SequenceCommand.Stop)
      _      <- setCurrentVisitId(obsId, none)
    yield result

  def flushAllPending: F[Unit] = flushAllPendingEvents

  private def recordVisit(
    obsId: Observation.Id
  ): F[VisitId] =
    for
      idempotencyKey <- newIdempotencyKey
      clientTime     <- clientTimeNow
      result         <- RecordVisitMutation[F]
                          .execute(
                            obsId,
                            idempotencyKey,
                            clientTime,
                            addIdempotencyKey(idempotencyKey)
                          )
                          .raiseGraphQLErrors
    yield result.recordVisit.visit.id

  private def recordDataset(
    stepId:  Step.Id,
    visitId: Visit.Id,
    fileId:  ImageFileId
  ): F[RecordDatasetMutation.Data.RecordDataset.Dataset] =
    Sync[F]
      .delay(Dataset.Filename.parse(normalizeFilename(fileId.value)).get)
      .flatMap: fileName =>
        newIdempotencyKey.flatMap: idempotencyKey =>
          RecordDatasetMutation[F]
            .execute(stepId, visitId, fileName, idempotencyKey, addIdempotencyKey(idempotencyKey))
            .raiseGraphQLErrors
            .map(_.recordDataset.dataset)

  override def getCurrentRecordedIds: F[ObsRecordedIds] = idTracker.get
}

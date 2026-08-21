// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.odb

import cats.Endo
import cats.MonadThrow
import cats.data.Ior
import cats.effect.Sync
import cats.effect.kernel.Ref
import cats.effect.std.UUIDGen
import cats.syntax.all.*
import clue.FetchClientWithPars
import clue.ResponseException
import clue.model.GraphQLResponse
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
import lucuma.schemas.odb.input.clientTimeNow
import observe.common.EventsGQL.*
import observe.model.dhs.*
import observe.model.odb.ObsRecordedIds
import org.http4s.Header
import org.http4s.Request
import org.http4s.headers.`Idempotency-Key`
import org.typelevel.log4cats.Logger

/**
 * Events are handed over to `eventSender`, which sends them in the background: the sequence doesn't
 * pay for the round trip. The commands that terminate a step (or the sequence) flush the pending
 * events, so that everything a step produced is in the ODB before the next step starts. Any send
 * failure surfaces there.
 *
 * Everything the ODB needs to be told about an event (visit id, dataset id, client time,
 * idempotency key) is resolved before submitting, so that a background send is unaffected by later
 * state changes.
 */
case class OdbCommandsImpl[F[_]: UUIDGen](
  idTracker:   Ref[F, ObsRecordedIds],
  eventSender: OdbEventSender[F]
)(using client: FetchClientWithPars[F, Request[F], ObservationDB])(using
  val F:       Sync[F],
  L:           Logger[F]
) extends OdbCommands[F]
    with IdTrackerOps[F](idTracker) {

  private val FitsFileExtension: String                   = ".fits"
  private def normalizeFilename(fileName: String): String =
    if (fileName.endsWith(FitsFileExtension)) fileName
    else fileName + FitsFileExtension

  private def newIdempotencyKey: F[IdempotencyKey] =
    UUIDGen[F].randomUUID.map(IdempotencyKey(_))

  // We use the default retry policy in the http4s client. For it to kick in
  // we need to add the `Idempotency-Key` header to non-GET requests.
  private def addIdempotencyKey(idempotencyKey: IdempotencyKey): Endo[Request[F]] = req =>
    req.putHeaders(`Idempotency-Key`(idempotencyKey.toString))

  override def flushEvents(obsId: Observation.Id): F[Unit] =
    L.debug(s"Awaiting pending ODB events for obsId: $obsId") >>
      eventSender.flush(obsId) >>
      L.debug(s"All ODB events acknowledged for obsId: $obsId")

  /** Submits an event mutation to be sent in the background, checking that the ODB recorded it. */
  private def submitEvent[D](obsId: Observation.Id, description: String)(
    mutation: F[GraphQLResponse[D]]
  ): F[Unit] =
    eventSender.submit(obsId, description, checked(description)(mutation))

  private def checked[D](description: String)(mutation: F[GraphQLResponse[D]]): F[Unit] =
    mutation.flatMap(OdbCommandsImpl.checkEventRecorded(description))

  override def visitStart(obsId: Observation.Id): F[Unit] =
    for
      _   <- L.debug(s"Record visit for obsId: [$obsId]")
      vId <- recordVisit(obsId)
      _   <- setCurrentVisitId(obsId, vId.some)
    yield ()

  override def sequenceStart(obsId: Observation.Id): F[Unit] =
    recordSequenceEvent(obsId, SequenceCommand.Start).void

  private def recordStepEvent(
    obsId:  Observation.Id,
    stepId: Step.Id,
    stage:  StepStage
  ): F[Boolean] =
    for
      visitId        <- getCurrentVisitId(obsId)
      _              <- L.debug(s"Queue ODB event $stage for obsId: $obsId, step $stepId")
      idempotencyKey <- newIdempotencyKey
      clientTime     <- clientTimeNow
      _              <- submitEvent(obsId, s"step $stage for step $stepId"):
                          AddStepEventMutation[F]
                            .execute(
                              stepId,
                              visitId,
                              stage,
                              idempotencyKey,
                              clientTime,
                              addIdempotencyKey(idempotencyKey)
                            )
    yield true

  override def stepStartStep[D](obsId: Observation.Id, stepId: Step.Id): F[Unit] =
    recordStepEvent(obsId, stepId, StepStage.StartStep).void

  override def stepStartConfigure(obsId: Observation.Id, stepId: Step.Id): F[Unit] =
    recordStepEvent(obsId, stepId, StepStage.StartConfigure).void

  override def stepEndConfigure(obsId: Observation.Id, stepId: Step.Id): F[Boolean] =
    recordStepEvent(obsId, stepId, StepStage.EndConfigure)

  override def stepStartObserve(obsId: Observation.Id, stepId: Step.Id): F[Boolean] =
    recordStepEvent(obsId, stepId, StepStage.StartObserve)

  override def datasetStartExposure(
    obsId:  Observation.Id,
    stepId: Step.Id,
    fileId: ImageFileId
  ): F[RecordDatasetMutation.Data.RecordDataset.Dataset] =
    for
      _       <- L.debug:
                   s"Send ODB event datasetStartExposure for obsId: $obsId, stepId: $stepId with fileId: $fileId"
      visitId <- getCurrentVisitId(obsId)
      dataset <- recordDataset(stepId, visitId, fileId)
      _       <- setCurrentDatasetId(obsId, fileId, dataset.id.some)
      _       <- L.debug(s"Recorded dataset id ${dataset.id}")
      _       <- submitDatasetEvent(obsId, dataset.id, DatasetStage.StartExpose)
    yield dataset

  private def submitDatasetEvent(
    obsId:     Observation.Id,
    datasetId: Dataset.Id,
    stage:     DatasetStage
  ): F[Unit] =
    for
      idempotencyKey <- newIdempotencyKey
      clientTime     <- clientTimeNow
      _              <- submitEvent(obsId, s"dataset $stage for dataset $datasetId"):
                          AddDatasetEventMutation[F]
                            .execute(
                              datasetId,
                              stage,
                              idempotencyKey,
                              clientTime,
                              addIdempotencyKey(idempotencyKey)
                            )
    yield ()

  private def recordDatasetEvent(
    obsId:  Observation.Id,
    fileId: ImageFileId,
    stage:  DatasetStage
  ): F[Boolean] =
    for
      datasetId <- getCurrentDatasetId(obsId, fileId)
      _         <- L.debug(s"Queue ODB event $stage for obsId: $obsId datasetId: $datasetId")
      _         <- submitDatasetEvent(obsId, datasetId, stage)
    yield true

  override def datasetEndExposure(obsId: Observation.Id, fileId: ImageFileId): F[Boolean] =
    recordDatasetEvent(obsId, fileId, DatasetStage.EndExpose)

  override def datasetStartReadout(obsId: Observation.Id, fileId: ImageFileId): F[Boolean] =
    recordDatasetEvent(obsId, fileId, DatasetStage.StartReadout)

  override def datasetEndReadout(obsId: Observation.Id, fileId: ImageFileId): F[Boolean] =
    recordDatasetEvent(obsId, fileId, DatasetStage.EndReadout)

  override def datasetStartWrite(obsId: Observation.Id, fileId: ImageFileId): F[Boolean] =
    recordDatasetEvent(obsId, fileId, DatasetStage.StartWrite)

  override def datasetEndWrite(obsId: Observation.Id, fileId: ImageFileId): F[Boolean] =
    for
      result <- recordDatasetEvent(obsId, fileId, DatasetStage.EndWrite)
      _      <- setCurrentDatasetId(obsId, fileId, none)
    yield result

  override def stepEndObserve(obsId: Observation.Id, stepId: Step.Id): F[Boolean] =
    recordStepEvent(obsId, stepId, StepStage.EndObserve)

  override def stepEndStep(obsId: Observation.Id, stepId: Step.Id): F[Boolean] =
    recordStepEvent(obsId, stepId, StepStage.EndStep) <* flushEvents(obsId)

  override def stepAbort(obsId: Observation.Id, stepId: Step.Id): F[Boolean] =
    recordStepEvent(obsId, stepId, StepStage.Abort) <* flushEvents(obsId)

  override def stepStop(obsId: Observation.Id, stepId: Step.Id): F[Boolean] =
    recordStepEvent(obsId, stepId, StepStage.Stop) <* flushEvents(obsId)

  override def stepPause(obsId: Observation.Id, stepId: Step.Id): F[Boolean] =
    recordStepEvent(obsId, stepId, StepStage.Pause) <* flushEvents(obsId)

  override def stepContinue(obsId: Observation.Id, stepId: Step.Id): F[Boolean] =
    recordStepEvent(obsId, stepId, StepStage.Continue)

  private def recordSequenceEvent(
    obsId:           Observation.Id,
    sequenceCommand: SequenceCommand
  ): F[Boolean] =
    for
      _              <- L.debug(s"Queue ODB event $sequenceCommand for obsId: $obsId")
      visitId        <- getCurrentVisitId(obsId)
      idempotencyKey <- newIdempotencyKey
      clientTime     <- clientTimeNow
      _              <- submitEvent(obsId, s"sequence $sequenceCommand"):
                          AddSequenceEventMutation[F]
                            .execute(
                              visitId,
                              sequenceCommand,
                              idempotencyKey,
                              clientTime,
                              addIdempotencyKey(idempotencyKey)
                            )
    yield true

  override def obsContinue(obsId: Observation.Id): F[Boolean] =
    recordSequenceEvent(obsId, SequenceCommand.Continue)

  override def obsPause(obsId: Observation.Id): F[Boolean] =
    recordSequenceEvent(obsId, SequenceCommand.Pause) <* flushEvents(obsId)

  override def obsStop(obsId: Observation.Id): F[Boolean] =
    for
      result <- recordSequenceEvent(obsId, SequenceCommand.Stop)
      _      <- flushEvents(obsId)
      _      <- setCurrentVisitId(obsId, none)
    yield result

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

object OdbCommandsImpl:

  /**
   * Checks that the ODB recorded an event. A response without data means it didn't, which is a hard
   * failure. Errors that come along with data mean the event was recorded, so those are only
   * logged: failing a step over them would be worse than the warning they carry.
   */
  private[odb] def checkEventRecorded[F[_]: {MonadThrow as F, Logger}, D](
    description: String
  )(response: GraphQLResponse[D]): F[Unit] =
    response.result match
      case Ior.Right(_)        => F.unit
      case Ior.Both(errors, _) =>
        Logger[F].warn:
          s"ODB reported errors recording $description: ${errors.map(_.message).mkString_("; ")}"
      case Ior.Left(errors)    => F.raiseError(ResponseException(errors, none))

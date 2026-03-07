// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.odb

import cats.Endo
import cats.effect.Sync
import cats.effect.kernel.Ref
import cats.effect.std.UUIDGen
import cats.syntax.all.*
import clue.FetchClientWithPars
import clue.syntax.*
import lucuma.core.enums.DatasetStage
import lucuma.core.enums.SequenceCommand
import lucuma.core.enums.StepStage
import lucuma.core.model.Observation
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.core.model.sequence.gmos
import lucuma.core.util.IdempotencyKey
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Scalars.VisitId
import lucuma.schemas.odb.input.*
import observe.common.EventsGQL.*
import observe.model.dhs.*
import observe.model.odb.ObsRecordedIds
import org.http4s.Header
import org.http4s.Request
import org.http4s.headers.`Idempotency-Key`
import org.typelevel.log4cats.Logger

case class OdbCommandsImpl[F[_]: UUIDGen](
  idTracker: Ref[F, ObsRecordedIds]
)(using client: FetchClientWithPars[F, Request[F], ObservationDB])(using
  val F:     Sync[F],
  L:         Logger[F]
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

  override def visitStart[S](obsId: Observation.Id, staticCfg: S): F[Unit] =
    for
      _   <- L.debug(s"Record visit for obsId: [$obsId]")
      vId <- recordVisit(obsId, staticCfg)
      _   <- setCurrentVisitId(obsId, vId.some)
    yield ()

  override def sequenceStart(obsId: Observation.Id): F[Unit] =
    for
      visitId        <- getCurrentVisitId(obsId)
      _              <- L.debug(s"Send ODB event sequenceStart for obsId: $obsId, visitId: $visitId")
      idempotencyKey <- newIdempotencyKey
      _              <-
        AddSequenceEventMutation[F]
          .execute(
            visitId,
            SequenceCommand.Start,
            idempotencyKey,
            addIdempotencyKey(idempotencyKey)
          )
      _              <- L.debug(s"ODB event sequenceStart sent for obsId: $obsId")
    yield ()

  private def recordStepEvent(
    obsId:  Observation.Id,
    stepId: Step.Id,
    stage:  StepStage
  ): F[Boolean] =
    for
      visitId        <- getCurrentVisitId(obsId)
      _              <- L.debug(s"Send ODB event $stage for obsId: $obsId, step $stepId")
      idempotencyKey <- newIdempotencyKey
      _              <- AddStepEventMutation[F]
                          .execute(
                            stepId,
                            visitId,
                            stage,
                            idempotencyKey,
                            addIdempotencyKey(idempotencyKey)
                          )
      _              <- L.debug(s"ODB event for step $stage sent")
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
      _              <- L.debug:
                          s"Send ODB event datasetStartExposure for obsId: $obsId, stepId: $stepId with fileId: $fileId"
      visitId        <- getCurrentVisitId(obsId)
      dataset        <- recordDataset(stepId, visitId, fileId)
      _              <- setCurrentDatasetId(obsId, fileId, dataset.id.some)
      _              <- L.debug(s"Recorded dataset id ${dataset.id}")
      idempotencyKey <- newIdempotencyKey
      _              <- AddDatasetEventMutation[F]
                          .execute(
                            dataset.id,
                            DatasetStage.StartExpose,
                            idempotencyKey,
                            addIdempotencyKey(idempotencyKey)
                          )
      _              <- L.debug("ODB event datasetStartExposure sent")
    yield dataset

  private def recordDatasetEvent(
    obsId:  Observation.Id,
    fileId: ImageFileId,
    stage:  DatasetStage
  ): F[Boolean] =
    for
      datasetId      <- getCurrentDatasetId(obsId, fileId)
      _              <- L.debug(s"Send ODB event $stage for obsId: $obsId datasetId: $datasetId")
      idempotencyKey <- newIdempotencyKey
      _              <- AddDatasetEventMutation[F]
                          .execute(
                            datasetId,
                            stage,
                            idempotencyKey,
                            addIdempotencyKey(idempotencyKey)
                          )
      _              <- L.debug(s"ODB event for dataset $stage sent")
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
    recordStepEvent(obsId, stepId, StepStage.EndStep)

  override def stepAbort(obsId: Observation.Id, stepId: Step.Id): F[Boolean] =
    recordStepEvent(obsId, stepId, StepStage.Abort)

  override def stepStop(obsId: Observation.Id, stepId: Step.Id): F[Boolean] =
    recordStepEvent(obsId, stepId, StepStage.Stop)

  override def stepPause(obsId: Observation.Id, stepId: Step.Id): F[Boolean] =
    recordStepEvent(obsId, stepId, StepStage.Pause)

  override def stepContinue(obsId: Observation.Id, stepId: Step.Id): F[Boolean] =
    recordStepEvent(obsId, stepId, StepStage.Continue)

  private def recordSequenceEvent(
    obsId:           Observation.Id,
    sequenceCommand: SequenceCommand
  ): F[Boolean] =
    for
      _              <- L.debug(s"Send ODB event $sequenceCommand for obsId: $obsId")
      visitId        <- getCurrentVisitId(obsId)
      idempotencyKey <- newIdempotencyKey
      _              <- AddSequenceEventMutation[F]
                          .execute(
                            visitId,
                            sequenceCommand,
                            idempotencyKey,
                            addIdempotencyKey(idempotencyKey)
                          )
      _              <- L.debug(s"ODB event for sequence $sequenceCommand sent")
    yield true

  override def obsContinue(obsId: Observation.Id): F[Boolean] =
    recordSequenceEvent(obsId, SequenceCommand.Continue)

  override def obsPause(obsId: Observation.Id, reason: String): F[Boolean] =
    recordSequenceEvent(obsId, SequenceCommand.Pause)

  override def obsStop(obsId: Observation.Id): F[Boolean] =
    for
      result <- recordSequenceEvent(obsId, SequenceCommand.Stop)
      _      <- setCurrentVisitId(obsId, none)
    yield result

  private def recordVisit[S](
    obsId:     Observation.Id,
    staticCfg: S
  ): F[VisitId] = staticCfg match
    case s @ gmos.StaticConfig.GmosNorth(_, _, _, _) => recordGmosNorthVisit(obsId, s)
    case s @ gmos.StaticConfig.GmosSouth(_, _, _, _) => recordGmosSouthVisit(obsId, s)
    case s @ Flamingos2StaticConfig(_, _)            => recordFlamingos2Visit(obsId, s)

  private def recordGmosNorthVisit(
    obsId:     Observation.Id,
    staticCfg: gmos.StaticConfig.GmosNorth
  ): F[VisitId] =
    newIdempotencyKey.flatMap: idempotencyKey =>
      RecordGmosNorthVisitMutation[F]
        .execute(obsId, staticCfg.toInput, idempotencyKey, addIdempotencyKey(idempotencyKey))
        .raiseGraphQLErrors
        .map(_.recordGmosNorthVisit.visit.id)

  private def recordGmosSouthVisit(
    obsId:     Observation.Id,
    staticCfg: gmos.StaticConfig.GmosSouth
  ): F[VisitId] =
    newIdempotencyKey.flatMap: idempotencyKey =>
      RecordGmosSouthVisitMutation[F]
        .execute(obsId, staticCfg.toInput, idempotencyKey, addIdempotencyKey(idempotencyKey))
        .raiseGraphQLErrors
        .map(_.recordGmosSouthVisit.visit.id)

  private def recordFlamingos2Visit(
    obsId:     Observation.Id,
    staticCfg: Flamingos2StaticConfig
  ): F[VisitId] =
    newIdempotencyKey.flatMap: idempotencyKey =>
      RecordFlamingos2VisitMutation[F]
        .execute(obsId, staticCfg.toInput, idempotencyKey, addIdempotencyKey(idempotencyKey))
        .raiseGraphQLErrors
        .map(_.recordFlamingos2Visit.visit.id)

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

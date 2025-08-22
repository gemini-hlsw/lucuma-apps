// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.services

import cats.effect.IO
import clue.*
import clue.data.syntax.*
import crystal.ViewF
import lucuma.core.model.sequence.Dataset
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.SequenceQueriesGQL
import lucuma.ui.sequence.SequenceData
import observe.queries.VisitQueriesGQL
import observe.ui.model.EditableQaFields
import observe.ui.model.LoadedObservation
import org.typelevel.log4cats.Logger

case class ODBQueryApiImpl(nighttimeObservation: ViewF[IO, Option[LoadedObservation]])(using
  FetchClient[IO, ObservationDB],
  Logger[IO]
) extends ODBQueryApi[IO]:

  override def refreshNighttimeVisits: IO[Unit] =
    nighttimeObservation.toOptionView.fold(
      Logger[IO].error("refreshNighttimeVisits with undefined loaded observation")
    ): loadedObs =>
      VisitQueriesGQL
        .ObservationVisits[IO]
        .query(loadedObs.get.obsId, loadedObs.get.lastVisitId.orIgnore)
        .raiseGraphQLErrors
        .map(_.observation.flatMap(_.execution))
        .attempt
        .flatMap: visits =>
          loadedObs.mod(_.addVisits(visits))

  override def refreshNighttimeSequence: IO[Unit] =
    nighttimeObservation.toOptionView.fold(
      Logger[IO].error("refreshNighttimeSequence with undefined loaded observation")
    ): loadedObs =>
      SequenceQueriesGQL
        .SequenceQuery[IO]
        .query(loadedObs.get.obsId)
        .raiseGraphQLErrors
        .adaptError:
          case ResponseException(errors, _) =>
            Exception(errors.map(_.message).toList.mkString("\n"))
        .map(SequenceData.fromOdbResponse)
        .attempt
        .map:
          _.flatMap:
            _.toRight:
              Exception:
                s"Execution Configuration not defined for observation [${loadedObs.get.obsId}]"
        .flatMap: sequenceData =>
          nighttimeObservation.mod(_.map(_.withSequenceData(sequenceData)))

  override def updateDatasetQa(datasetId: Dataset.Id, qaFields: EditableQaFields): IO[Unit] =
    VisitQueriesGQL
      .UpdateDatasetQa[IO]
      .execute(datasetId, qaFields.qaState.orUnassign, qaFields.comment.orUnassign)
      .void
      .onError:
        case e => Logger[IO].error(e)(s"Error updating dataset QA state for $datasetId")

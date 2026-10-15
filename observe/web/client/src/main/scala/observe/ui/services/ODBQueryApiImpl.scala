// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.services

import cats.effect.IO
import clue.*
import clue.data.syntax.*
import crystal.react.View
import lucuma.core.enums.SmartGcalType
import lucuma.core.model.sequence.Step
import lucuma.core.syntax.display.*
import lucuma.react.primereact.Message
import lucuma.schemas.ObservationDB
import lucuma.schemas.model.ExecutionVisits
import lucuma.schemas.model.Visit
import lucuma.schemas.odb.SequenceQueriesGql
import lucuma.schemas.odb.VisitQueriesGQL
import lucuma.ui.primereact.ToastCtx
import lucuma.ui.sequence.SequenceData
import observe.model.Observation
import observe.queries.ObsQueriesGQL
import observe.ui.display.given
import observe.ui.model.ObservationRequests
import observe.ui.model.enums.OperationRequest

case class ODBQueryApiImpl(requests: View[Map[Observation.Id, ObservationRequests]])(using
  FetchClient[IO, ObservationDB],
  ToastCtx[IO]
) extends ODBQueryApi[IO]:
  override def queryVisits(
    obsId: Observation.Id,
    from:  Option[Visit.Id]
  ): IO[Option[ExecutionVisits]] =
    VisitQueriesGQL
      .ObservationVisits[IO]
      .query(obsId, from.orIgnore)
      .raiseGraphQLErrors
      .map(_.observation.flatMap(_.execution))

  override def querySequence(obsId: Observation.Id, includeItc: Boolean): IO[SequenceData] =
    SequenceQueriesGql
      .SequenceQuery[IO]
      .query(obsId, includeItc)
      .raiseGraphQLErrors
      .adaptError:
        case ResponseException(errors, _) =>
          Exception(errors.map(_.message).toList.mkString("\n"))
      .map(SequenceData.fromOdbResponse)
      .flatMap:
        _.fold(
          IO.raiseError(Exception(s"Execution Configuration not defined for observation [$obsId]"))
        )(IO.pure(_))

  override def insertSmartGcal(
    obsId:         Observation.Id,
    smartGcalType: SmartGcalType
  ): IO[Unit] =
    val insert: IO[List[Step.Id]] =
      ObsQueriesGQL
        .InsertSmartGcalMutation[IO]
        .execute(obsId, List(smartGcalType))
        .raiseGraphQLErrors
        .map(_.insertSmartGcal.stepIds)

    requests.modObservation(obsId)(
      ObservationRequests.insertSmartGcal.replace(OperationRequest.InFlight)
    ) >>
      insert
        .flatMap: stepIds =>
          requests.modObservation(obsId)(
            ObservationRequests.insertedSteps.replace(stepIds.toSet)
          ) >>
            ToastCtx[IO].showToast(
              s"Inserted ${stepIds.size} ${smartGcalType.shortName} step(s) into $obsId",
              Message.Severity.Success
            )
        .onError: e =>
          ToastCtx[IO].showToast(
            s"Error inserting ${smartGcalType.shortName} into $obsId: ${e.getMessage}",
            Message.Severity.Error,
            sticky = true
          )
        .guarantee(
          requests.modObservation(obsId)(
            ObservationRequests.insertSmartGcal.replace(OperationRequest.Idle)
          )
        )

  override def clearInsertedSteps(obsId: Observation.Id, stepIds: Set[Step.Id]): IO[Unit] =
    requests.modObservation(obsId)(ObservationRequests.insertedSteps.modify(_ -- stepIds))

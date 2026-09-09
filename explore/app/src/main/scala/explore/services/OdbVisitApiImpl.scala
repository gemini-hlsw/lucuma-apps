// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import cats.effect.Resource
import cats.effect.Sync
import cats.syntax.all.*
import clue.StreamingClient
import clue.syntax.*
import lucuma.core.enums.StepStage
import lucuma.core.model.Observation
import lucuma.schemas.ObservationDB
import lucuma.schemas.model.ExecutionVisits
import lucuma.schemas.odb.VisitQueriesGQL.ObservationVisits
import queries.common.ObsQueriesGQL.*
import queries.common.ObsQueriesGQL.StepEventSubscription.Data.ExecutionEventAdded

trait OdbVisitApiImpl[F[_]: Sync](using StreamingClient[F, ObservationDB]) extends OdbVisitApi[F]:
  def observationVisits(obsId: Observation.Id): F[Option[ExecutionVisits]] =
    ObservationVisits[F]
      .query(obsId)
      .raiseGraphQLErrors
      .map(_.observation.flatMap(_.execution))

  def stepEventSubscription(
    obsId: Observation.Id
  ): Resource[F, fs2.Stream[F, StepEventSubscription.Data]] =
    StepEventSubscription
      .subscribe[F](obsId)
      .raiseFirstNoDataError
      .ignoreGraphQLErrors
      .map:
        _.filter: data =>
          // `value` is polymorphic (ExecutionEvent); only StepEvents carry a stage.
          data.executionEventAdded.value match
            case ExecutionEventAdded.Value.stepEvent(v) =>
              List(StepStage.StartStep, StepStage.EndStep).contains_(v.stepStage)
            case _                                      => false

  def datasetEventSubscription(
    obsId: Observation.Id
  ): Resource[F, fs2.Stream[F, DatasetEditSubscription.Data]] =
    DatasetEditSubscription
      .subscribe[F](obsId)
      .raiseFirstNoDataError
      .ignoreGraphQLErrors

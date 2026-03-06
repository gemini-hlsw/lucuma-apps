// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.services

import cats.effect.IO
import clue.*
import clue.data.syntax.*
import lucuma.schemas.ObservationDB
import lucuma.schemas.model.ExecutionVisits
import lucuma.schemas.model.Visit
import lucuma.schemas.odb.SequenceQueriesGQL
import lucuma.schemas.odb.VisitQueriesGQL
import lucuma.ui.sequence.SequenceData
import observe.model.Observation

case class ODBQueryApiImpl()(using FetchClient[IO, ObservationDB]) extends ODBQueryApi[IO]:
  override def queryVisits(
    obsId: Observation.Id,
    from:  Option[Visit.Id]
  ): IO[Option[ExecutionVisits]] =
    VisitQueriesGQL
      .ObservationVisits[IO]
      .query(obsId, from.orIgnore)
      .raiseGraphQLErrors
      .map(_.observation.flatMap(_.execution))

  override def querySequence(obsId: Observation.Id): IO[SequenceData] =
    SequenceQueriesGQL
      .SequenceQuery[IO]
      .query(obsId)
      .raiseGraphQLErrors
      .adaptError:
        case ResponseException(errors, _) =>
          Exception(errors.map(_.message).toList.mkString("\n"))
      .map(SequenceData.fromOdbResponse)
      .flatMap:
        _.fold(
          IO.raiseError(Exception(s"Execution Configuration not defined for observation [$obsId]"))
        )(IO.pure(_))

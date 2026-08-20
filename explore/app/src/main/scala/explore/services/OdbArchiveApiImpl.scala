// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import cats.MonadThrow
import cats.syntax.all.*
import clue.StreamingClient
import clue.data.syntax.*
import explore.model.ArchiveDuplication
import explore.model.ArchiveMatch
import explore.model.Observation
import lucuma.core.model.Program
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.RefreshArchiveDuplicationInput
import lucuma.schemas.odb.input.*
import queries.common.ArchiveDuplicationQueriesGQL.*

trait OdbArchiveApiImpl[F[_]: MonadThrow](using StreamingClient[F, ObservationDB])
    extends OdbArchiveApi[F]:
  self: OdbApiHelper[F] =>

  def programArchiveDuplications(
    programId: Program.Id
  ): F[Map[Observation.Id, ArchiveDuplication]] =
    drain[
      ProgramArchiveDuplications.Data.Observations.Matches,
      Observation.Id,
      ProgramArchiveDuplications.Data.Observations
    ](
      offset =>
        ProgramArchiveDuplications[F]
          .query(programId.toWhereObservation, offset.orUnassign)
          .processNoDataErrors
          .map(_.observations),
      _.matches,
      _.hasMore,
      _.id
    ).map(_.map(m => m.id -> m.archiveDuplication).toMap)

  def observationArchiveMatches(obsId: Observation.Id): F[List[ArchiveMatch]] =
    ObservationArchiveMatches[F]
      .query(obsId)
      .processErrors
      .map(_.observation.foldMap(_.archiveDuplication.matches))

  def refreshArchiveDuplication(obsId: Observation.Id): F[ArchiveDuplication] =
    RefreshArchiveDuplication[F]
      .execute(RefreshArchiveDuplicationInput(observationId = obsId.assign))
      .processErrors
      .map(_.refreshArchiveDuplication.archiveDuplication)

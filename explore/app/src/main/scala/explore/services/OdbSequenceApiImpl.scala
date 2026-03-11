// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import cats.MonadThrow
import cats.syntax.all.*
import clue.*
import clue.FetchClient
import clue.data.Input
import clue.syntax.*
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.gmos
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.SequenceEditQueriesGql.*
import lucuma.schemas.odb.SequenceQueriesGQL.*
import lucuma.schemas.odb.input.*
import lucuma.ui.sequence.SequenceData

trait OdbSequenceApiImpl[F[_]: MonadThrow](using FetchClient[F, ObservationDB])
    extends OdbSequenceApi[F]:
  def sequenceData(obsId: Observation.Id, includeItc: Boolean): F[Option[SequenceData]] =
    SequenceQuery[F]
      .query(obsId, includeItc = Input(includeItc))
      .raiseGraphQLErrors
      .map(SequenceData.fromOdbResponse)

  def replaceGmosNorthSequence(
    obsId:        Observation.Id,
    sequenceType: SequenceType,
    atoms:        List[Atom[gmos.DynamicConfig.GmosNorth]]
  ): F[Unit] =
    ReplaceGmosNorthSequence[F]
      .execute(obsId, sequenceType, atoms.map(_.toInput))
      .raiseGraphQLErrors
      .void

  def replaceGmosSouthSequence(
    obsId:        Observation.Id,
    sequenceType: SequenceType,
    atoms:        List[Atom[gmos.DynamicConfig.GmosSouth]]
  ): F[Unit] =
    ReplaceGmosSouthSequence[F]
      .execute(obsId, sequenceType, atoms.map(_.toInput))
      .raiseGraphQLErrors
      .void

  def replaceFlamingos2Sequence(
    obsId:        Observation.Id,
    sequenceType: SequenceType,
    atoms:        List[Atom[Flamingos2DynamicConfig]]
  ): F[Unit] =
    ReplaceFlamingos2Sequence[F]
      .execute(obsId, sequenceType, atoms.map(_.toInput))
      .raiseGraphQLErrors
      .void

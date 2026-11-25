// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import cats.MonadThrow
import cats.syntax.all.*
import clue.FetchClient
import clue.syntax.*
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.gmos
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.SequenceEditQueriesGql.*
import lucuma.schemas.odb.input.*

/**
 * Replaces the remaining acquisition or science sequence of an observation in the ODB, returning
 * the sequence as stored. Every step gets a new id.
 */
trait SequenceEditApi[F[_]]:
  def replaceGmosNorthSequence(
    obsId:        Observation.Id,
    sequenceType: SequenceType,
    atoms:        List[Atom[gmos.DynamicConfig.GmosNorth]]
  ): F[List[Atom[gmos.DynamicConfig.GmosNorth]]]

  def replaceGmosSouthSequence(
    obsId:        Observation.Id,
    sequenceType: SequenceType,
    atoms:        List[Atom[gmos.DynamicConfig.GmosSouth]]
  ): F[List[Atom[gmos.DynamicConfig.GmosSouth]]]

  def replaceFlamingos2Sequence(
    obsId:        Observation.Id,
    sequenceType: SequenceType,
    atoms:        List[Atom[Flamingos2DynamicConfig]]
  ): F[List[Atom[Flamingos2DynamicConfig]]]

  def replaceIgrins2Sequence(
    obsId:        Observation.Id,
    sequenceType: SequenceType,
    atoms:        List[Atom[Igrins2DynamicConfig]]
  ): F[List[Atom[Igrins2DynamicConfig]]]

  def replaceGnirsSequence(
    obsId:        Observation.Id,
    sequenceType: SequenceType,
    atoms:        List[Atom[GnirsDynamicConfig]]
  ): F[List[Atom[GnirsDynamicConfig]]]

  def replaceGhostSequence(
    obsId:        Observation.Id,
    sequenceType: SequenceType,
    atoms:        List[Atom[GhostDynamicConfig]]
  ): F[List[Atom[GhostDynamicConfig]]]

trait SequenceEditApiImpl[F[_]: MonadThrow](using FetchClient[F, ObservationDB])
    extends SequenceEditApi[F]:
  def replaceGmosNorthSequence(
    obsId:        Observation.Id,
    sequenceType: SequenceType,
    atoms:        List[Atom[gmos.DynamicConfig.GmosNorth]]
  ): F[List[Atom[gmos.DynamicConfig.GmosNorth]]] =
    ReplaceGmosNorthSequence[F]
      .execute(obsId, sequenceType, atoms.map(_.toInput))
      .raiseGraphQLErrors
      .map(_.replaceGmosNorthSequence.sequence)

  def replaceGmosSouthSequence(
    obsId:        Observation.Id,
    sequenceType: SequenceType,
    atoms:        List[Atom[gmos.DynamicConfig.GmosSouth]]
  ): F[List[Atom[gmos.DynamicConfig.GmosSouth]]] =
    ReplaceGmosSouthSequence[F]
      .execute(obsId, sequenceType, atoms.map(_.toInput))
      .raiseGraphQLErrors
      .map(_.replaceGmosSouthSequence.sequence)

  def replaceFlamingos2Sequence(
    obsId:        Observation.Id,
    sequenceType: SequenceType,
    atoms:        List[Atom[Flamingos2DynamicConfig]]
  ): F[List[Atom[Flamingos2DynamicConfig]]] =
    ReplaceFlamingos2Sequence[F]
      .execute(obsId, sequenceType, atoms.map(_.toInput))
      .raiseGraphQLErrors
      .map(_.replaceFlamingos2Sequence.sequence)

  def replaceIgrins2Sequence(
    obsId:        Observation.Id,
    sequenceType: SequenceType,
    atoms:        List[Atom[Igrins2DynamicConfig]]
  ): F[List[Atom[Igrins2DynamicConfig]]] =
    ReplaceIgrins2Sequence[F]
      .execute(obsId, sequenceType, atoms.map(_.toInput))
      .raiseGraphQLErrors
      .map(_.replaceIgrins2Sequence.sequence)

  def replaceGnirsSequence(
    obsId:        Observation.Id,
    sequenceType: SequenceType,
    atoms:        List[Atom[GnirsDynamicConfig]]
  ): F[List[Atom[GnirsDynamicConfig]]] =
    ReplaceGnirsSequence[F]
      .execute(obsId, sequenceType, atoms.map(_.toInput))
      .raiseGraphQLErrors
      .map(_.replaceGnirsSequence.sequence)

  def replaceGhostSequence(
    obsId:        Observation.Id,
    sequenceType: SequenceType,
    atoms:        List[Atom[GhostDynamicConfig]]
  ): F[List[Atom[GhostDynamicConfig]]] =
    ReplaceGhostSequence[F]
      .execute(obsId, sequenceType, atoms.map(_.toInput))
      .raiseGraphQLErrors
      .map(_.replaceGhostSequence.sequence)

object SequenceEditApiImpl:
  def apply[F[_]: MonadThrow](using FetchClient[F, ObservationDB]): SequenceEditApi[F] =
    new SequenceEditApiImpl[F] {}

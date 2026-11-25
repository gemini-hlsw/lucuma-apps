// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import cats.MonadThrow
import cats.syntax.all.*
import clue.*
import clue.FetchClient
import clue.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.sequence.StepEstimate
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.SequenceEditQueriesGql.*
import lucuma.schemas.odb.SequenceQueriesGql.*
import lucuma.ui.sequence.SequenceData
import lucuma.ui.sequence.SequenceEditApiImpl

trait OdbSequenceApiImpl[F[_]: MonadThrow](using FetchClient[F, ObservationDB])
    extends OdbSequenceApi[F]
    with SequenceEditApiImpl[F]:
  def sequenceData(obsId: Observation.Id, includeItc: Boolean): F[Option[SequenceData]] =
    SequenceQuery[F]
      .query(obsId, includeItc)
      .raiseGraphQLErrors
      .map(SequenceData.fromOdbResponse)

  def ghostScienceStepEstimate(obsId: Observation.Id): F[Option[StepEstimate]] =
    GhostScienceStepEstimateQuery[F]
      .query(obsId)
      .raiseGraphQLErrors
      .map:
        _.executionConfig
          .flatMap(_.ghost)
          .flatMap(_.science)
          .flatMap(_.nextAtom.steps.headOption)
          .map(_.estimate)

  def deleteSequence(obsId: Observation.Id): F[Unit] =
    DeleteSequence[F]
      .execute(obsId)
      .raiseGraphQLErrors
      .void

  def resetAcquisition(obsId: Observation.Id, regenerate: Boolean): F[Unit] =
    ResetAcquisition[F]
      .execute(obsId, regenerate)
      .raiseGraphQLErrors
      .void

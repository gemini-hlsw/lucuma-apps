// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import lucuma.core.model.Observation
import lucuma.core.model.sequence.StepEstimate
import lucuma.ui.sequence.SequenceData
import lucuma.ui.sequence.SequenceEditApi

trait OdbSequenceApi[F[_]] extends SequenceEditApi[F]:
  def sequenceData(obsId: Observation.Id, includeItc: Boolean): F[Option[SequenceData]]

  def ghostScienceStepEstimate(obsId: Observation.Id): F[Option[StepEstimate]]

  def deleteSequence(obsId: Observation.Id): F[Unit]

  def resetAcquisition(obsId: Observation.Id, regenerate: Boolean): F[Unit]

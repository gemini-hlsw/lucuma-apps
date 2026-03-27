// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model

import cats.*
import cats.derived.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.model.sequence.Step
import monocle.Focus
import monocle.Optional
import observe.model.enums.ActionStatus
import observe.model.enums.Resource

case class SequenceView(
  obsId:           Observation.Id,
  metadata:        SequenceMetadata,
  status:          SequenceStatus,
  systemOverrides: SystemOverrides,
  sequenceType:    SequenceType,
  runningStep:     Option[ObserveStep],
  willStopIn:      Option[Int],
  stepResources:   Map[Resource | Instrument, ActionStatus],
  breakpoints:     Set[Step.Id]
) derives Eq:
  def pausedStep: Option[PausedStep] =
    runningStep.filter(_.isObservePaused).map(_.id).map(PausedStep(_))

object SequenceView:
  val runningStep: Optional[SequenceView, ObserveStep] =
    Focus[SequenceView](_.runningStep).some

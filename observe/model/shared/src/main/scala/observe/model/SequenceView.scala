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

  // def progress: Option[RunningStepProgress] =
  //   steps.zipWithIndex
  //     .find(!_._1.isFinished)
  //     .flatMap: x =>
  //       RunningStep.fromInt(x._1.id.some, x._2, steps.length)

  // // Returns where on the sequence the execution is at
  // def runningStepProgress: Option[RunningStepProgress] =
  //   status match
  //     case SequenceStatus.Running(_, _, _, _, _) => progress
  //     case SequenceStatus.Failed(_)              => progress
  //     case SequenceStatus.Aborted                => progress
  //     case _                                    => none

  def pausedStep: Option[PausedStep] =
    runningStep.filter(_.isObservePaused).map(_.id).map(PausedStep(_))

object SequenceView:

  val runningStep: Optional[SequenceView, ObserveStep] =
    Focus[SequenceView](_.runningStep).some

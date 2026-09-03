// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import io.circe.*
import io.circe.Decoder
import io.circe.Encoder
import io.circe.derivation.*
import io.circe.refined.given
import lucuma.core.enums.SequenceType
import lucuma.core.model.sequence.Step
import monocle.Focus
import monocle.Lens
import mouse.boolean.*
import observe.model.enums.ActionStatus

/**
 * This class concentrates all the execution state that is kept in the server, except for current
 * recorded ids in the ODB.
 */
case class ExecutionState(
  sequenceStatus:  SequenceStatus,
  observer:        Option[Observer],
  sequenceType:    SequenceType,
  loadedStep:      Option[ObserveStep],
  nsState:         Option[NsRunningState],
  systemOverrides: SystemOverrides,
  singleRuns:      Map[Step.Id, Map[Subsystem, ActionStatus]],
  breakpoints:     Set[Step.Id] = Set.empty,
  pausedStep:      Option[PausedStep] = None
) derives Eq,
      Encoder.AsObject,
      Decoder:
  def stepResources(stepId: Step.Id): Option[Map[Subsystem, ActionStatus]] =
    loadedStep.flatMap(l => (l.id === stepId).option(l.configStatus)).orElse(singleRuns.get(stepId))

  // If there's a running step or resource, the step is considered locked.
  lazy val isLocked: Boolean =
    sequenceStatus.isRunning ||
      loadedStep
        .flatMap(ld => stepResources(ld.id))
        .exists:
          _.exists(r => ActionStatus.LockedStatuses.contains_(r._2))

  lazy val isWaitingAcquisitionPrompt: Boolean =
    sequenceType === SequenceType.Acquisition && sequenceStatus.isWaitingUserPrompt

object ExecutionState:
  val sequenceStatus: Lens[ExecutionState, SequenceStatus]   = Focus[ExecutionState](_.sequenceStatus)
  val observer: Lens[ExecutionState, Option[Observer]]       = Focus[ExecutionState](_.observer)
  val sequenceType: Lens[ExecutionState, SequenceType]       = Focus[ExecutionState](_.sequenceType)
  val loadedStep: Lens[ExecutionState, Option[ObserveStep]]  = Focus[ExecutionState](_.loadedStep)
  val nsState: Lens[ExecutionState, Option[NsRunningState]]  = Focus[ExecutionState](_.nsState)
  val systemOverrides: Lens[ExecutionState, SystemOverrides] =
    Focus[ExecutionState](_.systemOverrides)
  val breakpoints: Lens[ExecutionState, Set[Step.Id]]        = Focus[ExecutionState](_.breakpoints)

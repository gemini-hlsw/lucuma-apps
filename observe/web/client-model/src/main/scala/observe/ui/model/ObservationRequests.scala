// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import lucuma.core.model.sequence.Step
import monocle.Focus
import monocle.Lens
import observe.model.SequenceStatus
import observe.model.Subsystem
import observe.ui.model.enums.OperationRequest

case class ObservationRequests(
  startSequence:      OperationRequest,
  startSequenceFrom:  OperationRequest,
  sequenceHold:       OperationRequest,
  cancelSequenceHold: OperationRequest,
  stopExposure:       OperationRequest,
  abortExposure:      OperationRequest,
  pauseExposure:      OperationRequest,
  resumeExposure:     OperationRequest,
  rewind:             OperationRequest,
  subsystemRun:       Map[Step.Id, Map[Subsystem, OperationRequest]],
  acquisitionPrompt:  OperationRequest
) derives Eq:
  val stepRequestInFlight: Boolean                =
    sequenceHold === OperationRequest.InFlight ||
    cancelSequenceHold === OperationRequest.InFlight ||
    pauseExposure === OperationRequest.InFlight ||
    resumeExposure === OperationRequest.InFlight ||
    stopExposure === OperationRequest.InFlight ||
    abortExposure === OperationRequest.InFlight ||
    startSequenceFrom === OperationRequest.InFlight ||
    rewind === OperationRequest.InFlight

    // Indicate if any resource is being executed
  def subsystemInFlight(stepId: Step.Id): Boolean =
    subsystemRun.get(stepId).exists(_.exists(_._2 === OperationRequest.InFlight))

  def withSequenceStatus(status: SequenceStatus, isPaused: Boolean): ObservationRequests =
    this.copy(
      startSequence = if (status.isRunning) OperationRequest.Idle else startSequence,
      startSequenceFrom = if (status.isRunning) OperationRequest.Idle else startSequenceFrom,
      sequenceHold =
        if (status.isSequenceHoldRequested || !status.isRunning) OperationRequest.Idle
        else sequenceHold,
      cancelSequenceHold =
        if (!status.isSequenceHoldRequested) OperationRequest.Idle else cancelSequenceHold,
      stopExposure = if (status.isRunning) stopExposure else OperationRequest.Idle,
      abortExposure = if (status.isAborted) OperationRequest.Idle else abortExposure,
      pauseExposure =
        if (isPaused || !status.isRunning) OperationRequest.Idle else pauseExposure,
      resumeExposure = if (status.isRunning) OperationRequest.Idle else resumeExposure,
      rewind = if (status.isRunning) rewind else OperationRequest.Idle
    )

object ObservationRequests:
  val Idle: ObservationRequests = ObservationRequests(
    startSequence = OperationRequest.Idle,
    startSequenceFrom = OperationRequest.Idle,
    sequenceHold = OperationRequest.Idle,
    cancelSequenceHold = OperationRequest.Idle,
    stopExposure = OperationRequest.Idle,
    abortExposure = OperationRequest.Idle,
    pauseExposure = OperationRequest.Idle,
    resumeExposure = OperationRequest.Idle,
    rewind = OperationRequest.Idle,
    subsystemRun = Map.empty,
    acquisitionPrompt = OperationRequest.Idle
  )

  val startSequence: Lens[ObservationRequests, OperationRequest]                              =
    Focus[ObservationRequests](_.startSequence)
  val startSequenceFrom: Lens[ObservationRequests, OperationRequest]                          =
    Focus[ObservationRequests](_.startSequenceFrom)
  val sequenceHold: Lens[ObservationRequests, OperationRequest]                               =
    Focus[ObservationRequests](_.sequenceHold)
  val cancelSequenceHold: Lens[ObservationRequests, OperationRequest]                         =
    Focus[ObservationRequests](_.cancelSequenceHold)
  val stopExposure: Lens[ObservationRequests, OperationRequest]                               =
    Focus[ObservationRequests](_.stopExposure)
  val abortExposure: Lens[ObservationRequests, OperationRequest]                              =
    Focus[ObservationRequests](_.abortExposure)
  val pauseExposure: Lens[ObservationRequests, OperationRequest]                              =
    Focus[ObservationRequests](_.pauseExposure)
  val resumeExposure: Lens[ObservationRequests, OperationRequest]                             =
    Focus[ObservationRequests](_.resumeExposure)
  val rewind: Lens[ObservationRequests, OperationRequest]                                     =
    Focus[ObservationRequests](_.rewind)
  val subsystemRun: Lens[ObservationRequests, Map[Step.Id, Map[Subsystem, OperationRequest]]] =
    Focus[ObservationRequests](_.subsystemRun)
  val acquisitionPrompt: Lens[ObservationRequests, OperationRequest]                          =
    Focus[ObservationRequests](_.acquisitionPrompt)

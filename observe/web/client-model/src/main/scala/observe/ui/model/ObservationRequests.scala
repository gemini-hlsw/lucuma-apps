// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import lucuma.core.enums.Instrument
import lucuma.core.model.sequence.Step
import monocle.Focus
import monocle.Lens
import observe.model.SequenceStatus
import observe.model.enums.Resource
import observe.ui.model.enums.OperationRequest

case class ObservationRequests(
  run:               OperationRequest,
  stop:              OperationRequest,
  abort:             OperationRequest,
  pause:             OperationRequest,
  cancelPause:       OperationRequest,
  resume:            OperationRequest,
  startFrom:         OperationRequest,
  subsystemRun:      Map[Step.Id, Map[Resource | Instrument, OperationRequest]],
  acquisitionPrompt: OperationRequest
) derives Eq:
  val stepRequestInFlight: Boolean                =
    pause === OperationRequest.InFlight ||
    cancelPause === OperationRequest.InFlight ||
    resume === OperationRequest.InFlight ||
    stop === OperationRequest.InFlight ||
    abort === OperationRequest.InFlight ||
    startFrom === OperationRequest.InFlight

    // Indicate if any resource is being executed
  def subsystemInFlight(stepId: Step.Id): Boolean =
    subsystemRun.get(stepId).exists(_.exists(_._2 === OperationRequest.InFlight))

  def withSequenceStatus(status: SequenceStatus): ObservationRequests =
    this.copy(
      run = if (status.isRunning) OperationRequest.Idle else run,
      stop = if (status.isRunning) stop else OperationRequest.Idle,
      abort = if (status.isAborted) OperationRequest.Idle else abort,
      pause = if (status.isUserStopRequested) OperationRequest.Idle else pause,
      cancelPause = if (status.isUserStopRequested) OperationRequest.Idle else cancelPause,
      resume = if (status.isRunning) OperationRequest.Idle else resume,
      startFrom = if (status.isRunning) OperationRequest.Idle else startFrom
    )

object ObservationRequests:
  val Idle: ObservationRequests = ObservationRequests(
    run = OperationRequest.Idle,
    stop = OperationRequest.Idle,
    abort = OperationRequest.Idle,
    pause = OperationRequest.Idle,
    cancelPause = OperationRequest.Idle,
    resume = OperationRequest.Idle,
    startFrom = OperationRequest.Idle,
    subsystemRun = Map.empty,
    acquisitionPrompt = OperationRequest.Idle
  )

  val run: Lens[ObservationRequests, OperationRequest]               =
    Focus[ObservationRequests](_.run)
  val stop: Lens[ObservationRequests, OperationRequest]              =
    Focus[ObservationRequests](_.stop)
  val abort: Lens[ObservationRequests, OperationRequest]             =
    Focus[ObservationRequests](_.abort)
  val pause: Lens[ObservationRequests, OperationRequest]             =
    Focus[ObservationRequests](_.pause)
  val cancelPause: Lens[ObservationRequests, OperationRequest]       =
    Focus[ObservationRequests](_.cancelPause)
  val resume: Lens[ObservationRequests, OperationRequest]            =
    Focus[ObservationRequests](_.resume)
  val startFrom: Lens[ObservationRequests, OperationRequest]         =
    Focus[ObservationRequests](_.startFrom)
  val subsystemRun
    : Lens[ObservationRequests, Map[Step.Id, Map[Resource | Instrument, OperationRequest]]] =
    Focus[ObservationRequests](_.subsystemRun)
  val acquisitionPrompt: Lens[ObservationRequests, OperationRequest] =
    Focus[ObservationRequests](_.acquisitionPrompt)

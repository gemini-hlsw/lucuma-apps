// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.services

import cats.effect.IO
import cats.syntax.eq.*
import crystal.react.View
import crystal.react.syntax.all.*
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Step
import lucuma.core.util.Enumerated
import monocle.Lens
import observe.model.ClientId
import observe.model.Observer
import observe.model.Subsystem
import observe.model.enums.RunOverride
import observe.model.given
import observe.ui.model.ObservationRequests
import observe.ui.model.enums.OperationRequest
import org.http4s.Query
import org.http4s.Uri

case class SequenceApiImpl(
  client:   ApiClient,
  observer: Observer,
  requests: View[Map[Observation.Id, ObservationRequests]]
) extends SequenceApi[IO]:

  private def setInFlight(
    obsId: Observation.Id,
    lens:  Lens[ObservationRequests, OperationRequest]
  ): IO[Unit] =
    requests
      .mod: r =>
        r + (obsId ->
          lens.replace(OperationRequest.InFlight)(r.getOrElse(obsId, ObservationRequests.Idle)))
      .to[IO]

  override def loadObservation(obsId: Observation.Id, instrument: Instrument): IO[Unit] =
    client.postNoData:
      Uri.Path.empty / "load" / instrument.tag / obsId.toString / client.clientId.value / observer.toString

  override def setBreakpoint(
    obsId:  Observation.Id,
    stepId: Step.Id,
    value:  Breakpoint
  ): IO[Unit] =
    client.postNoData:
      Uri.Path.empty / obsId.toString / stepId.toString / client.clientId.value / "breakpoint" / observer.toString / (value === Breakpoint.Enabled)

  override def setBreakpoints(
    obsId:   Observation.Id,
    stepIds: List[Step.Id],
    value:   Breakpoint
  ): IO[Unit] =
    client.post(
      Uri.Path.empty / obsId.toString / client.clientId.value / "breakpoints" / observer.toString / (value === Breakpoint.Enabled),
      stepIds
    )

  override def startSequence(
    obsId:       Observation.Id,
    runOverride: RunOverride = RunOverride.Default
  ): IO[Unit] =
    setInFlight(obsId, ObservationRequests.startSequence) >>
      client.postNoData(
        Uri.Path.empty / obsId.toString / client.clientId.value / "startSequence" / observer.toString,
        if (runOverride === RunOverride.Override) Query.fromPairs("overrideTargetCheck" -> "true")
        else Query.empty
      )

  override def startSequenceFrom(
    obsId:       Observation.Id,
    stepId:      Step.Id,
    runOverride: RunOverride = RunOverride.Default
  ): IO[Unit] =
    setInFlight(obsId, ObservationRequests.startSequenceFrom) >>
      client.postNoData(
        Uri.Path.empty / obsId.toString / stepId.toString / client.clientId.value / "startSequenceFrom" / observer.toString,
        if (runOverride === RunOverride.Override) Query.fromPairs("overrideTargetCheck" -> "true")
        else Query.empty
      )

  override def requestSequenceHold(obsId: Observation.Id): IO[Unit] =
    setInFlight(obsId, ObservationRequests.sequenceHold) >>
      client.postNoData(
        Uri.Path.empty / obsId.toString / client.clientId.value / "holdSequence" / observer.toString
      )

  override def cancelSequenceHoldRequest(obsId: Observation.Id): IO[Unit] =
    setInFlight(obsId, ObservationRequests.cancelSequenceHold) >>
      client.postNoData(
        Uri.Path.empty / obsId.toString / client.clientId.value / "cancelHoldSequence" / observer.toString
      )

  override def stopExposure(obsId: Observation.Id): IO[Unit] =
    setInFlight(obsId, ObservationRequests.stopExposure) >>
      client.postNoData(
        Uri.Path.empty / obsId.toString / client.clientId.value / "stopExposure" / observer.toString
      )

  override def stopExposureGracefully(obsId: Observation.Id): IO[Unit] =
    setInFlight(obsId, ObservationRequests.stopExposure) >>
      client.postNoData(
        Uri.Path.empty / obsId.toString / client.clientId.value / "stopExposureGracefully" / observer.toString
      )

  override def abortExposure(obsId: Observation.Id): IO[Unit] =
    setInFlight(obsId, ObservationRequests.abortExposure) >>
      client.postNoData(
        Uri.Path.empty / obsId.toString / client.clientId.value / "abortExposure" / observer.toString
      )

  override def pauseExposure(obsId: Observation.Id): IO[Unit] =
    setInFlight(obsId, ObservationRequests.pauseExposure) >>
      client.postNoData(
        Uri.Path.empty / obsId.toString / client.clientId.value / "pauseExposure" / observer.toString
      )

  override def rewindStep(obsId: Observation.Id): IO[Unit] =
    setInFlight(obsId, ObservationRequests.rewind) >>
      client.postNoData(
        Uri.Path.empty / obsId.toString / client.clientId.value / "rewindStep" / observer.toString
      )

  override def skipAcquisition(obsId: Observation.Id): IO[Unit] =
    setInFlight(obsId, ObservationRequests.skipAcquisition) >>
      client.postNoData(
        Uri.Path.empty / obsId.toString / client.clientId.value / "skipAcquisition" / observer.toString
      )

  override def resetAcquisition(obsId: Observation.Id): IO[Unit] =
    setInFlight(obsId, ObservationRequests.resetAcquisition) >>
      client.postNoData(
        Uri.Path.empty / obsId.toString / client.clientId.value / "resetAcquisition" / observer.toString
      )

  override def cancelRewindRequest(obsId: Observation.Id): IO[Unit] =
    setInFlight(obsId, ObservationRequests.cancelRewind) >>
      client.postNoData(
        Uri.Path.empty / obsId.toString / client.clientId.value / "cancelRewindStep" / observer.toString
      )

  override def pauseExposureGracefully(obsId: Observation.Id): IO[Unit] =
    setInFlight(obsId, ObservationRequests.pauseExposure) >>
      client.postNoData(
        Uri.Path.empty / obsId.toString / client.clientId.value / "pauseExposureGracefully" / observer.toString
      )

  override def resumeExposure(obsId: Observation.Id): IO[Unit] =
    setInFlight(obsId, ObservationRequests.resumeExposure) >>
      client.postNoData(
        Uri.Path.empty / obsId.toString / client.clientId.value / "resumeExposure" / observer.toString
      )

  override def execute(
    obsId:     Observation.Id,
    stepId:    Step.Id,
    subsystem: Subsystem
  ): IO[Unit] =
    setInFlight(
      obsId,
      ObservationRequests.subsystemRun
        .at(stepId)
        .withDefault(Map.empty)
        .at(subsystem)
        .withDefault(OperationRequest.Idle)
    ) >>
      client.postNoData:
        Uri.Path.empty / obsId.toString / stepId.toString / client.clientId.value / "execute" /
          Enumerated[Subsystem].tag(subsystem) / observer.toString

  override def proceedAfterPrompt(obsId: Observation.Id, sequenceType: SequenceType): IO[Unit] =
    setInFlight(obsId, ObservationRequests.acquisitionPrompt) >>
      client.postNoData:
        Uri.Path.empty / obsId.toString / client.clientId.value / "proceedAfterPrompt" / observer.toString / sequenceType.tag

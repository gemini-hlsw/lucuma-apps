// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.model.arb

import lucuma.core.model.sequence.Step
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbUid.given
import observe.model.Subsystem
import observe.model.given
import observe.ui.model.ObservationRequests
import observe.ui.model.enums.OperationRequest
import observe.ui.model.enums.arb.ArbOperationRequest.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbObservationRequests:
  given Arbitrary[ObservationRequests] = Arbitrary:
    for
      startSequence      <- arbitrary[OperationRequest]
      startSequenceFrom  <- arbitrary[OperationRequest]
      sequenceHold       <- arbitrary[OperationRequest]
      cancelSequenceHold <- arbitrary[OperationRequest]
      stopExposure       <- arbitrary[OperationRequest]
      abortExposure      <- arbitrary[OperationRequest]
      pauseExposure      <- arbitrary[OperationRequest]
      resumeExposure     <- arbitrary[OperationRequest]
      rewind             <- arbitrary[OperationRequest]
      cancelRewind       <- arbitrary[OperationRequest]
      subsystemRun       <- arbitrary[Map[Step.Id, Map[Subsystem, OperationRequest]]]
      acquisitionPrompt  <- arbitrary[OperationRequest]
    yield ObservationRequests(
      startSequence,
      startSequenceFrom,
      sequenceHold,
      cancelSequenceHold,
      stopExposure,
      abortExposure,
      pauseExposure,
      resumeExposure,
      rewind,
      cancelRewind,
      subsystemRun,
      acquisitionPrompt
    )

  given Cogen[ObservationRequests] = Cogen[
    (OperationRequest,
     OperationRequest,
     OperationRequest,
     OperationRequest,
     OperationRequest,
     OperationRequest,
     OperationRequest,
     OperationRequest,
     OperationRequest,
     OperationRequest,
     List[(Step.Id, List[(Subsystem, OperationRequest)])],
     OperationRequest
    )
  ].contramap(x =>
    (x.startSequence,
     x.startSequenceFrom,
     x.sequenceHold,
     x.cancelSequenceHold,
     x.stopExposure,
     x.abortExposure,
     x.pauseExposure,
     x.resumeExposure,
     x.rewind,
     x.cancelRewind,
     x.subsystemRun.view.mapValues(_.toList).toList,
     x.acquisitionPrompt
    )
  )

object ArbObservationRequests extends ArbObservationRequests

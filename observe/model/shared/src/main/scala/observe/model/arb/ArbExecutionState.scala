// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model.arb

import eu.timepit.refined.scalacheck.string.given
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.math.arb.ArbRefined.given
import lucuma.core.model.sequence.Step
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbNewType.given
import lucuma.core.util.arb.ArbUid.given
import observe.model.ExecutionState
import observe.model.NsRunningState
import observe.model.ObserveStep
import observe.model.Observer
import observe.model.SequenceStatus
import observe.model.SystemOverrides
import observe.model.arb.ArbNsRunningState.given
import observe.model.arb.ObserveModelArbitraries.given
import observe.model.enums.ActionStatus
import observe.model.enums.Resource
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

import ArbObserveStep.given

trait ArbExecutionState:
  given Arbitrary[ExecutionState] = Arbitrary:
    for
      sequenceStatus  <- arbitrary[SequenceStatus]
      observer        <- arbitrary[Option[Observer]]
      sequenceType    <- arbitrary[SequenceType]
      step            <- arbitrary[Option[ObserveStep]]
      nsState         <- arbitrary[Option[NsRunningState]]
      stepResources   <- arbitrary[Map[Resource | Instrument, ActionStatus]]
      systemOverrides <- arbitrary[SystemOverrides]
      breakpoints     <- arbitrary[Set[Step.Id]]
    yield ExecutionState(
      sequenceStatus,
      observer,
      sequenceType,
      step,
      nsState,
      stepResources,
      systemOverrides,
      breakpoints
    )

  given Cogen[ExecutionState] =
    Cogen[
      (
        SequenceStatus,
        Option[Observer],
        SequenceType,
        Option[ObserveStep],
        Option[NsRunningState],
        List[(Resource | Instrument, ActionStatus)],
        SystemOverrides,
        List[Step.Id]
      )
    ].contramap: x =>
      (x.sequenceStatus,
       x.observer,
       x.sequenceType,
       x.runningStep,
       x.nsState,
       x.stepResources.toList,
       x.systemOverrides,
       x.breakpoints.toList
      )

object ArbExecutionState extends ArbExecutionState

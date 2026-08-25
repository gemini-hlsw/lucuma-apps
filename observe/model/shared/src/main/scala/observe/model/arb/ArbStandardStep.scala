// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model.arb

import lucuma.core.model.sequence.Step
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbUid.given
import observe.model.*
import observe.model.enums.*
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

import ArbSubsystem.given

trait ArbStandardStep {

  given Arbitrary[ObserveStep.Standard] = Arbitrary[ObserveStep.Standard] {
    for {
      id <- arbitrary[Step.Id]
      cs <- arbitrary[Map[Subsystem, ActionStatus]]
      os <- arbitrary[ActionStatus]
    } yield ObserveStep.Standard(
      id = id,
      configStatus = cs,
      observeStatus = os
    )
  }

  given Cogen[ObserveStep.Standard] =
    Cogen[
      (
        Step.Id,
        List[(Subsystem, ActionStatus)],
        ActionStatus
      )
    ].contramap(s =>
      (
        s.id,
       s.configStatus.toList,
       s.observeStatus
      )
    )

}

object ArbStandardStep extends ArbStandardStep

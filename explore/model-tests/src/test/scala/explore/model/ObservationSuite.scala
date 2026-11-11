// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.kernel.laws.discipline.EqTests
import cats.syntax.all.*
import explore.model.arb.ArbObservation
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.util.arb.ArbEnumerated.given
import munit.DisciplineSuite
import org.scalacheck.Prop.*

class ObservationSuite extends DisciplineSuite:
  import ArbObservation.given

  checkAll("Eq[Observation]", EqTests[Observation].eqv)

  property("a suspended observation is ongoing, and so executed"):
    forAll: (obs: Observation, state: ObservationWorkflowState) =>
      val o = Observation.workflowState.replace(state)(obs)
      assertEquals(
        o.isOngoing,
        state === ObservationWorkflowState.Ongoing || state === ObservationWorkflowState.Suspended
      )
      assertEquals(o.isExecuted, o.isOngoing || state === ObservationWorkflowState.Completed)

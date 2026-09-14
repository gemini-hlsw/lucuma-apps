// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.kernel.laws.discipline.EqTests
import cats.syntax.all.*
import explore.model.arb.ArbObservation
import lucuma.core.model.ObservationReference
import lucuma.core.model.arb.ArbObservationReference.given
import munit.DisciplineSuite
import org.scalacheck.Prop.forAll

class ObservationSuite extends DisciplineSuite:
  import ArbObservation.given

  checkAll("Eq[Observation]", EqTests[Observation].eqv)

  property("referenceWithId shows the reference with the id in parentheses"):
    forAll: (obs: Observation, ref: ObservationReference) =>
      assertEquals(obs.copy(reference = ref.some).referenceWithId, s"${ref.label} (${obs.id.show})")

  property("referenceWithId falls back to the bare id when there is no reference"):
    forAll: (obs: Observation) =>
      assertEquals(obs.copy(reference = none).referenceWithId, obs.id.show)

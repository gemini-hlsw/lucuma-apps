// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.kernel.laws.discipline.*
import explore.model.arb.all.given
import munit.DisciplineSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*

class AsterismSuite extends DisciplineSuite:
  checkAll("Eq[ObservationTargets]", EqTests[ObservationTargets].eqv)

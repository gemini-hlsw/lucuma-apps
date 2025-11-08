// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.kernel.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import explore.model.arb.all.given
import lucuma.core.enums.TargetDisposition
import lucuma.core.model.Target
import lucuma.core.model.arb.ArbTarget.given
import lucuma.core.util.arb.ArbGid.given
import lucuma.schemas.model.TargetWithId
import lucuma.schemas.model.arb.ArbTargetWithId.given
import monocle.law.discipline.*
import munit.DisciplineSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Gen
import org.scalacheck.Prop.*
import org.scalacheck.Test

class AsterismSuite extends DisciplineSuite:
  override val scalaCheckTestParameters = Test.Parameters.default.withMaxSize(10)

  checkAll("Eq[ObservationTargets]", EqTests[ObservationTargets].eqv)
  checkAll("ObservationTargets.isoTargets", IsoTests(ObservationTargets.isoTargets))
  checkAll("ObservationTargets.fromTargetsList", IsoTests(ObservationTargets.fromTargetsList))
  checkAll("ObservationTargets.targetsEach", TraversalTests(ObservationTargets.targetsEach))
  checkAll("ObservationTargets.siderealTargetsEach", TraversalTests(ObservationTargets.siderealTargetsEach))

  test("targetOptional") {
    forAll { (id: Target.Id) =>
      given Arbitrary[Option[ObservationTargets]] = gen.optObservationTargets(id)
      checkAll("ObservationTargets.targetOptional", OptionalTests(ObservationTargets.targetOptional(id)))
    }
  }

  test("fromTargetsListOn") {
    forAll { (id: Target.Id) =>
      given Arbitrary[Option[ObservationTargets]] = gen.optObservationTargets(id)
      checkAll("ObservationTargets.fromTargetsListOn", IsoTests(ObservationTargets.fromTargetsListOn(Some(id))))
    }
  }

  object gen:
    // Sometimes the observationTargets includes target id
    def optObservationTargets(id: Target.Id): Arbitrary[Option[ObservationTargets]] =
      Arbitrary(
        Gen.option[ObservationTargets](observationTargets(id).arbitrary)
      )

    def observationTargets(id: Target.Id): Arbitrary[ObservationTargets] =
      Arbitrary(
        Gen.oneOf(
          arbObservationTargets.arbitrary,
          Gen.oneOf(
            arbObservationTargets.arbitrary,
            for
              ast    <- arbObservationTargets.arbitrary
              target <-
                arbitrary[Target].map(t => TargetWithId(id, t, TargetDisposition.Science, None))
            yield ast.add(target)
          )
        )
      )

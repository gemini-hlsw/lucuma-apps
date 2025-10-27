// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import munit.DisciplineSuite
import org.scalacheck.Prop.*

import arb.ArbAngleTypes

class AngleTypesSuite extends DisciplineSuite with ArbAngleTypes {

  property("AzimuthAngle stays in range [-180º, 360º)") {
    forAll { (v: AzimuthAngle) =>
      assert(AzimuthAngle.MinValue <= v.µas && v.µas < AzimuthAngle.MaxValue)
    }
  }

  property("RotatorAngle stays in range [-270º, 270º)") {
    forAll { (v: RotatorAngle) =>
      assert(RotatorAngle.MinValue <= v.µas && v.µas < RotatorAngle.MaxValue)
    }
  }

}

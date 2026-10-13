// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import lucuma.core.math.Angle
import munit.DisciplineSuite
import org.scalacheck.Prop.*

import arb.ArbAngleTypes
import AzimuthAngle.toDoubleDegrees
import RotatorAngle.toDoubleDegrees

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

  private val Turn = BigInt(LinearAngle.angle360µas)

  property("RotatorAngle values circle around 360º") {
    forAll { (value: Long) =>
      val v = RotatorAngle.fromMicroarcseconds(value)
      assertEquals((BigInt(v.µas) - BigInt(value)).mod(Turn), BigInt(0))
    }
  }

  property("AzimuthAngle values circle around 360º") {
    forAll { (value: Long) =>
      val v = AzimuthAngle.fromMicroarcseconds(value)
      assertEquals((BigInt(v.µas) - BigInt(value)).mod(Turn), BigInt(0))
    }
  }

  property("In range values are kept as they are") {
    forAll { (v: RotatorAngle) =>
      assertEquals(RotatorAngle.fromMicroarcseconds(v.µas).µas, v.µas)
    }
  }

  test("RotatorAngle wraps out of range values by full turns") {
    def check(degrees: Double, expected: Double): Unit =
      assertEqualsDouble(RotatorAngle.fromDoubleDegrees(degrees).toDoubleDegrees, expected, 1e-6)

    check(100.0, 100.0)
    check(269.0, 269.0)
    check(-260.0, -260.0)
    check(270.0, -90.0)
    check(280.0, -80.0)
    check(315.0, -45.0)
    check(350.0, -10.0)
    check(500.0, 140.0)
    check(630.0, -90.0)
    check(-271.0, 89.0)
    check(-400.0, -40.0)
  }

  test("AzimuthAngle wraps out of range values by full turns") {
    def check(degrees: Double, expected: Double): Unit =
      assertEqualsDouble(AzimuthAngle.fromDoubleDegrees(degrees).toDoubleDegrees, expected, 1e-6)

    check(-180.0, -180.0)
    check(359.0, 359.0)
    check(360.0, 0.0)
    check(400.0, 40.0)
    check(-181.0, 179.0)
    check(-200.0, 160.0)
    check(-540.0, -180.0)
  }

  test("RotatorAngle.fromAngle keeps an IPA in [0º, 360º) equivalent to the original") {
    List(0.0, 10.0, 100.0, 200.0, 269.0, 280.0, 315.0, 350.0).foreach { ipa =>
      val r = RotatorAngle.fromAngle(Angle.fromDoubleDegrees(ipa)).toDoubleDegrees
      val d = ((r - ipa) % 360.0 + 360.0) % 360.0
      assert(d < 1e-6 || 360.0 - d < 1e-6, s"$ipa gave $r")
    }
  }

}

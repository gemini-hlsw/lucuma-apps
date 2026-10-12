// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import lucuma.core.math.Angle
import lucuma.core.math.Offset
import munit.FunSuite
import navigate.model.FocalPlaneOffset.toOffset

class FocalPlaneOffsetSuite extends FunSuite {

  private def arcsec(a: Double): Angle = Angle.fromBigDecimalArcseconds(BigDecimal(a))

  private def signedArcsec(a: Angle): Double = Angle.signedDecimalArcseconds.get(a).toDouble

  for {
    p      <- List(10.0, -10.0, 0.0)
    q      <- List(10.0, -10.0, 0.0)
    iaaDeg <- List(0.0, 33.0, 90.0, 180.0, 270.0, -45.0)
  } test(s"Offset (p=$p, q=$q) with iaa=$iaaDeg survives the round trip to the focal plane") {
    val iaa      = Angle.fromDoubleDegrees(iaaDeg)
    val original = Offset(Offset.P(arcsec(p)), Offset.Q(arcsec(q)))
    val back     = FocalPlaneOffset.fromOffset(original, iaa).toOffset(iaa)
    assertEqualsDouble(signedArcsec(back.p.toAngle), p, 1e-4)
    assertEqualsDouble(signedArcsec(back.q.toAngle), q, 1e-4)
  }

  test("A negative offset keeps its magnitude in the focal plane") {
    val fp = FocalPlaneOffset.fromOffset(Offset(Offset.P(arcsec(-10)), Offset.Q(arcsec(0))),
                                         Angle.fromDoubleDegrees(33.0)
    )
    assertEqualsDouble(signedArcsec(fp.deltaX.value), 10.0 * Math.cos(Math.toRadians(33.0)), 1e-4)
    assertEqualsDouble(signedArcsec(fp.deltaY.value), -10.0 * Math.sin(Math.toRadians(33.0)), 1e-4)
  }
}

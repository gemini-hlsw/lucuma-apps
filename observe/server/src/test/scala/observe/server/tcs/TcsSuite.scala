// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.tcs

import cats.kernel.laws.discipline.*
import coulomb.*
import coulomb.syntax.*
import coulomb.units.accepted.ArcSecond
import coulomb.units.accepted.Millimeter
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import observe.server.tcs.FocalPlaneScale.*
import observe.server.tcs.TcsController.OffsetP
import observe.server.tcs.TcsController.OffsetQ

/**
 * Tests Tcs typeclasses
 */
class TcsSuite extends munit.DisciplineSuite with TcsArbitraries:
  checkAll("Eq[CRFollow]", EqTests[CRFollow].eqv)

  assertEquals(0.01.withUnit[Millimeter] :* FOCAL_PLANE_SCALE, 0.0161144.withUnit[ArcSecond])
  assertEquals(0.032.withUnit[ArcSecond] :\ FOCAL_PLANE_SCALE,
               0.019858015191381622.withUnit[Millimeter]
  )

  test("Offset to InstrumentOffset is expressed in arcseconds") {
    val offset = Offset(Offset.P(Angle.fromDoubleArcseconds(10.0)),
                        Offset.Q(Angle.fromDoubleArcseconds(-5.0))
    ).toInstrumentOffset

    assertEqualsDouble(offset.p.value.value, 10.0, 1e-9)
    assertEqualsDouble(offset.q.value.value, -5.0, 1e-9)
  }

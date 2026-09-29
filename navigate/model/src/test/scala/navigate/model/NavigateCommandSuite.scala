// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import cats.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import navigate.model.NavigateCommand.*

class NavigateCommandSuite extends munit.FunSuite {

  private val offset: Offset = Offset(
    Offset.P(Angle.fromMicroarcseconds(1500000L)),
    Offset.Q(Angle.fromMicroarcseconds(-2250000L))
  )

  test("Offsets are shown as signed arcseconds") {
    assertEquals(showOffset(offset), "Offset(p = 1.5\", q = -2.25\")")
    assertEquals(showOffset(Offset.Zero), "Offset(p = 0\", q = 0\")")
  }

  test("Angles are shown as degrees in [0º, 360º)") {
    assertEquals(degrees(Angle.fromDoubleDegrees(-45.5)), "314.5º")
    assertEquals(degrees(Angle.fromDoubleDegrees(270)), "270º")
    assertEquals(degrees(Angle.fromDoubleDegrees(10.25)), "10.25º")
    assertEquals(degrees(Angle.Angle0), "0º")
  }

  test("AcquisitionAdjust shows ipa and iaa as degrees in [0º, 360º)") {
    assertEquals(
      (AcquisitionAdjust(
        offset,
        Angle.fromDoubleDegrees(-45.5).some,
        none
      ): NavigateCommand).show,
      "AcquisitionAdjust(offset = Offset(p = 1.5\", q = -2.25\"), ipa = Some(314.5º), iaa = None)"
    )
  }

  test("Offset commands show their offset as signed arcseconds") {
    assertEquals(
      (TelescopeOffset(offset, true): NavigateCommand).show,
      "TelescopeOffset(offset = Offset(p = 1.5\", q = -2.25\"), guiding = true)"
    )
    assertEquals(
      (ConfigureStep(offset.some, none, none, none, false): NavigateCommand).show,
      "ConfigureStep(offset = Some(Offset(p = 1.5\", q = -2.25\")), wavelength = None, " +
        "lightPath = None, defocus = None, guiding = false)"
    )
  }
}

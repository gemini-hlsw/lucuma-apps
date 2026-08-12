// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.modes

import lucuma.core.math.Wavelength
import munit.FunSuite

class GnirsCrossDispersedSuite extends FunSuite {

  private def toHBand(pm: Int): Int =
    GnirsCrossDispersed
      .toHBandWavelength(Wavelength.unsafeFromIntPicometers(pm))
      .toPicometers
      .value
      .value

  test("order 3 (K) converts to the H band") {
    // The example from the request: 2.20µm · 3/4 = 1.65µm.
    assertEquals(toHBand(2_200_000), 1_650_000)
    assertEquals(toHBand(2_500_000), 1_875_000) // the top of the XD range
  }

  test("order 4 (H) is unchanged") {
    assertEquals(toHBand(1_650_000), 1_650_000)
    assertEquals(toHBand(1_500_000), 1_500_000)
  }

  test("orders 5 through 8 convert to the H band") {
    assertEquals(toHBand(1_250_000), 1_562_500) // J, order 5
    assertEquals(toHBand(1_100_000), 1_650_000) // X, order 6
    assertEquals(toHBand(951_000), 1_664_250)   // order 7
    assertEquals(toHBand(850_000), 1_700_000)   // order 8, the bottom of the XD range
  }

  test("a wavelength outside every order is left alone") {
    assertEquals(toHBand(700_000), 700_000)     // below order 8
    assertEquals(toHBand(6_500_000), 6_500_000) // above order 1
  }

}

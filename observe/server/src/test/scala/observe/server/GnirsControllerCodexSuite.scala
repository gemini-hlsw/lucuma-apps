// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsGrating
import lucuma.core.enums.GnirsPrism
import observe.server.gnirs.GnirsControllerEpics

class GnirsControllerCodexSuite extends munit.FunSuite:

  test("buildGratingValue: cross-disperser special cases for LongBlue") {
    assertEquals(
      GnirsControllerEpics.buildGratingValue(GnirsPrism.Sxd,
                                             GnirsGrating.D10,
                                             GnirsCamera.LongBlue,
                                             hrIfu = false
      ),
      "10/mmLBSX"
    )
    assertEquals(
      GnirsControllerEpics.buildGratingValue(GnirsPrism.Lxd,
                                             GnirsGrating.D10,
                                             GnirsCamera.LongBlue,
                                             hrIfu = false
      ),
      "10/mmLBLX"
    )
    assertEquals(
      GnirsControllerEpics.buildGratingValue(GnirsPrism.Lxd,
                                             GnirsGrating.D111,
                                             GnirsCamera.LongBlue,
                                             hrIfu = false
      ),
      "111/mmLBLX"
    )
  }

  test("HR-IFU + Mirror reproduces the OCS GRATING/PRISM keywords") {
    // OCS readback for HR-IFU + LongBlue + D32 (component IDs are appended by the
    // instrument): GRATING='32/mmLBHR_G5533', PRISM='LB32+MIR_G5537'.
    assertEquals(
      GnirsControllerEpics.buildGratingValue(GnirsPrism.Mirror,
                                             GnirsGrating.D32,
                                             GnirsCamera.LongBlue,
                                             hrIfu = true
      ),
      "32/mmLBHR"
    )
    assertEquals(
      GnirsControllerEpics.buildPrismValue(GnirsPrism.Mirror,
                                           GnirsGrating.D32,
                                           GnirsCamera.LongBlue,
                                           hrIfu = true
      ),
      "LB32+MIR"
    )
  }

  test("Mirror without HR-IFU keeps the plain GRATING/PRISM keywords") {
    // The pre-existing (non HR-IFU) values, unchanged: GRATING='32/mmLB_G5533', PRISM='MIR_G5537'.
    assertEquals(
      GnirsControllerEpics.buildGratingValue(GnirsPrism.Mirror,
                                             GnirsGrating.D32,
                                             GnirsCamera.LongBlue,
                                             hrIfu = false
      ),
      "32/mmLB"
    )
    assertEquals(
      GnirsControllerEpics.buildPrismValue(GnirsPrism.Mirror,
                                           GnirsGrating.D32,
                                           GnirsCamera.LongBlue,
                                           hrIfu = false
      ),
      "MIR"
    )
  }

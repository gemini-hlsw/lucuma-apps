// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.model.enums

import cats.syntax.all.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Instrument
import lucuma.schemas.model.BasicConfiguration
import munit.FunSuite

class ObsClassSuite extends FunSuite:
  test("daytime calibration observing mode is daytime"):
    assertEquals(
      ObsClass.fromObservation(
        CalibrationRole.DaytimeCalibration.some,
        BasicConfiguration.DaytimeCalibration(Instrument.GmosSouth).some
      ),
      ObsClass.Daytime
    )

  test("daytime pinhole is daytime"):
    assertEquals(
      ObsClass.fromObservation(CalibrationRole.DaytimePinhole.some, None),
      ObsClass.Daytime
    )

  test("twilight is nighttime"):
    assertEquals(ObsClass.fromObservation(CalibrationRole.Twilight.some, None), ObsClass.Nighttime)

  test("science is nighttime"):
    assertEquals(ObsClass.fromObservation(None, None), ObsClass.Nighttime)

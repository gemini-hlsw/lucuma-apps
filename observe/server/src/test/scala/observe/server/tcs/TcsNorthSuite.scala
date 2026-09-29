// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.tcs

import cats.syntax.all.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.LightSinkName
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.schemas.model.navigate.LightSource
import observe.server.tcs.TcsController.LightPath

class TcsNorthSuite extends munit.FunSuite {

  test("Step guiding comes from the step's telescope configuration") {
    List(StepGuideState.Enabled, StepGuideState.Disabled).foreach { guiding =>
      assertEquals(
        TcsNorth
          .config(
            Instrument.GmosNorth,
            TelescopeConfig(Offset.Zero, guiding),
            LightPath(LightSource.Sky, LightSinkName.Gmos),
            none,
            none
          )
          .guiding,
        guiding
      )
    }
  }

}

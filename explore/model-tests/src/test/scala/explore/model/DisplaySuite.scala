// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import explore.model.display.given
import lucuma.core.enums.GmosRoi
import lucuma.core.util.Display
import munit.FunSuite

class DisplaySuite extends FunSuite:

  test("The full frame ROI reads like the other ROIs"):
    val roi = Display[GmosRoi]
    assertEquals(roi.shortName(GmosRoi.FullFrame), "Full Frame")
    assertEquals(roi.shortName(GmosRoi.CentralStamp), GmosRoi.CentralStamp.longName)

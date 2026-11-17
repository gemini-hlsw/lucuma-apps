// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.visualization

import cats.syntax.all.*
import lucuma.ags.GuideStarId
import lucuma.core.model.Target

class GuideStarLabelSuite extends munit.FunSuite:

  test("a catalog star is labelled by its catalog name"):
    assertEquals(GuideStarId.gaia(123L).displayLabel, "Gaia DR3 123")

  test("an observation target is labelled as the science target"):
    val targetId: Target.Id = Target.Id.fromLong(1L).get
    assertEquals(GuideStarId.ObservationTarget(targetId.some).displayLabel, "Science Target")
    assertEquals(GuideStarId.ObservationTarget(none).displayLabel, "Science Target")

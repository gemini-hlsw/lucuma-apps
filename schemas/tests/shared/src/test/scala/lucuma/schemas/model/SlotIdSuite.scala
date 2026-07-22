// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model

import lucuma.core.util.Enumerated
import munit.FunSuite

class SlotIdSuite extends FunSuite:

  test("the base position is a slot"):
    assert(Enumerated[SlotId].all.contains(SlotId.Base))

  test("position labels are derived per slot, not concatenated"):
    assertEquals(SlotId.GhostIfu1.positionLabel, "IFU1 Sky")
    assertEquals(SlotId.GhostIfu2.positionLabel, "IFU2 Sky")
    assertEquals(SlotId.Base.positionLabel, "Base Position")

  test("only the base slot is the base"):
    assertEquals(Enumerated[SlotId].all.filter(_.isBase), List(SlotId.Base))

// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.modes

import cats.syntax.all.*
import lucuma.core.enums.AltairMode
import munit.FunSuite

class AdaptiveOpticsFilterSuite extends FunSuite:
  import AdaptiveOpticsFilter.*

  private val altairs: List[Option[AltairMode]] =
    none :: List(AltairMode.Ngs, AltairMode.Lgs, AltairMode.LgsP1).map(_.some)

  test("only the filters admitting NGS or LGS rows await the guide star"):
    assertEquals(List(All, Ngs, Lgs).map(_.awaitsGuideStar), List(true, true, true))
    assertEquals(List(LgsP1, NoAo).map(_.awaitsGuideStar), List(false, false))

  test("forMode admits exactly the mode it was derived from"):
    altairs.foreach: altair =>
      assertEquals(altairs.filter(forMode(altair).admits), List(altair))

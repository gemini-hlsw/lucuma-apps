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

  private def admitted(filter: AdaptiveOpticsFilter): List[Option[AltairMode]] =
    altairs.filter(filter.admits)

  test("All admits every row"):
    assertEquals(admitted(All), altairs)

  test("NoAo admits only the plain rows"):
    assertEquals(admitted(NoAo), List(none))

  test("each Altair filter admits only its own mode"):
    assertEquals(admitted(Ngs), List(AltairMode.Ngs.some))
    assertEquals(admitted(Lgs), List(AltairMode.Lgs.some))
    assertEquals(admitted(LgsP1), List(AltairMode.LgsP1.some))

  test("only the filters admitting NGS or LGS rows await the guide star"):
    assertEquals(List(All, Ngs, Lgs).map(_.awaitsGuideStar), List(true, true, true))
    assertEquals(List(LgsP1, NoAo).map(_.awaitsGuideStar), List(false, false))

  test("forMode round trips through admits"):
    altairs.foreach: altair =>
      assertEquals(forMode(altair).admits(altair), true)
      assertEquals(altairs.filter(forMode(altair).admits), List(altair))

  test("the default filter is NoAo"):
    assertEquals(Default, NoAo)

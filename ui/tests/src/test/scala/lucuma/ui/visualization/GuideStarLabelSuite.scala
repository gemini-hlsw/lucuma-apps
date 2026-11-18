// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.visualization

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.ags.GuideStarId
import lucuma.core.model.Target

class GuideStarLabelSuite extends munit.FunSuite:

  private val T1: Target.Id = Target.Id.fromLong(1L).get
  private val T2: Target.Id = Target.Id.fromLong(2L).get

  private val One: Map[Target.Id, NonEmptyString] =
    Map(T1 -> NonEmptyString.unsafeFrom("Io"))
  private val Two: Map[Target.Id, NonEmptyString] =
    One + (T2 -> NonEmptyString.unsafeFrom("Titan"))

  test("a catalog star is labelled by its catalog name"):
    assertEquals(GuideStarId.GaiaDr3(123L).displayLabel(Two), "Gaia DR3 123")

  test("a science target is labelled with its name"):
    assertEquals(GuideStarId.ObservationTarget(T1.some).displayLabel(Two), "Science Target: Io")
    assertEquals(GuideStarId.ObservationTarget(T2.some).displayLabel(Two), "Science Target: Titan")

  test("a target that is not a science target gets the bare label"):
    assertEquals(GuideStarId.ObservationTarget(T2.some).displayLabel(One), "Science Target")

  test("the id-less selection is named only with a single science target"):
    assertEquals(GuideStarId.ObservationTarget(none).displayLabel(One), "Science Target: Io")
    assertEquals(GuideStarId.ObservationTarget(none).displayLabel(Two), "Science Target")
    assertEquals(GuideStarId.ObservationTarget(none).displayLabel(Map.empty), "Science Target")

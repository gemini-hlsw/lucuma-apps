// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import explore.model.GuideStarSelection.*
import lucuma.ags.AgsAnalysis
import lucuma.ags.AgsGuideQuality
import lucuma.ags.GuideStarCandidate
import lucuma.ags.GuideStarId
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.GuideSpeed
import lucuma.core.geom.Area
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import munit.FunSuite

import scala.collection.immutable.SortedMap

class GuideStarSelectionSuite extends FunSuite:
  private def targetId(n: Long): Target.Id = Target.Id.fromLong(n).get

  private def usable(id: GuideStarId): AgsAnalysis.Usable =
    AgsAnalysis.Usable(
      GuideProbe.AltairAOWFS,
      GuideStarCandidate(id, SiderealTracking.const(Coordinates.Zero), SortedMap.empty),
      GuideSpeed.Fast,
      AgsGuideQuality.DeliversRequestedIq,
      Angle.Angle0,
      Area.MinArea
    )

  private val Gaia1: GuideStarId   = GuideStarId.gaia(1L)
  private val Gaia2: GuideStarId   = GuideStarId.gaia(2L)
  private val Target1: GuideStarId = GuideStarId.ObservationTarget(targetId(1L).some)
  private val Target2: GuideStarId = GuideStarId.ObservationTarget(targetId(2L).some)

  // Ranking order: best first.
  private val Results: List[AgsAnalysis.Usable] =
    List(Gaia1, Target2, Target1, Gaia2).map(usable)

  private val Candidates: List[GuideStarCandidate] = Results.map(_.target)

  // An observation with a single science target.
  private val SingleTargetResults: List[AgsAnalysis.Usable]    = List(Gaia1, Target1, Gaia2).map(usable)
  private val SingleTargetCandidates: List[GuideStarCandidate] = SingleTargetResults.map(_.target)

  test("no selection takes the best candidate"):
    assertEquals(Results.select(none, Candidates), AgsSelection((0, Results.head).some))

  test("a catalog selection picks the same star"):
    assertEquals(Results.select(Gaia2.some, Candidates), AgsOverride(Gaia2, 3, Results(3)))

  test("an observation target selection with an id picks that target"):
    assertEquals(Results.pick(Target1, Candidates), AgsOverride(Target1, 2, Results(2)))

  test("the id-less observation target selection picks the only observation target"):
    val selection: GuideStarId = GuideStarId.ObservationTarget(none)
    val picked: GuideStarSelection = SingleTargetResults.pick(selection, SingleTargetCandidates)
    assertEquals(picked, AgsOverride(selection, 1, SingleTargetResults(1)))
    assertEquals(picked.selectedId, selection.some)
    assertEquals(picked.resetKeepingSelection, RemoteGSSelection(selection))

  test("a selection that is not found is kept as remote"):
    val missing: GuideStarId = GuideStarId.ObservationTarget(targetId(9L).some)
    assertEquals(Results.pick(missing, Candidates), RemoteGSSelection(missing))
    assertEquals(Results.pick(GuideStarId.gaia(9L), Candidates), RemoteGSSelection(GuideStarId.gaia(9L)))
    assertEquals(Nil.pick(GuideStarId.ObservationTarget(none), Nil),
                 RemoteGSSelection(GuideStarId.ObservationTarget(none))
    )

  test("picking by index keeps the candidate's own id"):
    val picked: GuideStarSelection = Results.pick(2)
    assertEquals(picked, AgsOverride(Target1, 2, Results(2)))
    assertEquals(picked.selectedId, Target1.some)

  test("the id-less selection is ambiguous with several observation targets, as in the ODB"):
    val selection: GuideStarId = GuideStarId.ObservationTarget(none)
    assert(selection.isAmbiguousAmong(Candidates))
    assert(!selection.isAmbiguousAmong(SingleTargetCandidates))
    assert(!Target1.isAmbiguousAmong(Candidates))
    assert(!Gaia1.isAmbiguousAmong(Candidates))
    assertEquals(Results.pick(selection, Candidates), RemoteGSSelection(selection))

  test("it is ambiguous even when only one of the observation targets is usable"):
    val selection: GuideStarId = GuideStarId.ObservationTarget(none)
    assertEquals(SingleTargetResults.pick(selection, Candidates), RemoteGSSelection(selection))

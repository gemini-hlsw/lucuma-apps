// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import coulomb.syntax.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.ags.GuideStarCandidate
import lucuma.ags.GuideStarId
import lucuma.core.enums.Band
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.TargetDisposition
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Region
import lucuma.core.math.RightAscension
import lucuma.core.math.dimensional.syntax.*
import lucuma.core.math.units.*
import lucuma.core.model.Ephemeris
import lucuma.core.model.SiderealTracking
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.Target
import lucuma.schemas.model.TargetWithId
import munit.FunSuite

import scala.collection.immutable.SortedMap

class ObservationGuideStarsSuite extends FunSuite:
  private def targetId(n: Long): Target.Id = Target.Id.fromLong(n).get

  private val SiderealId: Target.Id    = targetId(1L)
  private val NonsiderealId: Target.Id = targetId(2L)
  private val OpportunityId: Target.Id = targetId(3L)
  private val BlindOffsetId: Target.Id = targetId(4L)

  private val SiderealCoords: Coordinates = Coordinates.Zero

  private val NonsiderealCoords: Coordinates =
    Coordinates(
      RightAscension.fromDoubleDegrees(10.0),
      Declination.fromAngle.getOption(Angle.fromDoubleDegrees(20.0)).get
    )

  private val RMagnitude: BrightnessValue = BrightnessValue.unsafeFrom(11.0)

  private val Profile: SourceProfile =
    SourceProfile.Point(
      SpectralDefinition.BandNormalized(
        none,
        SortedMap(Band.R -> RMagnitude.withUnit[VegaMagnitude].toMeasureTagged)
      )
    )

  private def name(s: String): NonEmptyString = NonEmptyString.unsafeFrom(s)

  private val Sidereal: TargetWithId =
    TargetWithId(
      SiderealId,
      Target.Sidereal(name("star"), SiderealTracking.const(SiderealCoords), Profile, none),
      TargetDisposition.Science,
      none
    )

  private val Nonsidereal: TargetWithId =
    TargetWithId(
      NonsiderealId,
      Target.Nonsidereal(name("Io"), Ephemeris.Key.MajorBody(501), Profile),
      TargetDisposition.Science,
      none
    )

  private val Opportunity: TargetWithId =
    TargetWithId(
      OpportunityId,
      Target.Opportunity(name("ToO"), Region.Full, Profile),
      TargetDisposition.Science,
      none
    )

  private val BlindOffset: TargetWithId =
    TargetWithId(
      BlindOffsetId,
      Target.Sidereal(name("blind"), SiderealTracking.const(SiderealCoords), Profile, none),
      TargetDisposition.BlindOffset,
      none
    )

  private def targets(ts: TargetWithId*): ObservationTargets =
    ObservationTargets.fromTargets(ts.toList).get

  private val NonsiderealAt: Target.Id => Option[Coordinates] =
    Map(NonsiderealId -> NonsiderealCoords).get

  private val NoPositions: Target.Id => Option[Coordinates] = _ => none

  test("a sidereal science target keeps its tracking, id and magnitudes"):
    val candidates: List[GuideStarCandidate] =
      ObservationGuideStars.candidates(targets(Sidereal), NoPositions)
    assertEquals(
      candidates,
      List(
        GuideStarCandidate(
          GuideStarId.ObservationTarget(SiderealId.some),
          SiderealTracking.const(SiderealCoords),
          SortedMap(Band.R -> RMagnitude)
        )
      )
    )

  test("a non-sidereal science target is fixed at its position at the observation time"):
    val candidates: List[GuideStarCandidate] =
      ObservationGuideStars.candidates(targets(Nonsidereal), NonsiderealAt)
    assertEquals(candidates.map(_.id), List(GuideStarId.ObservationTarget(NonsiderealId.some)))
    assertEquals(candidates.map(_.tracking), List(SiderealTracking.const(NonsiderealCoords)))

  test("a target without a position is skipped"):
    assertEquals(ObservationGuideStars.candidates(targets(Nonsidereal), NoPositions), Nil)
    assertEquals(ObservationGuideStars.candidates(targets(Opportunity), NonsiderealAt), Nil)

  test("only science targets are candidates"):
    val candidates: List[GuideStarCandidate] =
      ObservationGuideStars.candidates(targets(BlindOffset, Sidereal, Nonsidereal), NonsiderealAt)
    assertEquals(
      candidates.map(_.id),
      List(GuideStarId.ObservationTarget(SiderealId.some),
           GuideStarId.ObservationTarget(NonsiderealId.some)
      )
    )

  test("only a probe that guides on axis gets the observation targets"):
    val ts: Option[ObservationTargets] = targets(Sidereal).some
    assertEquals(
      ObservationGuideStars.candidatesFor(GuideProbe.AltairAOWFS.some, ts, NoPositions).map(_.id),
      List(GuideStarId.ObservationTarget(SiderealId.some))
    )
    assertEquals(ObservationGuideStars.candidatesFor(GuideProbe.PWFS2.some, ts, NoPositions), Nil)
    assertEquals(ObservationGuideStars.candidatesFor(none, ts, NoPositions), Nil)
    assertEquals(
      ObservationGuideStars.candidatesFor(GuideProbe.AltairAOWFS.some, none, NoPositions),
      Nil
    )

  private val Picked: GuideStarId = GuideStarId.ObservationTarget(SiderealId.some)

  test("a pick with a single science target is stored without the target id"):
    assertEquals(
      ObservationGuideStars.selectionToStore(Picked, targets(Sidereal, BlindOffset).some),
      GuideStarId.ObservationTarget(none)
    )

  test("a pick with several science targets is stored with the target id"):
    assertEquals(
      ObservationGuideStars.selectionToStore(Picked, targets(Sidereal, Nonsidereal).some),
      Picked
    )

  test("a catalog pick is stored as it is"):
    val gaia: GuideStarId = GuideStarId.gaia(123L)
    assertEquals(ObservationGuideStars.selectionToStore(gaia, targets(Sidereal).some), gaia)

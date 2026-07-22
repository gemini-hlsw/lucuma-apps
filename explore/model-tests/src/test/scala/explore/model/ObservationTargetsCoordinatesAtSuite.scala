// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.data.NonEmptyList
import cats.syntax.all.*
import lucuma.core.enums.TargetDisposition
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.RightAscension
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.schemas.model.SlotId
import lucuma.schemas.model.TargetWithId
import munit.FunSuite

import java.time.Instant

class ObservationTargetsCoordinatesAtSuite extends FunSuite:

  private val at: Instant = Instant.parse("2026-01-01T00:00:00Z")

  private def coords(raDeg: Double, decDeg: Double): Coordinates =
    Coordinates(
      RightAscension.fromDoubleDegrees(raDeg),
      Declination.fromDoubleDegrees(decDeg).get
    )

  private def sidereal(id: Long, c: Coordinates): (TargetWithId, SiderealTracking) =
    val tracking = SiderealTracking(c, Epoch.J2000, none, none, none)
    val target   = Target.Sidereal.tracking.replace(tracking)(EmptySiderealTarget)
    (TargetWithId(Target.Id.fromLong(id).get, target, TargetDisposition.Science, none), tracking)

  private val c1 = coords(10.0, 10.0)
  private val c2 = coords(10.1, 10.0)

  private val (t1, tr1) = sidereal(1, c1)
  private val (t2, tr2) = sidereal(2, c2)

  private val obsTargets: ObservationTargets =
    ObservationTargets.fromTargets(List(t1, t2)).get

  private val trackingMap: RegionOrTrackingMap =
    RegionOrTrackingMap.from(List(t1.id -> tr1.asRight, t2.id -> tr2.asRight))

  test("without an explicit base, the base is the centre of the science targets"):
    val result = ObservationTargetsCoordinatesAt(at, obsTargets, trackingMap, Nil).toOption.get
    assertEquals(result.baseCoords, Coordinates.centerOf(NonEmptyList.of(c1, c2)).some)

  test("an explicit base overrides the computed centre"):
    val explicit = coords(11.0, 12.0)
    val result   =
      ObservationTargetsCoordinatesAt(at, obsTargets, trackingMap, Nil, explicit.some).toOption.get
    assertEquals(result.baseCoords, explicit.some)

  // ADR 0002: SlotId.Base lives in baseCoords and nowhere else. A base leaked into `slots`
  // would be collected by `skySlots` and drawn -- and analysed by AGS -- as a sky position.
  test("an explicit base never appears in the slots map"):
    val explicit = coords(11.0, 12.0)
    val result   =
      ObservationTargetsCoordinatesAt(at, obsTargets, trackingMap, Nil, explicit.some).toOption.get
    assert(!result.slots.contains(SlotId.Base))
    assert(!result.slotCoords.contains(SlotId.Base))
    assertEquals(result.skySlots, Nil)
    assertEquals(result.skyCoords, Nil)

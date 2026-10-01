// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.utils

import cats.syntax.all.*
import lucuma.core.enums.Site
import lucuma.core.math.Coordinates
import lucuma.core.model.EphemerisTracking
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Tracking
import munit.FunSuite

import java.time.Duration
import java.time.Instant
import java.time.temporal.ChronoUnit

class ObsTimeOrDefaultSuite extends FunSuite:

  private val siderealTracking: Tracking = SiderealTracking.const(Coordinates.Zero)

  test("an explicit time is kept"):
    val explicitTime: Instant = Instant.parse("2026-03-01T04:00:00Z")
    assertEquals(
      obsTimeOrDefault(explicitTime.some, Site.GN.some, siderealTracking.some),
      explicitTime
    )

  test("a sidereal base defaults to its next transit"):
    val before: Instant  = Instant.now()
    val default: Instant = obsTimeOrDefault(none, Site.GS.some, siderealTracking.some)
    assert(default.isAfter(before), s"$default is not after $before")
    assert(
      default.isBefore(before.plus(Duration.ofHours(24))),
      s"$default is not within 24 h of $before"
    )

  test("without a usable tracking or a site the default is the start of the day"):
    val startOfDay: Instant                               = Instant.now().truncatedTo(ChronoUnit.DAYS)
    val fallbacks: List[(Option[Site], Option[Tracking])] =
      List(
        (Site.GN.some, none),
        (Site.GN.some, EphemerisTracking().some),
        (none, siderealTracking.some)
      )
    fallbacks.foreach: (site, tracking) =>
      assertEquals(obsTimeOrDefault(none, site, tracking), startOfDay)

  test("an explicit base overrides the asterism"):
    val explicitBase: Coordinates = Coordinates.Zero
    assertEquals(siderealBaseTracking(none, explicitBase.some),
                 Tracking.constant(explicitBase).some
    )

  test("no targets and no explicit base yield no tracking"):
    assertEquals(siderealBaseTracking(none, none), none)

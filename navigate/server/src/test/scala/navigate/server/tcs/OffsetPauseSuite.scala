// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import cats.syntax.all.*
import lucuma.core.enums.ComaOption
import lucuma.core.enums.Instrument
import lucuma.core.enums.M1Source
import lucuma.core.enums.MountGuideOption
import lucuma.core.enums.TipTiltSource
import lucuma.core.math.Angle
import lucuma.core.model.M1GuideConfig
import lucuma.core.model.M2GuideConfig
import lucuma.core.model.TelescopeGuideConfig
import munit.FunSuite
import navigate.model.Distance
import navigate.model.TrackingConfig
import navigate.server.tcs.TcsBaseControllerEpics.*

import scala.concurrent.duration.*

class OffsetPauseSuite extends FunSuite {

  private def arcsec(a: Double): Distance = Angle.fromBigDecimalArcseconds(a).toLengthInFocalPlane

  private def move(demand: (Double, Double), current: (Double, Double)): OffsetMove =
    OffsetMove.from(arcsec(demand._1), arcsec(demand._2), arcsec(current._1), arcsec(current._2))

  private def guiding(src: TipTiltSource, m1: M1Source): TelescopeGuideConfig =
    TelescopeGuideConfig(
      MountGuideOption.MountGuideOn,
      M1GuideConfig.M1GuideOn(m1),
      M2GuideConfig.M2GuideOn(ComaOption.ComaOn, Set(src)),
      Some(false),
      none
    )

  private val p1Guiding  = guiding(TipTiltSource.PWFS1, M1Source.PWFS1)
  private val p2Guiding  = guiding(TipTiltSource.PWFS2, M1Source.PWFS2)
  private val oiGuiding  = guiding(TipTiltSource.OIWFS, M1Source.OIWFS)
  private val noGuiding  = TelescopeGuideConfig(
    MountGuideOption.MountGuideOff,
    M1GuideConfig.M1GuideOff,
    M2GuideConfig.M2GuideOff,
    Some(false),
    none
  )
  private val tracking   = WfsGuideStates(TrackingConfig.defaultTracking,
                                        TrackingConfig.defaultTracking,
                                        TrackingConfig.defaultTracking
  )
  private val noTracking = WfsGuideStates.noTracking

  private def mustPause(
    m:  OffsetMove,
    g:  TelescopeGuideConfig,
    tr: WfsGuideStates = tracking,
    oi: Option[Instrument] = none
  ): Boolean = mustPauseWhileOffsetting(m.displacement, oi, g, tr)

  test("A displacement above the threshold pauses PWFS1 and PWFS2 guiding") {
    assert(mustPause(move((1.0, 0.0), (0.0, 0.0)), p1Guiding))
    assert(mustPause(move((1.0, 0.0), (0.0, 0.0)), p2Guiding))
  }

  test("A displacement below the threshold does not pause guiding") {
    assert(!mustPause(move((0.005, 0.0), (0.0, 0.0)), p1Guiding))
    assert(!mustPause(move((0.005, 0.0), (0.0, 0.0)), p2Guiding))
  }

  test("The criteria use the displacement, not the absolute offset: tiny move to a far offset") {
    assert(!mustPause(move((100.0, 50.0), (100.0, 50.001)), p1Guiding))
  }

  test("The criteria use the displacement, not the absolute offset: big move to a small offset") {
    assert(mustPause(move((0.001, 0.0), (5.0, 0.0)), p1Guiding))
  }

  test("A move that is symmetric around zero still counts its full length") {
    assert(mustPause(move((-5.0, 0.0), (5.0, 0.0)), p1Guiding))
  }

  test("No pause if no guider is in use") {
    assert(!mustPause(move((5.0, 0.0), (0.0, 0.0)), noGuiding))
  }

  test("A guider whose probe tracking is not active does not force a pause") {
    assert(!mustPause(move((5.0, 0.0), (0.0, 0.0)), p1Guiding, noTracking))
    assert(!mustPause(move((5.0, 0.0), (0.0, 0.0)), p2Guiding, noTracking))
    assert(
      !mustPause(move((5.0, 0.0), (0.0, 0.0)), oiGuiding, noTracking, Instrument.GmosNorth.some)
    )
  }

  test("OIWFS guiding pauses on a big displacement only for instruments with a threshold") {
    val m = move((5.0, 0.0), (0.0, 0.0))
    assert(mustPause(m, oiGuiding, oi = Instrument.GmosNorth.some))
    assert(mustPause(m, oiGuiding, oi = Instrument.Flamingos2.some))
    assert(!mustPause(m, oiGuiding, oi = Instrument.Gnirs.some))
    assert(!mustPause(m, oiGuiding, oi = none))
  }

  test("A move is null when the telescope is already at the demanded offset") {
    assert(move((5.0, -3.0), (5.0, -3.0)).isNull)
    assert(!move((5.0, -3.0), (5.0, -2.0)).isNull)
  }

  test("The in-position timeout covers the displacement, also when going back to zero") {
    val back = move((0.0, 0.0), (10.0, 0.0))
    assertEqualsDouble(back.displacementArcsec, 10.0, 0.01)
    assert(offsetTimeout(back.displacementArcsec) >= offsetTimeout(10.0) - 1.second)
    assert(offsetTimeout(back.displacementArcsec) > offsetTimeout(0.0))
  }
}

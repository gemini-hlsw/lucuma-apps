// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import lucuma.ags.GuideStarCandidate
import lucuma.core.enums.AltairMode
import lucuma.core.enums.AltairNdFilter
import lucuma.core.enums.Band
import lucuma.core.enums.CassRotator
import lucuma.core.enums.FieldLens
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.RightAscension
import lucuma.core.model.SiderealTracking
import lucuma.itc.AltairParameters
import lucuma.odb.data.AltairConfiguration
import munit.FunSuite

import java.time.Instant
import scala.collection.immutable.SortedMap

class AltairControlsSuite extends FunSuite:
  private val obsTime: Instant = Instant.parse("2026-09-26T08:00:00Z")

  private def coordinatesAtDecArcseconds(arcseconds: Double): Coordinates =
    Coordinates(
      RightAscension.Zero,
      Declination.fromAngle.getOption(Angle.fromDoubleArcseconds(arcseconds)).get
    )

  private def candidateAt(coordinates: Coordinates): GuideStarCandidate =
    GuideStarCandidate(1L,
                       SiderealTracking(coordinates, Epoch.J2000, none, none, none),
                       SortedMap.empty
    )

  private val twoArcseconds: Angle = Angle.fromDoubleArcseconds(2.0)

  private val halfArcsecond: Angle = Angle.fromDoubleArcseconds(0.5)

  private val rBrightness: BrightnessValue = BrightnessValue.unsafeFrom(12.5)

  private val candidateWithR: GuideStarCandidate =
    GuideStarCandidate(1L,
                       SiderealTracking(Coordinates.Zero, Epoch.J2000, none, none, none),
                       SortedMap(Band.R -> rBrightness)
    )

  private def altairIn(
    mode:              AltairMode,
    explicitFieldLens: Option[FieldLens]
  ): AltairConfiguration =
    AltairConfiguration(mode, explicitFieldLens, CassRotator.Following, AltairNdFilter.Out)

  test("separation is the distance between the base and the guide star"):
    val separation: Option[Angle] =
      AltairControls.guideStarSeparation(
        Coordinates.Zero.some,
        candidateAt(coordinatesAtDecArcseconds(2.0)).some,
        obsTime
      )
    assertEqualsDouble(separation.map(Angle.decimalArcseconds.get(_).toDouble).getOrElse(-1.0),
                       2.0,
                       1e-6
    )

  test("separation is unknown without a guide star"):
    assertEquals(AltairControls.guideStarSeparation(Coordinates.Zero.some, none, obsTime), none)

  test("separation is unknown without a base"):
    val candidate: GuideStarCandidate = candidateAt(Coordinates.Zero)
    assertEquals(AltairControls.guideStarSeparation(none, candidate.some, obsTime), none)

  test("only NGS offers the field lens; the laser modes lock the ND filter"):
    assertEquals(AltairControls.fieldLensSelectable(AltairMode.Ngs), true)
    assertEquals(AltairControls.fieldLensSelectable(AltairMode.Lgs), false)
    assertEquals(AltairControls.fieldLensSelectable(AltairMode.LgsP1), false)
    assertEquals(AltairControls.ndFilterLocked(AltairMode.Ngs), false)
    assertEquals(AltairControls.ndFilterLocked(AltairMode.Lgs), true)
    assertEquals(AltairControls.ndFilterLocked(AltairMode.LgsP1), true)

  test("NGS placeholder follows the guide star separation"):
    assertEquals(AltairControls.fieldLensPlaceholder(none), "Auto")
    assertEquals(AltairControls.fieldLensPlaceholder(twoArcseconds.some), "Auto (In)")
    assertEquals(
      AltairControls.fieldLensPlaceholder(Angle.fromDoubleArcseconds(0.5).some),
      "Auto (Out)"
    )

  test("LGS+P1 needs no guide star for the ITC"):
    assertEquals(
      AltairControls.itcParameters(altairIn(AltairMode.LgsP1, none), none, none),
      AltairParameters.LgsP1.some
    )

  test("NGS without a guide star has no ITC parameters"):
    assertEquals(AltairControls.itcParameters(altairIn(AltairMode.Ngs, none), none, none), none)

  test("NGS with a distant guide star puts the field lens in"):
    assertEquals(
      AltairControls.itcParameters(altairIn(AltairMode.Ngs, none),
                                   candidateWithR.some,
                                   twoArcseconds.some
      ),
      AltairParameters.Ngs(twoArcseconds, rBrightness, FieldLens.In).some
    )

  test("NGS with a close guide star takes the field lens out"):
    assertEquals(
      AltairControls.itcParameters(altairIn(AltairMode.Ngs, none),
                                   candidateWithR.some,
                                   halfArcsecond.some
      ),
      AltairParameters.Ngs(halfArcsecond, rBrightness, FieldLens.Out).some
    )

  test("an explicit NGS field lens wins over the separation"):
    assertEquals(
      AltairControls.itcParameters(altairIn(AltairMode.Ngs, FieldLens.Out.some),
                                   candidateWithR.some,
                                   twoArcseconds.some
      ),
      AltairParameters.Ngs(twoArcseconds, rBrightness, FieldLens.Out).some
    )

  test("LGS uses the tip/tilt star"):
    assertEquals(
      AltairControls.itcParameters(altairIn(AltairMode.Lgs, none),
                                   candidateWithR.some,
                                   twoArcseconds.some
      ),
      AltairParameters.Lgs(twoArcseconds, rBrightness).some
    )

  test("a guide star without an R brightness gives no ITC parameters"):
    assertEquals(
      AltairControls.itcParameters(altairIn(AltairMode.Ngs, none),
                                   candidateAt(Coordinates.Zero).some,
                                   twoArcseconds.some
      ),
      none
    )

  test("the ITC waits only with Altair and no parameters"):
    assertEquals(AltairControls.itcAwaitingGuideStar(none, none), false)
    assertEquals(AltairControls.itcAwaitingGuideStar(altairIn(AltairMode.Ngs, none).some, none),
                 true
    )
    assertEquals(
      AltairControls.itcAwaitingGuideStar(altairIn(AltairMode.LgsP1, none).some,
                                          AltairParameters.LgsP1.some
      ),
      false
    )

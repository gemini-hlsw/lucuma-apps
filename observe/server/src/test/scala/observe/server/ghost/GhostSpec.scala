// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.ghost

import cats.syntax.all.*
import coulomb.syntax.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import giapi.enums.GiapiStatusApply.*
import lucuma.core.enums.Band
import lucuma.core.enums.GalaxySpectrum
import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostIfu1FiberAgitator
import lucuma.core.enums.GhostIfu2FiberAgitator
import lucuma.core.enums.GhostReadMode
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.enums.ObserveClass
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.RightAscension
import lucuma.core.math.dimensional.syntax.*
import lucuma.core.math.units.VegaMagnitude
import lucuma.core.model.Ephemeris
import lucuma.core.model.SiderealTracking
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.Target
import lucuma.core.model.UnnormalizedSED
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.ghost.GhostDetector
import lucuma.core.util.TimeSpan
import observe.model.CurrentConditions

import java.time.temporal.ChronoUnit
import java.util.Locale
import scala.collection.immutable.SortedMap

/**
 * Tests GHOST Config typeclasses
 */
final class GhostSpec extends munit.DisciplineSuite with GhostArbitraries {
  // checkAll("Eq[GHOSTConfig]", EqTests[GhostConfig].eqv)

  val dec         = Declination.fromRadians(1.0).getOrElse(Declination.Max)
  val ra          = RightAscension.fromRadians(2.0)
  val coord1      = Coordinates(ra, dec)
  val target1     = Target.Sidereal(
    NonEmptyString.unsafeFrom("target01"),
    SiderealTracking(coord1, Epoch.J2000, none, none, none),
    SourceProfile.Point(
      SpectralDefinition.BandNormalized(
        UnnormalizedSED.Galaxy(GalaxySpectrum.Spiral).some,
        SortedMap(
          Band.V -> BrightnessValue.unsafeFrom(12.7).withUnit[VegaMagnitude].toMeasureTagged
        )
      )
    ),
    none
  )
  val dec2        = Declination.fromRadians(1.2).getOrElse(Declination.Max)
  val ra2         = RightAscension.fromRadians(0.3)
  val coord2      = Coordinates(ra2, dec2)
  val target2     = Target.Sidereal(
    NonEmptyString.unsafeFrom("target02"),
    SiderealTracking(coord2, Epoch.J2000, none, none, none),
    SourceProfile.Point(
      SpectralDefinition.BandNormalized(
        UnnormalizedSED.Galaxy(GalaxySpectrum.Spiral).some,
        SortedMap(
          Band.V -> BrightnessValue.unsafeFrom(12.7).withUnit[VegaMagnitude].toMeasureTagged
        )
      )
    ),
    none
  )
  val nonsidereal = Target.Nonsidereal(
    NonEmptyString.unsafeFrom("target01"),
    Ephemeris.Key.AsteroidNew("dummy"),
    SourceProfile.Point(
      SpectralDefinition.BandNormalized(
        UnnormalizedSED.Galaxy(GalaxySpectrum.Spiral).some,
        SortedMap(
          Band.V -> BrightnessValue.unsafeFrom(12.7).withUnit[VegaMagnitude].toMeasureTagged
        )
      )
    )
  )
  
  def formatDoubleValue(v: Double): String = "%1.6f".formatLocal(Locale.US, v)

  test("binning") {
    val cfg = StandardResolutionMode.SingleTarget(
      StepConfig.Science,
      ObserveClass.Science,
      GhostDetector.Blue(
        GhostDetector(TimeSpan.unsafeFromMicroseconds(1000000),
                      PosInt.unsafeFrom(1),
                      GhostBinning.OneByOne,
                      GhostReadMode.Fast
        )
      ),
      GhostDetector.Red(
        GhostDetector(TimeSpan.unsafeFromMicroseconds(1000000),
                      PosInt.unsafeFrom(1),
                      GhostBinning.OneByOne,
                      GhostReadMode.Slow
        )
      ),
      none,
      GhostIfu1FiberAgitator.Disabled,
      GhostIfu2FiberAgitator.Disabled,
      IFUTargetType.SiderealTarget(target1),
      coord1,
      List.empty,
      none,
      none,
      none
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value(GhostRedBinningRcf.applyItem),
                 "1".some
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value(GhostRedBinningCcf.applyItem),
                 "1".some
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value(GhostBlueBinningRcf.applyItem),
                 "1".some
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value(GhostBlueBinningCcf.applyItem),
                 "1".some
    )
  }

  test("fiber agitator on/off") {
    val cfg = StandardResolutionMode.SingleTarget(
      StepConfig.Science,
      ObserveClass.Science,
      GhostDetector.Blue(
        GhostDetector(TimeSpan.unsafeFromMicroseconds(1000000),
                      PosInt.unsafeFrom(1),
                      GhostBinning.OneByOne,
                      GhostReadMode.Fast
        )
      ),
      GhostDetector.Red(
        GhostDetector(TimeSpan.unsafeFromMicroseconds(1000000),
                      PosInt.unsafeFrom(1),
                      GhostBinning.OneByOne,
                      GhostReadMode.Slow
        )
      ),
      none,
      GhostIfu1FiberAgitator.Enabled,
      GhostIfu2FiberAgitator.Disabled,
      IFUTargetType.SiderealTarget(target1),
      coord1,
      List.empty,
      none,
      none,
      none
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value(GhostFiberAgitator1.applyItem),
                 "FA_DEMAND_NONE".some
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value(GhostFiberAgitator2.applyItem),
                 "FA_DEMAND_NONE".some
    )
  }

  test("fiber agitator on/off for bias") {
    val cfg = GhostCalibration(
      StepConfig.Bias,
      ObserveClass.DayCal,
      GhostDetector.Blue(
        GhostDetector(TimeSpan.unsafeFromMicroseconds(1000000),
                      PosInt.unsafeFrom(1),
                      GhostBinning.OneByOne,
                      GhostReadMode.Fast
        )
      ),
      GhostDetector.Red(
        GhostDetector(TimeSpan.unsafeFromMicroseconds(1000000),
                      PosInt.unsafeFrom(1),
                      GhostBinning.OneByOne,
                      GhostReadMode.Slow
        )
      ),
      none,
      GhostIfu1FiberAgitator.Enabled,
      GhostIfu2FiberAgitator.Disabled,
      GhostResolutionMode.Standard,
      none,
      false
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value(GhostFiberAgitator1.applyItem),
                 "FA_DEMAND_ON".some
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value(GhostFiberAgitator2.applyItem),
                 "FA_DEMAND_OFF".some
    )
  }

  test("sru ifu1 ra/dec") {
    val cfg = StandardResolutionMode.SingleTarget(
      StepConfig.Science,
      ObserveClass.Science,
      GhostDetector.Blue(
        GhostDetector(TimeSpan.unsafeFromMicroseconds(1000000),
                      PosInt.unsafeFrom(1),
                      GhostBinning.OneByOne,
                      GhostReadMode.Fast
        )
      ),
      GhostDetector.Red(
        GhostDetector(TimeSpan.unsafeFromMicroseconds(1000000),
                      PosInt.unsafeFrom(1),
                      GhostBinning.OneByOne,
                      GhostReadMode.Slow
        )
      ),
      none,
      GhostIfu1FiberAgitator.Disabled,
      GhostIfu2FiberAgitator.Disabled,
      IFUTargetType.SiderealTarget(target1),
      coord1,
      List.empty,
      none,
      none,
      none
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value("ghost:cc:cu:ifu1.type"),
                 "IFU_DEMAND_RADEC".some
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value("ghost:cc:cu:ifu1.ra"),
                 ra.toAngle.toDoubleDegrees.some.map(formatDoubleValue)
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value("ghost:cc:cu:ifu1.dec"),
                 dec.toAngle.toSignedDoubleDegrees.some.map(formatDoubleValue)
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value("ghost:cc:cu:ifu1.bundle"),
                 "IFU_STDRES".some
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value("ghost:cc:cu:ifu1.target"),
                 "IFU_TARGET_OBJECT".some
    )
    // ifu2 not used
    assertEquals(cfg.configuration(CurrentConditions.Default).value("ghost:cc:cu:ifu2.type"),
                 "IFU_DEMAND_PARK".some
    )
    assert(cfg.configuration(CurrentConditions.Default).value("ghost:cc:cu:ifu2.bundle").isEmpty)
    assertEquals(cfg.configuration(CurrentConditions.Default).value("ghost:cc:cu:ifu2.target"),
                 "IFU_TARGET_NONE".some
    )
  }

  test("sru ifu1/2 ra/dec") {
    val cfg = StandardResolutionMode.DualTarget(
      StepConfig.Science,
      ObserveClass.Science,
      GhostDetector.Blue(
        GhostDetector(TimeSpan.unsafeFromMicroseconds(1000000),
                      PosInt.unsafeFrom(1),
                      GhostBinning.OneByOne,
                      GhostReadMode.Fast
        )
      ),
      GhostDetector.Red(
        GhostDetector(TimeSpan.unsafeFromMicroseconds(1000000),
                      PosInt.unsafeFrom(1),
                      GhostBinning.OneByOne,
                      GhostReadMode.Slow
        )
      ),
      none,
      GhostIfu1FiberAgitator.Disabled,
      GhostIfu2FiberAgitator.Disabled,
      IFUTargetType.SiderealTarget(target1),
      coord1,
      IFUTargetType.SiderealTarget(target2),
      coord2,
      List.empty,
      none,
      none,
      none
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value("ghost:cc:cu:ifu1.type"),
                 "IFU_DEMAND_RADEC".some
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value("ghost:cc:cu:ifu1.ra"),
                 ra.toAngle.toDoubleDegrees.some.map(formatDoubleValue)
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value("ghost:cc:cu:ifu1.dec"),
                 dec.toAngle.toSignedDoubleDegrees.some.map(formatDoubleValue)
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value("ghost:cc:cu:ifu1.bundle"),
                 "IFU_STDRES".some
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value("ghost:cc:cu:ifu1.target"),
                 "IFU_TARGET_OBJECT".some
    )
    // ifu2 not used
    assertEquals(cfg.configuration(CurrentConditions.Default).value("ghost:cc:cu:ifu2.type"),
                 "IFU_DEMAND_RADEC".some
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value("ghost:cc:cu:ifu2.bundle"),
                 "IFU_STDRES".some
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value("ghost:cc:cu:ifu2.target"),
                 "IFU_TARGET_OBJECT".some
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value("ghost:cc:cu:ifu2.ra"),
                 ra2.toAngle.toDoubleDegrees.some.map(formatDoubleValue)
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value("ghost:cc:cu:ifu2.dec"),
                 dec2.toAngle.toSignedDoubleDegrees.some.map(formatDoubleValue)
    )
  }

  test("sru ifu1 park if only one used") {
    val cfg = StandardResolutionMode.SingleTarget(
      StepConfig.Science,
      ObserveClass.Science,
      GhostDetector.Blue(
        GhostDetector(TimeSpan.unsafeFromMicroseconds(1000000),
                      PosInt.unsafeFrom(1),
                      GhostBinning.OneByOne,
                      GhostReadMode.Fast
        )
      ),
      GhostDetector.Red(
        GhostDetector(TimeSpan.unsafeFromMicroseconds(1000000),
                      PosInt.unsafeFrom(1),
                      GhostBinning.OneByOne,
                      GhostReadMode.Slow
        )
      ),
      none,
      GhostIfu1FiberAgitator.Disabled,
      GhostIfu2FiberAgitator.Disabled,
      IFUTargetType.SiderealTarget(target1),
      coord1,
      List.empty,
      none,
      none,
      none
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value("ghost:cc:cu:ifu1.type"),
                 "IFU_DEMAND_RADEC".some
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value("ghost:cc:cu:ifu1.ra"),
                 ra.toAngle.toDoubleDegrees.some.map(formatDoubleValue)
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value("ghost:cc:cu:ifu1.dec"),
                 dec.toAngle.toSignedDoubleDegrees.some.map(formatDoubleValue)
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value("ghost:cc:cu:ifu1.bundle"),
                 "IFU_STDRES".some
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value("ghost:cc:cu:ifu1.target"),
                 "IFU_TARGET_OBJECT".some
    )
    // ifu2 not used
    assertEquals(cfg.configuration(CurrentConditions.Default).value("ghost:cc:cu:ifu2.type"),
                 "IFU_DEMAND_PARK".some
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value("ghost:cc:cu:ifu2.bundle"),
                 None
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value("ghost:cc:cu:ifu2.target"),
                 "IFU_TARGET_NONE".some
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value("ghost:cc:cu:ifu2.ra"), None)
    assertEquals(cfg.configuration(CurrentConditions.Default).value("ghost:cc:cu:ifu2.dec"), None)
  }

  test("Support SV overrides") {
    val cfg = StandardResolutionMode.SingleTarget(
      StepConfig.Science,
      ObserveClass.Science,
      GhostDetector.Blue(
        GhostDetector(TimeSpan.unsafeFromMicroseconds(1000000),
                      PosInt.unsafeFrom(1),
                      GhostBinning.OneByOne,
                      GhostReadMode.Slow
        )
      ),
      GhostDetector.Red(
        GhostDetector(TimeSpan.unsafeFromMicroseconds(1000000),
                      PosInt.unsafeFrom(1),
                      GhostBinning.OneByOne,
                      GhostReadMode.Fast
        )
      ),
      none,
      GhostIfu1FiberAgitator.Disabled,
      GhostIfu2FiberAgitator.Disabled,
      IFUTargetType.SiderealTarget(target1),
      coord1,
      List.empty,
      none,
      none,
      TimeSpan.unsafeFromDuration(5, ChronoUnit.SECONDS).some
    )

    assertEquals(cfg.configuration(CurrentConditions.Best).value(GhostSVDuration.applyItem),
                 Some("5000")
    )
    assertEquals(cfg.configuration(CurrentConditions.Best).value(GhostSVRepeat.applyItem),
                 Some("9")
    )
  }

  test("Support AG overrides".ignore) {
    val cfg = StandardResolutionMode.SingleTarget(
      StepConfig.Science,
      ObserveClass.Science,
      GhostDetector.Blue(
        GhostDetector(TimeSpan.unsafeFromMicroseconds(1000000),
                      PosInt.unsafeFrom(1),
                      GhostBinning.OneByOne,
                      GhostReadMode.Slow
        )
      ),
      GhostDetector.Red(
        GhostDetector(TimeSpan.unsafeFromMicroseconds(1000000),
                      PosInt.unsafeFrom(1),
                      GhostBinning.OneByOne,
                      GhostReadMode.Fast
        )
      ),
      none,
      GhostIfu1FiberAgitator.Disabled,
      GhostIfu2FiberAgitator.Disabled,
      IFUTargetType.SiderealTarget(target1),
      coord1,
      List.empty,
      none,
      TimeSpan.unsafeFromDuration(5, ChronoUnit.SECONDS).some,
      none
    )

    assertEquals(cfg.configuration(CurrentConditions.Default).value(GhostAGDuration.applyItem),
                 Some("50")
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value(GhostAGRepeat.applyItem), None)
  }

  test("Support SR non-sidereal") {
    val cfg = StandardResolutionMode.NonSiderealTarget(
      StepConfig.Science,
      ObserveClass.Science,
      GhostDetector.Blue(
        GhostDetector(TimeSpan.unsafeFromMicroseconds(1000000),
                      PosInt.unsafeFrom(1),
                      GhostBinning.OneByOne,
                      GhostReadMode.Slow
        )
      ),
      GhostDetector.Red(
        GhostDetector(TimeSpan.unsafeFromMicroseconds(1000000),
                      PosInt.unsafeFrom(1),
                      GhostBinning.OneByOne,
                      GhostReadMode.Fast
        )
      ),
      coord1.some,
      IFUTargetType.NonsiderealTarget(nonsidereal),
      GhostIfu1FiberAgitator.Disabled,
      GhostIfu2FiberAgitator.Disabled,
      List.empty,
      none,
      none,
      none
    )

    assertEquals(cfg.configuration(CurrentConditions.Best).value(GhostIFU1X.applyItem),
                 Some("0.000000")
    )
    assertEquals(cfg.configuration(CurrentConditions.Best).value(GhostIFU1Y.applyItem),
                 Some("0.000000")
    )
    assertEquals(cfg.configuration(CurrentConditions.Best).value(GhostIFU1Type.applyItem),
                 Some("IFU_DEMAND_XY")
    )
    assertEquals(cfg.configuration(CurrentConditions.Best).value(GhostIFU2Type.applyItem),
                 Some("IFU_DEMAND_PARK")
    )

  }

  test("Support HR non-sidereal") {
    val cfg = HighResolutionMode.NonSidereal(
      StepConfig.Science,
      ObserveClass.Science,
      GhostDetector.Blue(
        GhostDetector(TimeSpan.unsafeFromMicroseconds(1000000),
                      PosInt.unsafeFrom(1),
                      GhostBinning.OneByOne,
                      GhostReadMode.Fast
        )
      ),
      GhostDetector.Red(
        GhostDetector(TimeSpan.unsafeFromMicroseconds(1000000),
                      PosInt.unsafeFrom(1),
                      GhostBinning.OneByOne,
                      GhostReadMode.Slow
        )
      ),
      coord1.some,
      GhostIfu1FiberAgitator.Disabled,
      GhostIfu2FiberAgitator.Disabled,
      IFUTargetType.NonsiderealTarget(nonsidereal),
      List.empty,
      none,
      none,
      none
    )

    assertEquals(cfg.configuration(CurrentConditions.Default).value(GhostIFU1X.applyItem),
                 Some("0.000000")
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value(GhostIFU1Y.applyItem),
                 Some("0.000000")
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value(GhostIFU1Type.applyItem),
                 Some("IFU_DEMAND_XY")
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value(GhostIFU2X.applyItem),
                 Some("-100.000000")
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value(GhostIFU2Y.applyItem),
                 Some("0.000000")
    )
    assertEquals(cfg.configuration(CurrentConditions.Default).value(GhostIFU2Type.applyItem),
                 Some("IFU_DEMAND_XY")
    )

  }
}

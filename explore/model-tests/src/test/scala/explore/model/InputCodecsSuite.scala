// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import eu.timepit.refined.scalacheck.numeric.given
import eu.timepit.refined.scalacheck.string.given
import explore.model.enums.WavelengthUnits
import lucuma.core.arb.ArbTime.given
import lucuma.core.data.EmailAddress
import lucuma.core.math.Angle
import lucuma.core.math.ApparentRadialVelocity
import lucuma.core.math.BrightnessValue
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDelta
import lucuma.core.math.arb.ArbAngle.given
import lucuma.core.math.arb.ArbDeclination.given
import lucuma.core.math.arb.ArbEpoch.given
import lucuma.core.math.arb.ArbParallax.given
import lucuma.core.math.arb.ArbProperMotion.given
import lucuma.core.math.arb.ArbRadialVelocity.given
import lucuma.core.math.arb.ArbRedshift.given
import lucuma.core.math.arb.ArbRightAscension.given
import lucuma.core.util.arb.ArbTimeSpan.given
import lucuma.ui.input.InputCodecTests
import munit.DisciplineSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

/**
 * The codecs are checked over the domain their fields are meant for, so some lucuma-core
 * arbitraries are replaced:
 *   - `ApparentRadialVelocity` and `EmailAddress`: TEMPORARY workarounds for generator bugs fixed
 *     in lucuma-core (infinite velocities; sample addresses that fail `EmailPred`). Remove once a
 *     lucuma-core release with those fixes is in use.
 *   - `BrightnessValue`, `SignalToNoise`, `Wavelength`, `WavelengthDelta`: domain restrictions. The
 *     core generators cover the whole type (40-decimal brightnesses, picometer wavelengths), whose
 *     canonical text the Explore wedges themselves truncate away.
 */
final class InputCodecsSuite extends DisciplineSuite:
  private given Arbitrary[ApparentRadialVelocity] = Arbitrary(
    Gen
      .choose(-299000000L, 299000000L)
      .map(ms => ApparentRadialVelocity.kilometerspersecond.get(BigDecimal(ms) / 1000))
  )
  private given Arbitrary[BrightnessValue]        = Arbitrary(
    Gen.choose(-10000, 99999).map(milli => BrightnessValue.unsafeFrom(BigDecimal(milli) / 1000))
  )
  private given Arbitrary[SignalToNoise]          = Arbitrary(
    Gen
      .choose(0, 999999)
      .map(deci => SignalToNoise.unsafeFromBigDecimalExact(BigDecimal(deci) / 10))
  )
  private given Arbitrary[Wavelength]             = Arbitrary(
    Gen.choose(300000, 30000000).map(Wavelength.unsafeFromIntPicometers)
  )
  private given Arbitrary[WavelengthDelta]        = Arbitrary(
    Gen.choose(1, 30000000).map(WavelengthDelta.unsafeFromIntPicometers)
  )
  // Angles below 180° print as non-negative signed arcseconds.
  private val nonNegativeAngles: Arbitrary[Angle] = Arbitrary(
    Gen.choose(0L, Angle.Angle180.toMicroarcseconds - 1).map(Angle.fromMicroarcseconds)
  )
  private val zoomFactors: Arbitrary[BigDecimal]  = Arbitrary(
    Gen.choose(1, 100000).map(n => BigDecimal(n) / 100)
  )
  private given Arbitrary[EmailAddress]           = Arbitrary(
    for
      user   <- Gen.listOfN(6, Gen.alphaNumChar).map(_.mkString)
      domain <- Gen.listOfN(6, Gen.alphaLowerChar).map(_.mkString)
    yield EmailAddress.unsafeFrom(s"$user@$domain.org")
  )

  checkAll("Ra", InputCodecTests(inputCodecs.Ra))
  checkAll("Dec", InputCodecTests(inputCodecs.Dec))
  checkAll("EpochNoScheme", InputCodecTests(inputCodecs.EpochNoScheme))
  checkAll("ProperMotionRa", InputCodecTests(inputCodecs.ProperMotionRa))
  checkAll("ProperMotionDec", InputCodecTests(inputCodecs.ProperMotionDec))
  checkAll("ParallaxMas", InputCodecTests(inputCodecs.ParallaxMas))
  checkAll("RedshiftZ", InputCodecTests(inputCodecs.RedshiftZ))
  checkAll("ApparentRadialVelocityCz", InputCodecTests(inputCodecs.ApparentRadialVelocityCz))
  checkAll("RadialVelocityKms", InputCodecTests(inputCodecs.RadialVelocityKms))
  checkAll("PositionAngleDegrees", InputCodecTests(inputCodecs.PositionAngleDegrees))
  checkAll("Fwhm", InputCodecTests(inputCodecs.Fwhm))
  checkAll("DecimalArcseconds", InputCodecTests(inputCodecs.DecimalArcseconds))
  checkAll("NonNegativeArcseconds",
           InputCodecTests(inputCodecs.NonNegativeArcseconds)(using nonNegativeAngles)
  )
  checkAll(
    "NonNegativeArcsecondsOptional",
    InputCodecTests(inputCodecs.NonNegativeArcsecondsOptional)(using
      Arbitrary(Gen.option(nonNegativeAngles.arbitrary))
    )
  )
  checkAll("SignedArcsecondsOptional", InputCodecTests(inputCodecs.SignedArcsecondsOptional))
  checkAll("FinderChartZoom", InputCodecTests(inputCodecs.FinderChartZoom)(using zoomFactors))
  checkAll("Integer", InputCodecTests(inputCodecs.Integer))
  checkAll("NonNegativeInt", InputCodecTests(inputCodecs.NonNegativeInt))
  checkAll("DurationHm", InputCodecTests(inputCodecs.DurationHm))
  checkAll("DurationHmOptional", InputCodecTests(inputCodecs.DurationHmOptional))
  checkAll("DurationHms", InputCodecTests(inputCodecs.DurationHms))
  checkAll("Time24h", InputCodecTests(inputCodecs.Time24h))
  checkAll("ExposureTimeSeconds", InputCodecTests(inputCodecs.ExposureTimeSeconds))
  checkAll("ExposureTimeMilliseconds", InputCodecTests(inputCodecs.ExposureTimeMilliseconds))
  checkAll("PositiveInt", InputCodecTests(inputCodecs.PositiveInt))
  checkAll("ExposureCount", InputCodecTests(inputCodecs.ExposureCount))
  checkAll("Percent", InputCodecTests(inputCodecs.Percent))
  checkAll("SignalToNoiseOptional", InputCodecTests(inputCodecs.SignalToNoiseOptional))
  checkAll("Brightness", InputCodecTests(inputCodecs.Brightness))
  checkAll("BlackBodyTemperatureK", InputCodecTests(inputCodecs.BlackBodyTemperatureK))
  checkAll("NonEmptyText", InputCodecTests(inputCodecs.NonEmptyText))
  checkAll("NonEmptyTextOptional", InputCodecTests(inputCodecs.NonEmptyTextOptional))
  checkAll("EmailOptional", InputCodecTests(inputCodecs.EmailOptional))
  WavelengthUnits.values.foreach: units =>
    checkAll(s"wavelength($units)", InputCodecTests(inputCodecs.wavelength(units)))
    checkAll(s"wavelengthOptional($units)", InputCodecTests(inputCodecs.wavelengthOptional(units)))
    checkAll(s"wavelengthDelta($units)", InputCodecTests(inputCodecs.wavelengthDelta(units)))
    checkAll(s"sequenceWavelength($units)", InputCodecTests(inputCodecs.sequenceWavelength(units)))

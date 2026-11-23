// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.data.NonEmptyList
import eu.timepit.refined.api.Refined
import eu.timepit.refined.scalacheck.numeric.given
import eu.timepit.refined.scalacheck.string.given
import eu.timepit.refined.types.numeric.PosInt
import explore.model.enums.WavelengthUnits
import lucuma.core.arb.ArbTime.given
import lucuma.core.data.EmailAddress
import lucuma.core.math.Angle
import lucuma.core.math.ApparentRadialVelocity
import lucuma.core.math.BrightnessValue
import lucuma.core.math.FluxDensityContinuumValue
import lucuma.core.math.LineFluxValue
import lucuma.core.math.LineWidthValue
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDelta
import lucuma.core.math.WavelengthDither
import lucuma.core.math.arb.ArbAngle.given
import lucuma.core.math.arb.ArbDeclination.given
import lucuma.core.math.arb.ArbEpoch.given
import lucuma.core.math.arb.ArbParallax.given
import lucuma.core.math.arb.ArbProperMotion.given
import lucuma.core.math.arb.ArbRadialVelocity.given
import lucuma.core.math.arb.ArbRedshift.given
import lucuma.core.math.arb.ArbRightAscension.given
import lucuma.core.model.ElevationRange
import lucuma.core.model.arb.ArbElevationRange.given
import lucuma.core.model.sequence.ghost as ghost
import lucuma.core.model.sequence.gnirs.GnirsFocusMotorStepRefinement
import lucuma.core.model.sequence.igrins2 as igrins2
import lucuma.core.model.validation.ModelValidators
import lucuma.core.syntax.validation.*
import lucuma.core.util.TimeSpan
import lucuma.refined.*
import lucuma.ui.input.InputCodecTests
import munit.DisciplineSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

/**
 * The codecs are checked over the domain their fields are meant for, so some lucuma-core
 * arbitraries are replaced:
 *   - `ApparentRadialVelocity` and `EmailAddress`: TEMPORARY workarounds for generator bugs fixed
 *     in lucuma-core (infinite velocities; sample addresses that fail `EmailPred`). Remove once a
 *     lucuma-core release with those fixes is in use.
 *   - `BrightnessValue`, `SignalToNoise`, `Wavelength`, `WavelengthDelta`: domain restrictions. The
 *     core generators cover the whole type (40-decimal brightnesses, picometer wavelengths), whose
 *     canonical text the Explore wedges themselves round away.
 *   - `TimeSpan`: the duration masks accept at most 5 hour digits.
 *   - Offset and IFU angles: their masks accept at most 3 integer digits, so they are checked up to
 *     999.99". Larger offsets or IFU sizes aren't realistic, so the limit is kept; the mask isn't
 *     attached to a field whose text doesn't fit it (see `useInputmask`).
 *
 * Codecs whose values can come from the ODB with more precision than a user would type (line widths
 * and fluxes, power law indices) are checked over the values the type can hold.
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

  private given Arbitrary[TimeSpan] = Arbitrary(
    Gen.choose(0L, 100000L * 3600 * 1000000 - 1).map(TimeSpan.unsafeFromMicroseconds)
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

  // Dithers within the three integer digits and one decimal that the mask accepts.
  private val dithers: Arbitrary[Option[NonEmptyList[WavelengthDither]]] =
    val dither: Gen[WavelengthDither] =
      Gen.choose(-9999, 9999).map(tenthsNm => WavelengthDither.intPicometers.get(tenthsNm * 100))

    Arbitrary(Gen.option(Gen.nonEmptyListOf(dither).map(NonEmptyList.fromListUnsafe)))

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
  checkAll("ExposureTimeSeconds", InputCodecTests(inputCodecs.exposureTimeSeconds(List.empty)))
  checkAll(
    "ExposureTimeMilliseconds",
    InputCodecTests(inputCodecs.exposureTimeMilliseconds(List.empty))
  )
  checkAll("PositiveInt", InputCodecTests(inputCodecs.PositiveInt))
  checkAll(
    "CalibrationSetCount",
    InputCodecTests(inputCodecs.CalibrationSetCount)(using
      Arbitrary(Gen.choose(1, inputCodecs.MaxCalibrationSetCount.value).map(PosInt.unsafeFrom))
    )
  )
  checkAll("ExposureCount", InputCodecTests(inputCodecs.ExposureCount))
  checkAll("Percent", InputCodecTests(inputCodecs.Percent))
  checkAll("SignalToNoiseOptional", InputCodecTests(inputCodecs.SignalToNoiseOptional))
  checkAll("Brightness", InputCodecTests(inputCodecs.Brightness))
  checkAll("BlackBodyTemperatureK", InputCodecTests(inputCodecs.BlackBodyTemperatureK))
  checkAll("NonEmptyText", InputCodecTests(inputCodecs.NonEmptyText))
  checkAll("NonEmptyTextOptional", InputCodecTests(inputCodecs.NonEmptyTextOptional))
  checkAll("EmailOptional", InputCodecTests(inputCodecs.EmailOptional))
  checkAll(
    "wavelengthDithers",
    InputCodecTests(
      inputCodecs.wavelengthDithers(
        ExploreModelValidators.ditherValidWedge.toNel(",".refined).optional
      )
    )(using dithers)
  )
  WavelengthUnits.values.foreach: units =>
    checkAll(s"wavelength($units)", InputCodecTests(inputCodecs.wavelength(units)))
    checkAll(s"wavelengthOptional($units)", InputCodecTests(inputCodecs.wavelengthOptional(units)))
    checkAll(s"wavelengthDelta($units)", InputCodecTests(inputCodecs.wavelengthDelta(units)))
    checkAll(s"sequenceWavelength($units)", InputCodecTests(inputCodecs.sequenceWavelength(units)))

  /** Decimals from `lo` to `hi` with up to `maxScale` decimals. */
  private def decimals(lo: BigDecimal, hi: BigDecimal, maxScale: Int): Gen[BigDecimal] =
    for
      scale    <- Gen.choose(0, maxScale)
      unscaled <- Gen.choose((lo * BigDecimal(10).pow(scale)).toLongExact,
                             (hi * BigDecimal(10).pow(scale)).toLongExact
                  )
    yield BigDecimal(unscaled, scale)

  /** Small positive values, like line fluxes in W/m² (e.g. `1.23456e-17`). */
  private val tinyPositives: Gen[BigDecimal] =
    for
      mantissa <- Gen.choose(1L, 999999L)
      scale    <- Gen.choose(10, 30)
    yield BigDecimal(mantissa, scale)

  private def timeSpansBetween(min: TimeSpan, max: TimeSpan): Arbitrary[TimeSpan] =
    Arbitrary(
      Gen.choose(min.toMicroseconds, max.toMicroseconds).map(TimeSpan.unsafeFromMicroseconds)
    )

  // Offset and IFU angles, which their masks limit to 999.99".
  private val MaxOffsetMicroarcseconds: Long = 999990000L

  private val offsetAngles: Arbitrary[Angle] = Arbitrary(
    Gen
      .choose(-MaxOffsetMicroarcseconds, MaxOffsetMicroarcseconds)
      .map(Angle.fromMicroarcseconds)
  )

  // From 0.01", so that they are still positive once rounded.
  private val positiveOffsetAngles: Arbitrary[Angle] = Arbitrary(
    Gen.choose(5000L, MaxOffsetMicroarcseconds).map(Angle.fromMicroarcseconds)
  )

  private val nonNegativeOffsetAngles: Arbitrary[Angle] = Arbitrary(
    Gen.choose(0L, MaxOffsetMicroarcseconds).map(Angle.fromMicroarcseconds)
  )

  checkAll("OffsetArcseconds", InputCodecTests(inputCodecs.OffsetArcseconds)(using offsetAngles))
  checkAll(
    "ifuArcseconds(positive)",
    InputCodecTests(
      inputCodecs.ifuArcseconds(ExploreModelValidators.positiveDecimalArcsecondsValidWedge)
    )(using positiveOffsetAngles)
  )
  checkAll(
    "ifuArcseconds(nonNegative)",
    InputCodecTests(
      inputCodecs.ifuArcseconds(ExploreModelValidators.nonNegativeDecimalArcsecondsValidWedge)
    )(using nonNegativeOffsetAngles)
  )
  checkAll(
    "elevationRange(airMass)",
    InputCodecTests(inputCodecs.elevationRange(ModelValidators.AirMassElevationRangeValidWedge))(
      using Arbitrary(arbitrary[ElevationRange.ByAirMass].flatMap(r => Gen.oneOf(r.min, r.max)))
    )
  )
  checkAll(
    "elevationRange(hourAngle)",
    InputCodecTests(
      inputCodecs.elevationRange(ModelValidators.HourAngleElevationRangeValidWedge)
    )(using
      Arbitrary(
        arbitrary[ElevationRange.ByHourAngle].flatMap(r => Gen.oneOf(r.minHours, r.maxHours))
      )
    )
  )
  checkAll(
    "Igrins2CalibrationExposure",
    InputCodecTests(inputCodecs.Igrins2CalibrationExposure)(using
      timeSpansBetween(igrins2.MinExposureTime, igrins2.MaxExposureTime)
    )
  )
  checkAll(
    "GhostCalibrationExposure",
    InputCodecTests(inputCodecs.GhostCalibrationExposure)(using
      timeSpansBetween(ghost.MinExposureTime, ghost.MaxExposureTime)
    )
  )
  checkAll(
    "calibrationExposureMilliseconds",
    InputCodecTests(inputCodecs.calibrationExposureMilliseconds(List.empty))
  )
  checkAll(
    "ProposalHours",
    InputCodecTests(inputCodecs.ProposalHours)(using
      Arbitrary(decimals(0, 1000, 4).map(Refined.unsafeApply[BigDecimal, HourRange]))
    )
  )
  checkAll(
    "GnirsFocusMotorSteps",
    InputCodecTests(inputCodecs.GnirsFocusMotorSteps)(using
      Arbitrary(
        Gen
          .choose(-179999, 180000)
          .map(Refined.unsafeApply[Int, GnirsFocusMotorStepRefinement])
      )
    )
  )
  checkAll(
    "PowerLawIndex",
    InputCodecTests(inputCodecs.PowerLawIndex)(using Arbitrary(decimals(-100, 100, 8)))
  )
  checkAll(
    "LineWidth",
    InputCodecTests(inputCodecs.LineWidth)(using
      Arbitrary(decimals(0, 1000000, 6).suchThat(_ > 0).map(LineWidthValue.unsafeFrom))
    )
  )
  checkAll(
    "LineFlux",
    InputCodecTests(inputCodecs.LineFlux)(using
      Arbitrary(tinyPositives.map(LineFluxValue.unsafeFrom))
    )
  )
  checkAll(
    "FluxDensityContinuum",
    InputCodecTests(inputCodecs.FluxDensityContinuum)(using
      Arbitrary(tinyPositives.map(FluxDensityContinuumValue.unsafeFrom))
    )
  )

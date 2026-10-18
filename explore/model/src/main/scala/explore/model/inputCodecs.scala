// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.enums.WavelengthUnits
import explore.model.formats.*
import lucuma.core.data.EmailAddress
import lucuma.core.math.Angle
import lucuma.core.math.ApparentRadialVelocity
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.FluxDensityContinuumValue
import lucuma.core.math.FluxDensityContinuumValueRefinement
import lucuma.core.math.LineFluxValue
import lucuma.core.math.LineFluxValueRefinement
import lucuma.core.math.LineWidthValue
import lucuma.core.math.LineWidthValueRefinement
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.math.RadialVelocity
import lucuma.core.math.Redshift
import lucuma.core.math.RightAscension
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDelta
import lucuma.core.math.validation.MathValidators
import lucuma.core.model.ZeroTo100
import lucuma.core.model.sequence.gnirs.GnirsFocusMotorStepRefinement
import lucuma.core.util.TimeSpan
import lucuma.core.validation.*
import lucuma.refined.*
import lucuma.ui.input.InputCodec
import lucuma.ui.input.Masks
import lucuma.ui.input.facade.InputmaskOptions
import lucuma.ui.input.toCodec

import java.time.LocalTime

/**
 * The input codecs of Explore's text fields: parser/printer plus typing mask, defined once so that
 * `InputCodecsSuite` can check that each mask accepts the canonical text of every value.
 */
trait inputCodecs:
  // Coordinates
  val Ra: InputCodec[RightAscension]            = InputCodec.TruncatedRa
  val Dec: InputCodec[Declination]              = InputCodec.TruncatedDec
  val EpochNoScheme: InputCodec[Epoch]          =
    MathValidators.epochNoScheme.toCodec.withDecimalMask(3, allowMinus = false, integerDigits = 4.some)
  val ProperMotionRa: InputCodec[ProperMotion.RA]   =
    ExploreModelValidators.pmRAValidWedge.toCodec.withDecimalMask(3)
  val ProperMotionDec: InputCodec[ProperMotion.Dec] =
    ExploreModelValidators.pmDecValidWedge.toCodec.withDecimalMask(3)
  val ParallaxMas: InputCodec[Parallax]             = ExploreModelValidators.pxValidWedge.toCodec.withDecimalMask(3)

  // Line of sight motion
  val RedshiftZ: InputCodec[Redshift]                       =
    InputValidSplitEpi.fromFormat(formatZ, "Must be a number".refined).toCodec.withDecimalMask(9)
  val ApparentRadialVelocityCz: InputCodec[ApparentRadialVelocity] =
    InputValidSplitEpi.fromFormat(formatCZ, "Must be a number".refined).toCodec.withDecimalMask(10)
  val RadialVelocityKms: InputCodec[RadialVelocity]         =
    InputValidSplitEpi.fromFormat(formatRV, "Must be a number".refined).toCodec.withDecimalMask(3)

  // Angles
  val PositionAngleDegrees: InputCodec[Angle] =
    MathValidators.truncatedAngleDegrees.toCodec.withDecimalMask(2, integerDigits = 3.some)
  val Fwhm: InputCodec[Angle]                 =
    MathValidators.angleArcSec.toCodec.withUnboundedDecimalMask(allowMinus = false)
  val DecimalArcseconds: InputCodec[Angle]    =
    ExploreModelValidators.decimalArcsecondsValidWedge.toCodec.withDecimalMask(2)
  val OffsetArcseconds: InputCodec[Angle]     =
    ExploreModelValidators.decimalArcsecondsValidWedge.toCodec.withDecimalMask(2, integerDigits = 3.some)
  val NonNegativeArcseconds: InputCodec[Angle] =
    ExploreModelValidators.decimalArcsecondsValidWedge.toCodec.withDecimalMask(2, allowMinus = false)
  val NonNegativeArcsecondsOptional: InputCodec[Option[Angle]] =
    ExploreModelValidators.decimalArcsecondsValidWedge.optional.toCodec
      .withDecimalMask(2, allowMinus = false)
  val SignedArcsecondsOptional: InputCodec[Option[Angle]] =
    ExploreModelValidators.decimalArcsecondsValidWedge.optional.toCodec.withDecimalMask(2)
  /** Finder chart zoom factor. */
  val FinderChartZoom: InputCodec[BigDecimal] =
    InputValidWedge.truncatedBigDecimal(2.refined).toCodec.withDecimalMask(2)

  /** IFU field sizes vary by kind, so the parser is supplied; the mask is shared. */
  def ifuArcseconds(valid: InputValidFormat[Angle]): InputCodec[Angle] =
    valid.toCodec.withDecimalMask(2, allowMinus = false, integerDigits = 3.some)

  /** Air mass and hour angle bounds depend on the other bound, so the parser is supplied. */
  def elevationRange[A](valid: InputValidFormat[A]): InputCodec[A] = valid.toCodec.withDecimalMask(1)

  // Time
  val DurationHm: InputCodec[TimeSpan]                   = durationHM.toCodec.withRegexMask(Masks.DurationHm)
  val DurationHmOptional: InputCodec[Option[TimeSpan]]   =
    durationHM.optional.toCodec.withRegexMask(Masks.DurationHm)
  val DurationHms: InputCodec[TimeSpan]                  = durationHMS.toCodec.withRegexMask(Masks.DurationHms)
  val Time24h: InputCodec[LocalTime]                     = time24h.toCodec.withRegexMask(Masks.Hm)
  val ExposureTimeSeconds: InputCodec[Option[TimeSpan]]  =
    durationS.optional.toCodec.withIntegerMask(allowMinus = false)
  val ExposureTimeMilliseconds: InputCodec[Option[TimeSpan]] =
    durationMs.optional.toCodec.withDecimalMask(2, allowMinus = false)
  val ProposalHours: InputCodec[BigDecimal Refined HourRange] =
    ExploreModelValidators.hoursValidWedge.toCodec.withDecimalMask(2)

  // Counts and numbers
  val Integer: InputCodec[Int]                        = InputValidSplitEpi.int.toCodec.withIntegerMask()
  val NonNegativeInt: InputCodec[NonNegInt]           =
    InputValidSplitEpi.nonNegInt.toCodec.withIntegerMask(allowMinus = false)
  val GnirsFocusMotorSteps: InputCodec[Int Refined GnirsFocusMotorStepRefinement] =
    ExploreModelValidators.GnirsFocusMotorStepsValidSplitEpi.toCodec.withIntegerMask()
  val PositiveInt: InputCodec[PosInt]                 =
    InputValidSplitEpi.posInt.toCodec.withIntegerMask(allowMinus = false)
  val ExposureCount: InputCodec[Option[PosInt]]       =
    InputValidSplitEpi.posInt.optional.toCodec.withIntegerMask(allowMinus = false)
  val Percent: InputCodec[Int Refined ZeroTo100]      =
    InputValidSplitEpi.refinedInt[ZeroTo100].toCodec.withRegexMask(Masks.segment(100, 3))
  // SignalToNoise prints three decimals (milli-S/N), so the mask must accept them.
  val SignalToNoiseOptional: InputCodec[Option[SignalToNoise]] =
    ExploreModelValidators.signalToNoiseValidSplitEpi.optional.toCodec
      .withDecimalMask(3, allowMinus = false)
  val Brightness: InputCodec[BrightnessValue]         =
    ExploreModelValidators.brightnessValidWedge.toCodec
      .withScientificMask(integerDigits = 2, decimals = 3, exponentDigits = 2)
  val PowerLawIndex: InputCodec[BigDecimal]           =
    InputValidSplitEpi.bigDecimal.toCodec.withUnboundedDecimalMask()
  val BlackBodyTemperatureK: InputCodec[PosInt]       =
    InputValidSplitEpi.posInt.toCodec.withIntegerMask(allowMinus = false)
  val LineWidth: InputCodec[LineWidthValue]           =
    InputValidSplitEpi
      .refinedBigDecimal[LineWidthValueRefinement]
      .andThen(LineWidthValue.Value.reverse)
      .toCodec
      .withDecimalMask(3, allowMinus = false)
  val LineFlux: InputCodec[LineFluxValue]             =
    InputValidSplitEpi
      .refinedBigDecimalWithScientificNotation[LineFluxValueRefinement]
      .andThen(LineFluxValue.Value.reverse)
      .toCodec
      .withScientificMask(allowMinus = false)
  val FluxDensityContinuum: InputCodec[FluxDensityContinuumValue] =
    InputValidSplitEpi
      .refinedBigDecimalWithScientificNotation[FluxDensityContinuumValueRefinement]
      .andThen(FluxDensityContinuumValue.Value.reverse)
      .toCodec
      .withScientificMask(allowMinus = false)

  // Wavelengths
  // The decimals follow each unit's wedge: μm prints four, nm one, Å none.
  private def wavelengthMask(units: WavelengthUnits): InputmaskOptions =
    units match
      case WavelengthUnits.Micrometers => Masks.decimalOptions(4.some, false, none)
      case WavelengthUnits.Nanometers  => Masks.decimalOptions(1.some, false, none)
      case WavelengthUnits.Angstroms   => Masks.integerOptions(false, none)

  def wavelengthValid(units: WavelengthUnits): InputValidWedge[Wavelength] =
    units match
      case WavelengthUnits.Micrometers => ExploreModelValidators.wavelengthMicroValidWedge
      case WavelengthUnits.Nanometers  => ExploreModelValidators.wavelengthNanoValidWedge
      case WavelengthUnits.Angstroms   => ExploreModelValidators.wavelengthAngstromValidWedge

  def wavelength(units: WavelengthUnits): InputCodec[Wavelength] =
    wavelengthValid(units).toCodec.withMask(wavelengthMask(units))

  def wavelengthOptional(units: WavelengthUnits): InputCodec[Option[Wavelength]] =
    wavelengthValid(units).optional.toCodec.withMask(wavelengthMask(units))

  def wavelengthDelta(units: WavelengthUnits): InputCodec[Option[WavelengthDelta]] =
    val valid: InputValidWedge[WavelengthDelta] =
      units match
        case WavelengthUnits.Micrometers => ExploreModelValidators.wavelengthMicroDeltaValidWedge
        case WavelengthUnits.Nanometers  => ExploreModelValidators.wavelengthNanoDeltaValidWedge
        case WavelengthUnits.Angstroms   => ExploreModelValidators.wavelengthAngstromDeltaValidWedge
    valid.optional.toCodec.withMask(wavelengthMask(units))

  /** Step wavelengths keep 0.1 nm precision; the μm wedge already prints four decimals. */
  def sequenceWavelength(units: WavelengthUnits): InputCodec[Wavelength] = wavelength(units)

  /** Comma separated dithers in nm; the parser depends on the mode, so it is supplied. */
  def wavelengthDithers[A](valid: InputValidFormat[A]): InputCodec[A] =
    valid.toCodec.withRegexMask(Masks.commaList(Masks.decimalPattern(3, 1, allowMinus = true)))

  // Text
  val Text: InputCodec[String]                            = InputCodec.Text
  val NonEmptyText: InputCodec[NonEmptyString]            = InputValidSplitEpi.nonEmptyString.toCodec
  val NonEmptyTextOptional: InputCodec[Option[NonEmptyString]] =
    InputValidSplitEpi.nonEmptyString.optional.toCodec
  val EmailOptional: InputCodec[Option[EmailAddress]]     =
    ExploreModelValidators.MailValidator.optional.toCodec

object inputCodecs extends inputCodecs

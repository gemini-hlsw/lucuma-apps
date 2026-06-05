// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model.arb

import cats.data.NonEmptyList
import cats.laws.discipline.arbitrary.*
import cats.syntax.all.*
import eu.timepit.refined.scalacheck.all.given
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.*
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDither
import lucuma.core.math.arb.ArbAngle
import lucuma.core.math.arb.ArbOffset
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.math.arb.ArbWavelengthDither
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.arb.ArbExposureTimeMode
import lucuma.core.model.sequence.arb.ArbSlitTelescopeConfigs
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMode
import lucuma.core.model.sequence.gnirs.GnirsFocusMotorStepsValue
import lucuma.core.model.sequence.gnirs.GnirsGratingWavelength
import lucuma.core.model.sequence.gnirs.arb.ArbGnirsAcquisitionMode
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbNewType
import lucuma.core.util.arb.ArbTimeSpan
import lucuma.schemas.model.CentralWavelength
import lucuma.schemas.model.GmosImagingVariant
import lucuma.schemas.model.ObservingMode
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

import scala.annotation.targetName

trait ArbObservingMode {
  import ArbAngle.given
  import ArbEnumerated.given
  import ArbExposureTimeMode.given
  import ArbGmosImagingVariant.given
  import ArbGnirsAcquisitionMode.given
  import ArbOffset.given
  import ArbSlitTelescopeConfigs.given
  import ArbTimeSpan.given
  import ArbWavelength.given
  import ArbWavelengthDither.given

  given given_Arbitrary_GmosNorthLongSlit_Acquisition
    : Arbitrary[ObservingMode.GmosNorthLongSlit.Acquisition] =
    Arbitrary[ObservingMode.GmosNorthLongSlit.Acquisition](
      for
        defaultFilter  <- arbitrary[GmosNorthFilter]
        explicitFilter <- arbitrary[Option[GmosNorthFilter]]
        defaultRoi     <- arbitrary[GmosLongSlitAcquisitionRoi]
        explicitRoi    <- arbitrary[Option[GmosLongSlitAcquisitionRoi]]
        etm            <- arbitrary[ExposureTimeMode]
      yield ObservingMode.GmosNorthLongSlit.Acquisition(
        defaultFilter,
        explicitFilter,
        defaultRoi,
        explicitRoi,
        etm
      )
    )

  given given_Cogen_GmosNorthLongSlit_Acquisition
    : Cogen[ObservingMode.GmosNorthLongSlit.Acquisition] =
    Cogen[
      (GmosNorthFilter,
       Option[GmosNorthFilter],
       GmosLongSlitAcquisitionRoi,
       Option[GmosLongSlitAcquisitionRoi],
       ExposureTimeMode
      )
    ]
      .contramap(a =>
        (a.defaultFilter, a.explicitFilter, a.defaultRoi, a.explicitRoi, a.exposureTimeMode)
      )

  given given_Arbitrary_GmosSouthLongSlit_Acquisition
    : Arbitrary[ObservingMode.GmosSouthLongSlit.Acquisition] =
    Arbitrary[ObservingMode.GmosSouthLongSlit.Acquisition](
      for
        defaultFilter  <- arbitrary[GmosSouthFilter]
        explicitFilter <- arbitrary[Option[GmosSouthFilter]]
        defaultRoi     <- arbitrary[GmosLongSlitAcquisitionRoi]
        explicitRoi    <- arbitrary[Option[GmosLongSlitAcquisitionRoi]]
        etm            <- arbitrary[ExposureTimeMode]
      yield ObservingMode.GmosSouthLongSlit.Acquisition(
        defaultFilter,
        explicitFilter,
        defaultRoi,
        explicitRoi,
        etm
      )
    )

  given given_Cogen_GmosSouthLongSlit_Acquisition
    : Cogen[ObservingMode.GmosSouthLongSlit.Acquisition] =
    Cogen[
      (GmosSouthFilter,
       Option[GmosSouthFilter],
       GmosLongSlitAcquisitionRoi,
       Option[GmosLongSlitAcquisitionRoi],
       ExposureTimeMode
      )
    ]
      .contramap(a =>
        (a.defaultFilter, a.explicitFilter, a.defaultRoi, a.explicitRoi, a.exposureTimeMode)
      )

  given Arbitrary[ObservingMode.GmosNorthLongSlit] =
    Arbitrary[ObservingMode.GmosNorthLongSlit](
      for
        initialGrating            <- arbitrary[GmosNorthGrating]
        grating                   <- arbitrary[GmosNorthGrating]
        initialFilter             <- arbitrary[Option[GmosNorthFilter]]
        filter                    <- arbitrary[Option[GmosNorthFilter]]
        initialFpu                <- arbitrary[GmosNorthFpu]
        fpu                       <- arbitrary[GmosNorthFpu]
        initialCentralWavelength  <- arbitrary[Wavelength]
        centralWavelength         <- arbitrary[Wavelength]
        defaultXBin               <- arbitrary[GmosXBinning]
        explicitXBin              <- arbitrary[Option[GmosXBinning]]
        defaultYBin               <- arbitrary[GmosYBinning]
        explicitYBin              <- arbitrary[Option[GmosYBinning]]
        defaultAmpReadMode        <- arbitrary[GmosAmpReadMode]
        explicitAmpReadMode       <- arbitrary[Option[GmosAmpReadMode]]
        defaultAmpGain            <- arbitrary[GmosAmpGain]
        explicitAmpGain           <- arbitrary[Option[GmosAmpGain]]
        defaultRoi                <- arbitrary[GmosRoi]
        explicitRoi               <- arbitrary[Option[GmosRoi]]
        defaultWavelengthDithers  <- arbitrary[NonEmptyList[WavelengthDither]]
        explicitWavelengthDithers <- arbitrary[Option[NonEmptyList[WavelengthDither]]]
        defaultOffsets            <- arbitrary[NonEmptyList[Offset.Q]]
        explicitOffsets           <- arbitrary[Option[NonEmptyList[Offset.Q]]]
        exposureTimeMode          <- arbitrary[ExposureTimeMode]
        acquisition               <- arbitrary[ObservingMode.GmosNorthLongSlit.Acquisition]
      yield ObservingMode.GmosNorthLongSlit(
        initialGrating,
        grating,
        initialFilter,
        filter,
        initialFpu,
        fpu,
        CentralWavelength(initialCentralWavelength),
        CentralWavelength(centralWavelength),
        defaultXBin,
        explicitXBin,
        defaultYBin,
        explicitYBin,
        defaultAmpReadMode,
        explicitAmpReadMode,
        defaultAmpGain,
        explicitAmpGain,
        defaultRoi,
        explicitRoi,
        defaultWavelengthDithers,
        explicitWavelengthDithers,
        defaultOffsets,
        explicitOffsets,
        exposureTimeMode,
        acquisition
      )
    )

  given Arbitrary[ObservingMode.GmosSouthLongSlit] =
    Arbitrary[ObservingMode.GmosSouthLongSlit](
      for
        initialGrating            <- arbitrary[GmosSouthGrating]
        grating                   <- arbitrary[GmosSouthGrating]
        initialFilter             <- arbitrary[Option[GmosSouthFilter]]
        filter                    <- arbitrary[Option[GmosSouthFilter]]
        initialFpu                <- arbitrary[GmosSouthFpu]
        fpu                       <- arbitrary[GmosSouthFpu]
        initialCentralWavelength  <- arbitrary[Wavelength]
        centralWavelength         <- arbitrary[Wavelength]
        defaultXBin               <- arbitrary[GmosXBinning]
        explicitXBin              <- arbitrary[Option[GmosXBinning]]
        defaultYBin               <- arbitrary[GmosYBinning]
        explicitYBin              <- arbitrary[Option[GmosYBinning]]
        defaultAmpReadMode        <- arbitrary[GmosAmpReadMode]
        explicitAmpReadMode       <- arbitrary[Option[GmosAmpReadMode]]
        defaultAmpGain            <- arbitrary[GmosAmpGain]
        explicitAmpGain           <- arbitrary[Option[GmosAmpGain]]
        defaultRoi                <- arbitrary[GmosRoi]
        explicitRoi               <- arbitrary[Option[GmosRoi]]
        defaultWavelengthDithers  <- arbitrary[NonEmptyList[WavelengthDither]]
        explicitWavelengthDithers <- arbitrary[Option[NonEmptyList[WavelengthDither]]]
        defaultOffsets            <- arbitrary[NonEmptyList[Offset.Q]]
        explicitOffsets           <- arbitrary[Option[NonEmptyList[Offset.Q]]]
        exposureTimeMode          <- arbitrary[ExposureTimeMode]
        acquisition               <- arbitrary[ObservingMode.GmosSouthLongSlit.Acquisition]
      yield ObservingMode.GmosSouthLongSlit(
        initialGrating,
        grating,
        initialFilter,
        filter,
        initialFpu,
        fpu,
        CentralWavelength(initialCentralWavelength),
        CentralWavelength(centralWavelength),
        defaultXBin,
        explicitXBin,
        defaultYBin,
        explicitYBin,
        defaultAmpReadMode,
        explicitAmpReadMode,
        defaultAmpGain,
        explicitAmpGain,
        defaultRoi,
        explicitRoi,
        defaultWavelengthDithers,
        explicitWavelengthDithers,
        defaultOffsets,
        explicitOffsets,
        exposureTimeMode,
        acquisition
      )
    )

  given Cogen[ObservingMode.GmosNorthLongSlit] =
    Cogen[
      (GmosNorthGrating,
       GmosNorthGrating,
       Option[GmosNorthFilter],
       Option[GmosNorthFilter],
       GmosNorthFpu,
       GmosNorthFpu,
       Wavelength,
       Wavelength,
       GmosXBinning,
       Option[GmosXBinning],
       GmosYBinning,
       Option[GmosYBinning],
       GmosAmpReadMode,
       Option[GmosAmpReadMode],
       GmosAmpGain,
       Option[GmosAmpGain],
       GmosRoi,
       Option[GmosRoi],
       NonEmptyList[WavelengthDither],
       Option[NonEmptyList[WavelengthDither]],
       NonEmptyList[Offset.Q],
       (Option[NonEmptyList[Offset.Q]],
        ExposureTimeMode,
        ObservingMode.GmosNorthLongSlit.Acquisition
       )
      )
    ]
      .contramap(o =>
        (o.initialGrating,
         o.grating,
         o.initialFilter,
         o.filter,
         o.initialFpu,
         o.fpu,
         o.initialCentralWavelength.value,
         o.centralWavelength.value,
         o.defaultXBin,
         o.explicitXBin,
         o.defaultYBin,
         o.explicitYBin,
         o.defaultAmpReadMode,
         o.explicitAmpReadMode,
         o.defaultAmpGain,
         o.explicitAmpGain,
         o.defaultRoi,
         o.explicitRoi,
         o.defaultWavelengthDithers,
         o.explicitWavelengthDithers,
         o.defaultOffsets,
         (o.explicitOffsets, o.exposureTimeMode, o.acquisition)
        )
      )

  given Cogen[ObservingMode.GmosSouthLongSlit] =
    Cogen[
      (GmosSouthGrating,
       GmosSouthGrating,
       Option[GmosSouthFilter],
       Option[GmosSouthFilter],
       GmosSouthFpu,
       GmosSouthFpu,
       Wavelength,
       Wavelength,
       GmosXBinning,
       Option[GmosXBinning],
       GmosYBinning,
       Option[GmosYBinning],
       GmosAmpReadMode,
       Option[GmosAmpReadMode],
       GmosAmpGain,
       Option[GmosAmpGain],
       GmosRoi,
       Option[GmosRoi],
       NonEmptyList[WavelengthDither],
       Option[NonEmptyList[WavelengthDither]],
       NonEmptyList[Offset.Q],
       (Option[NonEmptyList[Offset.Q]],
        ExposureTimeMode,
        ObservingMode.GmosSouthLongSlit.Acquisition
       )
      )
    ]
      .contramap(o =>
        (o.initialGrating,
         o.grating,
         o.initialFilter,
         o.filter,
         o.initialFpu,
         o.fpu,
         o.initialCentralWavelength.value,
         o.centralWavelength.value,
         o.defaultXBin,
         o.explicitXBin,
         o.defaultYBin,
         o.explicitYBin,
         o.defaultAmpReadMode,
         o.explicitAmpReadMode,
         o.defaultAmpGain,
         o.explicitAmpGain,
         o.defaultRoi,
         o.explicitRoi,
         o.defaultWavelengthDithers,
         o.explicitWavelengthDithers,
         o.defaultOffsets,
         (o.explicitOffsets, o.exposureTimeMode, o.acquisition)
        )
      )

  given arbGmosNorthImagingFilter: Arbitrary[ObservingMode.GmosNorthImaging.ImagingFilter] =
    Arbitrary(
      for {
        filter <- arbitrary[GmosNorthFilter]
        etm    <- arbitrary[ExposureTimeMode]
      } yield ObservingMode.GmosNorthImaging.ImagingFilter(filter, etm)
    )

  given Arbitrary[ObservingMode.GmosNorthImaging] =
    Arbitrary[ObservingMode.GmosNorthImaging](
      for
        variant             <- arbitrary[GmosImagingVariant]
        initialFilters      <- arbitrary[NonEmptyList[ObservingMode.GmosNorthImaging.ImagingFilter]]
        filters             <- arbitrary[NonEmptyList[ObservingMode.GmosNorthImaging.ImagingFilter]]
        defaultBin          <- arbitrary[GmosBinning]
        explicitBin         <- arbitrary[Option[GmosBinning]]
        defaultAmpReadMode  <- arbitrary[GmosAmpReadMode]
        explicitAmpReadMode <- arbitrary[Option[GmosAmpReadMode]]
        defaultAmpGain      <- arbitrary[GmosAmpGain]
        explicitAmpGain     <- arbitrary[Option[GmosAmpGain]]
        defaultRoi          <- arbitrary[GmosRoi]
        explicitRoi         <- arbitrary[Option[GmosRoi]]
        offsets             <- arbitrary[List[Offset]]
      yield ObservingMode.GmosNorthImaging(
        variant,
        initialFilters,
        filters,
        defaultBin,
        explicitBin,
        defaultAmpReadMode,
        explicitAmpReadMode,
        defaultAmpGain,
        explicitAmpGain,
        defaultRoi,
        explicitRoi
      )
    )

  given arbGmosSouthImagingFilter: Arbitrary[ObservingMode.GmosSouthImaging.ImagingFilter] =
    Arbitrary(
      for {
        filter <- arbitrary[GmosSouthFilter]
        etm    <- arbitrary[ExposureTimeMode]
      } yield ObservingMode.GmosSouthImaging.ImagingFilter(filter, etm)
    )

  given Arbitrary[ObservingMode.GmosSouthImaging] =
    Arbitrary[ObservingMode.GmosSouthImaging](
      for
        variant             <- arbitrary[GmosImagingVariant]
        initialFilters      <- arbitrary[NonEmptyList[ObservingMode.GmosSouthImaging.ImagingFilter]]
        filters             <- arbitrary[NonEmptyList[ObservingMode.GmosSouthImaging.ImagingFilter]]
        defaultBin          <- arbitrary[GmosBinning]
        explicitBin         <- arbitrary[Option[GmosBinning]]
        defaultAmpReadMode  <- arbitrary[GmosAmpReadMode]
        explicitAmpReadMode <- arbitrary[Option[GmosAmpReadMode]]
        defaultAmpGain      <- arbitrary[GmosAmpGain]
        explicitAmpGain     <- arbitrary[Option[GmosAmpGain]]
        defaultRoi          <- arbitrary[GmosRoi]
        explicitRoi         <- arbitrary[Option[GmosRoi]]
        offsets             <- arbitrary[List[Offset]]
      yield ObservingMode.GmosSouthImaging(
        variant,
        initialFilters,
        filters,
        defaultBin,
        explicitBin,
        defaultAmpReadMode,
        explicitAmpReadMode,
        defaultAmpGain,
        explicitAmpGain,
        defaultRoi,
        explicitRoi
      )
    )

  given arbFlamingos2ImagingFilter: Arbitrary[ObservingMode.Flamingos2Imaging.ImagingFilter] =
    Arbitrary(
      for {
        filter <- arbitrary[Flamingos2Filter]
        etm    <- arbitrary[ExposureTimeMode]
      } yield ObservingMode.Flamingos2Imaging.ImagingFilter(filter, etm)
    )

  given Arbitrary[ObservingMode.Flamingos2Imaging] =
    Arbitrary[ObservingMode.Flamingos2Imaging](
      for
        initialFilters         <- arbitrary[NonEmptyList[ObservingMode.Flamingos2Imaging.ImagingFilter]]
        filters                <- arbitrary[NonEmptyList[ObservingMode.Flamingos2Imaging.ImagingFilter]]
        defaultReadMode        <- arbitrary[Flamingos2ReadMode]
        explicitReadMode       <- arbitrary[Option[Flamingos2ReadMode]]
        defaultReads           <- arbitrary[Flamingos2Reads]
        explicitReads          <- arbitrary[Option[Flamingos2Reads]]
        defaultDecker          <- arbitrary[Flamingos2Decker]
        explicitDecker         <- arbitrary[Option[Flamingos2Decker]]
        defaultReadoutMode     <- arbitrary[Flamingos2ReadoutMode]
        explicitReadoutMode    <- arbitrary[Option[Flamingos2ReadoutMode]]
        defaultSpatialOffsets  <- arbitrary[List[Offset]]
        explicitSpatialOffsets <- arbitrary[Option[List[Offset]]]
      yield ObservingMode.Flamingos2Imaging(
        initialFilters,
        filters,
        defaultReadMode,
        explicitReadMode,
        defaultReads,
        explicitReads,
        defaultDecker,
        explicitDecker,
        defaultReadoutMode,
        explicitReadoutMode,
        defaultSpatialOffsets,
        explicitSpatialOffsets
      )
    )

  given Arbitrary[ObservingMode.Flamingos2LongSlit.Acquisition] =
    Arbitrary[ObservingMode.Flamingos2LongSlit.Acquisition](
      for {
        defaultFilter    <- arbitrary[Flamingos2Filter]
        explicitFilter   <- arbitrary[Option[Flamingos2Filter]]
        exposureTimeMode <- arbitrary[ExposureTimeMode]
      } yield ObservingMode.Flamingos2LongSlit.Acquisition(
        defaultFilter,
        explicitFilter,
        exposureTimeMode
      )
    )

  given Arbitrary[ObservingMode.Flamingos2LongSlit] =
    Arbitrary[ObservingMode.Flamingos2LongSlit](
      for {
        initialDisperser   <- arbitrary[Flamingos2Disperser]
        disperser          <- arbitrary[Flamingos2Disperser]
        initialFilter      <- arbitrary[Flamingos2Filter]
        filter             <- arbitrary[Flamingos2Filter]
        initialFpu         <- arbitrary[Flamingos2Fpu]
        fpu                <- arbitrary[Flamingos2Fpu]
        explicitReadMode   <- arbitrary[Option[Flamingos2ReadMode]]
        explicitReads      <- arbitrary[Option[Flamingos2Reads]]
        defaultDecker      <- arbitrary[Flamingos2Decker]
        explicitDecker     <- arbitrary[Option[Flamingos2Decker]]
        defaultReadoutMode <- arbitrary[Flamingos2ReadoutMode]
        expicitReadoutMode <- arbitrary[Option[Flamingos2ReadoutMode]]
        defaultOffsets     <- arbitrary[NonEmptyList[Offset]]
        explicitOffsets    <- arbitrary[Option[NonEmptyList[Offset]]]
        exposureTimeMode   <- arbitrary[ExposureTimeMode]
        acquisition        <- arbitrary[ObservingMode.Flamingos2LongSlit.Acquisition]
      } yield ObservingMode.Flamingos2LongSlit(
        initialDisperser,
        disperser,
        initialFilter,
        filter,
        initialFpu,
        fpu,
        explicitReadMode,
        explicitReads,
        defaultDecker,
        explicitDecker,
        defaultReadoutMode,
        expicitReadoutMode,
        defaultOffsets,
        explicitOffsets,
        exposureTimeMode,
        acquisition
      )
    )

  given Cogen[ObservingMode.Flamingos2LongSlit.Acquisition] =
    Cogen[
      (Flamingos2Filter, Option[Flamingos2Filter], ExposureTimeMode)
    ]
      .contramap(a => (a.defaultFilter, a.explicitFilter, a.exposureTimeMode))

  given Cogen[ObservingMode.Flamingos2LongSlit] =
    Cogen[
      (Flamingos2Disperser,
       Flamingos2Disperser,
       Flamingos2Filter,
       Flamingos2Filter,
       Flamingos2Fpu,
       Flamingos2Fpu,
       Option[Flamingos2ReadMode],
       Option[Flamingos2Reads],
       Flamingos2Decker,
       Option[Flamingos2Decker],
       Flamingos2ReadoutMode,
       Option[Flamingos2ReadoutMode],
       NonEmptyList[Offset],
       Option[NonEmptyList[Offset]],
       ExposureTimeMode,
       ObservingMode.Flamingos2LongSlit.Acquisition
      )
    ]
      .contramap(o =>
        (
          o.initialDisperser,
          o.disperser,
          o.initialFilter,
          o.filter,
          o.initialFpu,
          o.fpu,
          o.explicitReadMode,
          o.explicitReads,
          o.defaultDecker,
          o.explicitDecker,
          o.defaultReadoutMode,
          o.explicitReadoutMode,
          o.defaultOffsets,
          o.explicitOffsets,
          o.exposureTimeMode,
          o.acquisition
        )
      )

  given cogenGmosNorthImagingFilter: Cogen[ObservingMode.GmosNorthImaging.ImagingFilter] =
    Cogen[(GmosNorthFilter, ExposureTimeMode)].contramap(i => (i.filter, i.exposureTimeMode))

  given Cogen[ObservingMode.GmosNorthImaging] =
    Cogen[
      (
        GmosImagingVariant,
        NonEmptyList[ObservingMode.GmosNorthImaging.ImagingFilter],
        NonEmptyList[ObservingMode.GmosNorthImaging.ImagingFilter],
        GmosBinning,
        Option[GmosBinning],
        GmosAmpReadMode,
        Option[GmosAmpReadMode],
        GmosAmpGain,
        Option[GmosAmpGain],
        GmosRoi,
        Option[GmosRoi]
      )
    ]
      .contramap(o =>
        (
          o.variant,
          o.initialFilters,
          o.filters,
          o.defaultBin,
          o.explicitBin,
          o.defaultAmpReadMode,
          o.explicitAmpReadMode,
          o.defaultAmpGain,
          o.explicitAmpGain,
          o.defaultRoi,
          o.explicitRoi
        )
      )

  given cogenGmosSouthImagingFilter: Cogen[ObservingMode.GmosSouthImaging.ImagingFilter] =
    Cogen[(GmosSouthFilter, ExposureTimeMode)].contramap(i => (i.filter, i.exposureTimeMode))

  given Cogen[ObservingMode.GmosSouthImaging] =
    Cogen[
      (GmosImagingVariant,
       NonEmptyList[ObservingMode.GmosSouthImaging.ImagingFilter],
       NonEmptyList[ObservingMode.GmosSouthImaging.ImagingFilter],
       GmosBinning,
       Option[GmosBinning],
       GmosAmpReadMode,
       Option[GmosAmpReadMode],
       GmosAmpGain,
       Option[GmosAmpGain],
       GmosRoi,
       Option[GmosRoi]
      )
    ]
      .contramap(o =>
        (
          o.variant,
          o.initialFilters,
          o.filters,
          o.defaultBin,
          o.explicitBin,
          o.defaultAmpReadMode,
          o.explicitAmpReadMode,
          o.defaultAmpGain,
          o.explicitAmpGain,
          o.defaultRoi,
          o.explicitRoi
        )
      )

  given cogenFlamingos2ImagingFilter: Cogen[ObservingMode.Flamingos2Imaging.ImagingFilter] =
    Cogen[(Flamingos2Filter, ExposureTimeMode)].contramap(i => (i.filter, i.exposureTimeMode))

  given Cogen[ObservingMode.Flamingos2Imaging] =
    Cogen[
      (
        NonEmptyList[ObservingMode.Flamingos2Imaging.ImagingFilter],
        NonEmptyList[ObservingMode.Flamingos2Imaging.ImagingFilter],
        Flamingos2ReadMode,
        Option[Flamingos2ReadMode],
        Flamingos2Reads,
        Option[Flamingos2Reads],
        Flamingos2Decker,
        Option[Flamingos2Decker],
        Flamingos2ReadoutMode,
        Option[Flamingos2ReadoutMode],
        List[Offset],
        Option[List[Offset]]
      )
    ]
      .contramap(o =>
        (
          o.initialFilters,
          o.filters,
          o.defaultReadMode,
          o.explicitReadMode,
          o.defaultReads,
          o.explicitReads,
          o.defaultDecker,
          o.explicitDecker,
          o.defaultReadoutMode,
          o.explicitReadoutMode,
          o.defaultSpatialOffsets,
          o.explicitSpatialOffsets
        )
      )

  given Arbitrary[ObservingMode.Igrins2LongSlit] = Arbitrary[ObservingMode.Igrins2LongSlit](
    for {
      exposureTimeMode      <- arbitrary[ExposureTimeMode]
      defaultOffsetMode     <- arbitrary[SlitOffsetMode]
      explicitOffsetMode    <- arbitrary[Option[SlitOffsetMode]]
      defaultSaveSVCImages  <- arbitrary[Boolean]
      explicitSaveSVCImages <- arbitrary[Option[Boolean]]
      defaultOffsets        <- arbitrary[NonEmptyList[Offset]]
      explicitOffsets       <- arbitrary[Option[NonEmptyList[Offset]]]
    } yield ObservingMode.Igrins2LongSlit(
      exposureTimeMode,
      defaultOffsetMode,
      explicitOffsetMode,
      defaultSaveSVCImages,
      explicitSaveSVCImages,
      defaultOffsets,
      explicitOffsets
    )
  )

  given Cogen[ObservingMode.Igrins2LongSlit] =
    Cogen[
      (ExposureTimeMode,
       SlitOffsetMode,
       Option[SlitOffsetMode],
       Boolean,
       Option[Boolean],
       NonEmptyList[Offset],
       Option[NonEmptyList[Offset]]
      )
    ]
      .contramap(o =>
        (
          o.exposureTimeMode,
          o.defaultOffsetMode,
          o.explicitOffsetMode,
          o.defaultSaveSVCImages,
          o.explicitSaveSVCImages,
          o.defaultOffsets,
          o.explicitOffsets
        )
      )

  @targetName("gnirsLongSlitAcquisitionArbitrary")
  given Arbitrary[ObservingMode.GnirsLongSlit.Acquisition] =
    Arbitrary[ObservingMode.GnirsLongSlit.Acquisition](
      for {
        explicitAcquisitionMode <- arbitrary[Option[GnirsAcquisitionMode]]
        explicitFilter          <- arbitrary[Option[GnirsFilter]]
        exposureTimeMode        <- arbitrary[ExposureTimeMode]
        coadds                  <- arbitrary[PosInt]
      } yield ObservingMode.GnirsLongSlit.Acquisition(
        explicitAcquisitionMode,
        explicitFilter,
        exposureTimeMode,
        coadds
      )
    )

  @targetName("gnirsLongSlitAcquisitionCogen")
  given Cogen[ObservingMode.GnirsLongSlit.Acquisition] =
    Cogen[
      (Option[GnirsAcquisitionMode], Option[GnirsFilter], ExposureTimeMode, PosInt)
    ].contramap(a => (a.explicitAcquisitionMode, a.explicitFilter, a.exposureTimeMode, a.coadds))

  given Arbitrary[ObservingMode.GnirsLongSlit] =
    import ArbNewType.given
    Arbitrary[ObservingMode.GnirsLongSlit](
      for {
        initialGrating            <- arbitrary[GnirsGrating]
        grating                   <- arbitrary[GnirsGrating]
        initialFilter             <- arbitrary[GnirsFilter]
        filter                    <- arbitrary[GnirsFilter]
        initialFpu                <- arbitrary[GnirsFpuSlit]
        fpu                       <- arbitrary[GnirsFpuSlit]
        initialPrism              <- arbitrary[GnirsPrism]
        prism                     <- arbitrary[GnirsPrism]
        initialCamera             <- arbitrary[GnirsCamera]
        camera                    <- arbitrary[GnirsCamera]
        defaultGratingWavelength  <- arbitrary[GnirsGratingWavelength]
        explicitGratingWavelength <- arbitrary[Option[GnirsGratingWavelength]]
        defaultDecker             <- arbitrary[GnirsDecker]
        explicitDecker            <- arbitrary[Option[GnirsDecker]]
        explicitReadMode          <- arbitrary[Option[GnirsReadMode]]
        defaultWellDepth          <- arbitrary[GnirsWellDepth]
        explicitWellDepth         <- arbitrary[Option[GnirsWellDepth]]
        explicitFocusMotorSteps   <- arbitrary[Option[GnirsFocusMotorStepsValue]]
        defaultTelescopeConfigs   <- arbitrary[SlitTelescopeConfigs]
        explicitTelescopeConfigs  <- arbitrary[Option[SlitTelescopeConfigs]]
        exposureTimeMode          <- arbitrary[ExposureTimeMode]
        coadds                    <- arbitrary[PosInt]
        acquisition               <- arbitrary[ObservingMode.GnirsLongSlit.Acquisition]
      } yield ObservingMode.GnirsLongSlit(
        initialGrating,
        grating,
        initialFilter,
        filter,
        initialFpu,
        fpu,
        initialPrism,
        prism,
        initialCamera,
        camera,
        defaultGratingWavelength,
        explicitGratingWavelength,
        defaultDecker,
        explicitDecker,
        explicitReadMode,
        defaultWellDepth,
        explicitWellDepth,
        explicitFocusMotorSteps,
        defaultTelescopeConfigs,
        explicitTelescopeConfigs,
        exposureTimeMode,
        coadds,
        acquisition
      )
    )

  // We exceed the max number of fields for contramap, so we use tuples.
  given Cogen[ObservingMode.GnirsLongSlit] =
    import ArbNewType.given
    Cogen[
      ((GnirsGrating, GnirsGrating),
       (GnirsFilter, GnirsFilter),
       (GnirsFpuSlit, GnirsFpuSlit),
       (GnirsPrism, GnirsPrism),
       (GnirsCamera, GnirsCamera),
       (GnirsGratingWavelength, Option[GnirsGratingWavelength]),
       (GnirsDecker, Option[GnirsDecker]),
       Option[GnirsReadMode],
       (GnirsWellDepth, Option[GnirsWellDepth]),
       Option[GnirsFocusMotorStepsValue],
       (SlitTelescopeConfigs, Option[SlitTelescopeConfigs]),
       ExposureTimeMode,
       (PosInt, ObservingMode.GnirsLongSlit.Acquisition)
      )
    ]
      .contramap: o =>
        (
          (o.initialGrating, o.grating),
          (o.initialFilter, o.filter),
          (o.initialFpu, o.fpu),
          (o.initialPrism, o.prism),
          (o.initialCamera, o.camera),
          (o.defaultGratingWavelength, o.explicitGratingWavelength),
          (o.defaultDecker, o.explicitDecker),
          o.explicitReadMode,
          (o.defaultWellDepth, o.explicitWellDepth),
          o.explicitFocusMotorSteps,
          (o.defaultTelescopeConfigs, o.explicitTelescopeConfigs),
          o.exposureTimeMode,
          (o.coadds, o.acquisition)
        )

  given Arbitrary[ObservingMode.GhostIfu.GhostDetector] =
    Arbitrary[ObservingMode.GhostIfu.GhostDetector](
      for {
        timeAndCount     <- arbitrary[ExposureTimeMode.TimeAndCountMode]
        defaultReadMode  <- arbitrary[GhostReadMode]
        explicitReadMode <- arbitrary[Option[GhostReadMode]]
        defaultBinning   <- arbitrary[GhostBinning]
        explicitBinning  <- arbitrary[Option[GhostBinning]]
      } yield ObservingMode.GhostIfu.GhostDetector(
        timeAndCount,
        defaultBinning,
        explicitBinning,
        defaultReadMode,
        explicitReadMode
      )
    )

  given Cogen[ObservingMode.GhostIfu.GhostDetector] =
    Cogen[
      (ExposureTimeMode.TimeAndCountMode,
       GhostBinning,
       Option[GhostBinning],
       GhostReadMode,
       Option[GhostReadMode]
      )
    ]
      .contramap(o =>
        (o.timeAndCount, o.defaultBinning, o.explicitBinning, o.defaultReadMode, o.explicitReadMode)
      )

  given Arbitrary[ObservingMode.GhostIfu] = Arbitrary[ObservingMode.GhostIfu](
    for {
      resolutionMode <- arbitrary[GhostResolutionMode]
      snAt           <- arbitrary[Wavelength]
      stepCount      <- arbitrary[PosInt]
      red            <- arbitrary[ObservingMode.GhostIfu.GhostDetector]
      blue           <- arbitrary[ObservingMode.GhostIfu.GhostDetector]
      defaultIfu1    <- arbitrary[GhostIfu1FiberAgitator]
      explicitIfu1   <- arbitrary[Option[GhostIfu1FiberAgitator]]
      defaultIfu2    <- arbitrary[GhostIfu2FiberAgitator]
      explicitIfu2   <- arbitrary[Option[GhostIfu2FiberAgitator]]
    } yield ObservingMode.GhostIfu(
      resolutionMode,
      snAt,
      stepCount,
      red,
      blue,
      defaultIfu1,
      explicitIfu1,
      defaultIfu2,
      explicitIfu2
    )
  )

  given Cogen[ObservingMode.GhostIfu] =
    Cogen[
      (
        GhostResolutionMode,
        PosInt,
        ObservingMode.GhostIfu.GhostDetector,
        ObservingMode.GhostIfu.GhostDetector,
        GhostIfu1FiberAgitator,
        Option[GhostIfu1FiberAgitator],
        GhostIfu2FiberAgitator,
        Option[GhostIfu2FiberAgitator]
      )
    ]
      .contramap(o =>
        (
          o.resolutionMode,
          o.stepCount,
          o.red,
          o.blue,
          o.defaultIfu1Agitator,
          o.explicitIfu1Agitator,
          o.defaultIfu2Agitator,
          o.explicitIfu2Agitator
        )
      )

  given Arbitrary[ObservingMode.Visitor] = Arbitrary[ObservingMode.Visitor](
    for {
      mode <- arbitrary[VisitorObservingModeType]
      cw   <- arbitrary[Wavelength]
      gsms <- arbitrary[lucuma.core.math.Angle]
      name <- arbitrary[Option[NonEmptyString]]
      trt  <- arbitrary[Option[TimeSpan]]
    } yield ObservingMode.Visitor(mode, CentralWavelength(cw), gsms, name, trt)
  )

  given Cogen[ObservingMode.Visitor] =
    Cogen[(VisitorObservingModeType, Wavelength, Long)]
      .contramap(o => (o.mode, o.centralWavelength.value, o.agsDiameter.toMicroarcseconds))

  given Arbitrary[ObservingMode] = Arbitrary[ObservingMode](
    Gen.oneOf(
      arbitrary[ObservingMode.GmosNorthLongSlit],
      arbitrary[ObservingMode.GmosSouthLongSlit],
      arbitrary[ObservingMode.GmosNorthImaging],
      arbitrary[ObservingMode.GmosSouthImaging],
      arbitrary[ObservingMode.Flamingos2LongSlit],
      arbitrary[ObservingMode.Flamingos2Imaging],
      arbitrary[ObservingMode.Igrins2LongSlit],
      arbitrary[ObservingMode.GnirsLongSlit],
      arbitrary[ObservingMode.GhostIfu],
      arbitrary[ObservingMode.Visitor]
    )
  )

  given Cogen[ObservingMode] =
    Cogen[Either[
      ObservingMode.GnirsLongSlit,
      Either[
        ObservingMode.Igrins2LongSlit,
        Either[
          ObservingMode.Flamingos2LongSlit,
          Either[
            ObservingMode.GmosNorthLongSlit,
            Either[
              ObservingMode.GmosSouthLongSlit,
              Either[
                ObservingMode.GmosNorthImaging,
                Either[ObservingMode.GmosSouthImaging,
                       Either[ObservingMode.GhostIfu, Either[ObservingMode.Flamingos2Imaging,
                                                             ObservingMode.Visitor
                       ]]
                ]
              ]
            ]
          ]
        ]
      ]
    ]]
      .contramap {
        case g: ObservingMode.GnirsLongSlit      => g.asLeft
        case i: ObservingMode.Igrins2LongSlit    => i.asLeft.asRight
        case f: ObservingMode.Flamingos2LongSlit => f.asLeft.asRight.asRight
        case n: ObservingMode.GmosNorthLongSlit  => n.asLeft.asRight.asRight.asRight
        case s: ObservingMode.GmosSouthLongSlit  => s.asLeft.asRight.asRight.asRight.asRight
        case n: ObservingMode.GmosNorthImaging   => n.asLeft.asRight.asRight.asRight.asRight.asRight
        case s: ObservingMode.GmosSouthImaging   =>
          s.asLeft.asRight.asRight.asRight.asRight.asRight.asRight
        case g: ObservingMode.GhostIfu           =>
          g.asLeft.asRight.asRight.asRight.asRight.asRight.asRight.asRight
        case f: ObservingMode.Flamingos2Imaging  =>
          f.asLeft.asRight.asRight.asRight.asRight.asRight.asRight.asRight.asRight
        case v: ObservingMode.Visitor            =>
          v.asRight.asRight.asRight.asRight.asRight.asRight.asRight.asRight.asRight
      }

}

object ArbObservingMode extends ArbObservingMode

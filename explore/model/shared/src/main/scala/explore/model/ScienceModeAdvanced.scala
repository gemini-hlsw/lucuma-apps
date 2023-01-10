// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import io.circe.Decoder
import io.circe.refined.*
import lucuma.core.enums.*
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.schemas.decoders.*
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism

sealed trait ScienceModeAdvanced extends Product with Serializable derives Eq

object ScienceModeAdvanced {
  case class GmosNorthLongSlit(
    overrideWavelength:        Option[Wavelength],
    overrideGrating:           Option[GmosNorthGrating],
    overrideFilter:            Option[GmosNorthFilter],
    overrideFpu:               Option[GmosNorthFpu],
    overrideExposureTimeMode:  Option[ExposureTimeMode],
    explicitXBin:              Option[GmosXBinning],
    explicitYBin:              Option[GmosYBinning],
    explicitAmpReadMode:       Option[GmosAmpReadMode],
    explicitAmpGain:           Option[GmosAmpGain],
    explicitRoi:               Option[GmosRoi],
    explicitWavelengthDithers: Option[NonEmptyList[DitherNanoMeters]],
    explicitSpatialOffsets:    Option[NonEmptyList[Offset.Q]]
  ) extends ScienceModeAdvanced
      derives Eq

  object GmosNorthLongSlit {
    lazy val Empty: GmosNorthLongSlit =
      GmosNorthLongSlit(none, none, none, none, none, none, none, none, none, none, none, none)

    implicit val gmosNLongSlitDecoder: Decoder[GmosNorthLongSlit] =
      Decoder.instance(c =>
        for {
          overrideWavelength        <- c.downField("overrideWavelength").as[Option[Wavelength]]
          overrideGrating           <- c.downField("overrideGrating").as[Option[GmosNorthGrating]]
          overrideFilter            <- c.downField("overrideFilter").as[Option[GmosNorthFilter]]
          overrideFpu               <- c.downField("overrideFpu").as[Option[GmosNorthFpu]]
          overrideExposureTimeMode  <-
            c.downField("overrideExposureTimeMode").as[Option[ExposureTimeMode]]
          explicitXBin              <- c.downField("explicitXBin").as[Option[GmosXBinning]]
          explicitYBin              <- c.downField("explicitYBin").as[Option[GmosYBinning]]
          explicitAmpReadMode       <- c.downField("explicitAmpReadMode").as[Option[GmosAmpReadMode]]
          explicitAmpGain           <- c.downField("explicitAmpGain").as[Option[GmosAmpGain]]
          explicitRoi               <- c.downField("explicitRoi").as[Option[GmosRoi]]
          explicitWavelengthDithers <-
            c.downField("explicitWavelengthDithersNm").as[Option[NonEmptyList[DitherNanoMeters]]]
          explicitSpatialOffsets    <-
            c.downField("explicitSpatialOffsets").as[Option[NonEmptyList[Offset.Q]]]
        } yield GmosNorthLongSlit(
          overrideWavelength,
          overrideGrating,
          overrideFilter,
          overrideFpu,
          overrideExposureTimeMode,
          explicitXBin,
          explicitYBin,
          explicitAmpReadMode,
          explicitAmpGain,
          explicitRoi,
          explicitWavelengthDithers,
          explicitSpatialOffsets
        )
      )

    val overrideWavelength: Lens[GmosNorthLongSlit, Option[Wavelength]] =
      Focus[GmosNorthLongSlit](_.overrideWavelength)

    val overrideGrating: Lens[GmosNorthLongSlit, Option[GmosNorthGrating]] =
      Focus[GmosNorthLongSlit](_.overrideGrating)

    val overrideFilter: Lens[GmosNorthLongSlit, Option[GmosNorthFilter]] =
      Focus[GmosNorthLongSlit](_.overrideFilter)

    val overrideFpu: Lens[GmosNorthLongSlit, Option[GmosNorthFpu]] =
      Focus[GmosNorthLongSlit](_.overrideFpu)

    val overrideExposureTimeMode: Lens[GmosNorthLongSlit, Option[ExposureTimeMode]] =
      Focus[GmosNorthLongSlit](_.overrideExposureTimeMode)

    val explicitXBin: Lens[GmosNorthLongSlit, Option[GmosXBinning]] =
      Focus[GmosNorthLongSlit](_.explicitXBin)

    val explicitYBin: Lens[GmosNorthLongSlit, Option[GmosYBinning]] =
      Focus[GmosNorthLongSlit](_.explicitYBin)

    val explicitAmpReadMode: Lens[GmosNorthLongSlit, Option[GmosAmpReadMode]] =
      Focus[GmosNorthLongSlit](_.explicitAmpReadMode)

    val explicitAmpGain: Lens[GmosNorthLongSlit, Option[GmosAmpGain]] =
      Focus[GmosNorthLongSlit](_.explicitAmpGain)

    val explicitRoi: Lens[GmosNorthLongSlit, Option[GmosRoi]] =
      Focus[GmosNorthLongSlit](_.explicitRoi)

    val explicitWavelengthDithers: Lens[GmosNorthLongSlit, Option[NonEmptyList[DitherNanoMeters]]] =
      Focus[GmosNorthLongSlit](_.explicitWavelengthDithers)

    val explicitSpatialOffsets: Lens[GmosNorthLongSlit, Option[NonEmptyList[Offset.Q]]] =
      Focus[GmosNorthLongSlit](_.explicitSpatialOffsets)
  }

  case class GmosSouthLongSlit(
    overrideWavelength:        Option[Wavelength],
    overrideGrating:           Option[GmosSouthGrating],
    overrideFilter:            Option[GmosSouthFilter],
    overrideFpu:               Option[GmosSouthFpu],
    overrideExposureTimeMode:  Option[ExposureTimeMode],
    explicitXBin:              Option[GmosXBinning],
    explicitYBin:              Option[GmosYBinning],
    explicitAmpReadMode:       Option[GmosAmpReadMode],
    explicitAmpGain:           Option[GmosAmpGain],
    explicitRoi:               Option[GmosRoi],
    explicitWavelengthDithers: Option[NonEmptyList[DitherNanoMeters]],
    explicitSpatialOffsets:    Option[NonEmptyList[Offset.Q]]
  ) extends ScienceModeAdvanced
      derives Eq

  object GmosSouthLongSlit {
    lazy val Empty: GmosSouthLongSlit =
      GmosSouthLongSlit(none, none, none, none, none, none, none, none, none, none, none, none)

    implicit val gmosSLongSlitDecoder: Decoder[GmosSouthLongSlit] =
      Decoder.instance(c =>
        for {
          overrideWavelength        <- c.downField("overrideWavelength").as[Option[Wavelength]]
          overrideGrating           <- c.downField("overrideGrating").as[Option[GmosSouthGrating]]
          overrideFilter            <- c.downField("overrideFilter").as[Option[GmosSouthFilter]]
          overrideFpu               <- c.downField("overrideFpu").as[Option[GmosSouthFpu]]
          overrideExposureTimeMode  <-
            c.downField("overrideExposureTimeMode").as[Option[ExposureTimeMode]]
          explicitXBin              <- c.downField("explicitXBin").as[Option[GmosXBinning]]
          explicitYBin              <- c.downField("explicitYBin").as[Option[GmosYBinning]]
          explicitAmpReadMode       <- c.downField("explicitAmpReadMode").as[Option[GmosAmpReadMode]]
          explicitAmpGain           <- c.downField("explicitAmpGain").as[Option[GmosAmpGain]]
          explicitRoi               <- c.downField("explicitRoi").as[Option[GmosRoi]]
          explicitWavelengthDithers <-
            c.downField("explicitWavelengthDithersNm").as[Option[NonEmptyList[DitherNanoMeters]]]
          explicitSpatialOffsets    <-
            c.downField("explicitSpatialOffsets").as[Option[NonEmptyList[Offset.Q]]]
        } yield GmosSouthLongSlit(
          overrideWavelength,
          overrideGrating,
          overrideFilter,
          overrideFpu,
          overrideExposureTimeMode,
          explicitXBin,
          explicitYBin,
          explicitAmpReadMode,
          explicitAmpGain,
          explicitRoi,
          explicitWavelengthDithers,
          explicitSpatialOffsets
        )
      )

    val overrideWavelength: Lens[GmosSouthLongSlit, Option[Wavelength]] =
      Focus[GmosSouthLongSlit](_.overrideWavelength)

    val overrideGrating: Lens[GmosSouthLongSlit, Option[GmosSouthGrating]] =
      Focus[GmosSouthLongSlit](_.overrideGrating)

    val overrideFilter: Lens[GmosSouthLongSlit, Option[GmosSouthFilter]] =
      Focus[GmosSouthLongSlit](_.overrideFilter)

    val overrideFpu: Lens[GmosSouthLongSlit, Option[GmosSouthFpu]] =
      Focus[GmosSouthLongSlit](_.overrideFpu)

    val overrideExposureTimeMode: Lens[GmosSouthLongSlit, Option[ExposureTimeMode]] =
      Focus[GmosSouthLongSlit](_.overrideExposureTimeMode)

    val explicitXBin: Lens[GmosSouthLongSlit, Option[GmosXBinning]] =
      Focus[GmosSouthLongSlit](_.explicitXBin)

    val explicitYBin: Lens[GmosSouthLongSlit, Option[GmosYBinning]] =
      Focus[GmosSouthLongSlit](_.explicitYBin)

    val explicitAmpReadMode: Lens[GmosSouthLongSlit, Option[GmosAmpReadMode]] =
      Focus[GmosSouthLongSlit](_.explicitAmpReadMode)

    val explicitAmpGain: Lens[GmosSouthLongSlit, Option[GmosAmpGain]] =
      Focus[GmosSouthLongSlit](_.explicitAmpGain)

    val explicitRoi: Lens[GmosSouthLongSlit, Option[GmosRoi]] =
      Focus[GmosSouthLongSlit](_.explicitRoi)

    val explicitWavelengthDithers: Lens[GmosSouthLongSlit, Option[NonEmptyList[DitherNanoMeters]]] =
      Focus[GmosSouthLongSlit](_.explicitWavelengthDithers)

    val explicitSpatialOffsets: Lens[GmosSouthLongSlit, Option[NonEmptyList[Offset.Q]]] =
      Focus[GmosSouthLongSlit](_.explicitSpatialOffsets)
  }

  val gmosNorthLongSlit: Prism[ScienceModeAdvanced, GmosNorthLongSlit] =
    GenPrism[ScienceModeAdvanced, GmosNorthLongSlit]

  val gmosSouthLongSlit: Prism[ScienceModeAdvanced, GmosSouthLongSlit] =
    GenPrism[ScienceModeAdvanced, GmosSouthLongSlit]

}

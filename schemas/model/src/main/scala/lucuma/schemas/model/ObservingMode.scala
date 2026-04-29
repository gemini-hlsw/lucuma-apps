// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.generic.semiauto.*
import io.circe.refined.given
import lucuma.core.enums.*
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDither
import lucuma.core.model.ExposureTimeMode
import lucuma.itc.ItcGhostDetector
import lucuma.odb.json.offset.decoder.given
import lucuma.odb.json.wavelength
import lucuma.odb.json.wavelength.decoder.given
import lucuma.schemas.decoders.given
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism

sealed abstract class ObservingMode(val instrument: Instrument) extends Product with Serializable
    derives Eq {
  def isCustomized: Boolean

  def obsModeType: ObservingModeType = this match
    case _: ObservingMode.GmosNorthLongSlit  => ObservingModeType.GmosNorthLongSlit
    case _: ObservingMode.GmosSouthLongSlit  => ObservingModeType.GmosSouthLongSlit
    case _: ObservingMode.GmosNorthImaging   => ObservingModeType.GmosNorthImaging
    case _: ObservingMode.GmosSouthImaging   => ObservingModeType.GmosSouthImaging
    case _: ObservingMode.Flamingos2LongSlit => ObservingModeType.Flamingos2LongSlit
    case _: ObservingMode.Igrins2LongSlit    => ObservingModeType.Igrins2LongSlit
    case _: ObservingMode.GhostIfu           => ObservingModeType.GhostIfu

  def gmosFpuAlternative: Option[Either[GmosNorthFpu, GmosSouthFpu]] = this match
    case o: ObservingMode.GmosNorthLongSlit => o.fpu.asLeft.some
    case o: ObservingMode.GmosSouthLongSlit => o.fpu.asRight.some
    case _                                  => none

  def siteFor: Site = this match
    case _: ObservingMode.GmosNorthLongSlit  => Site.GN
    case _: ObservingMode.GmosSouthLongSlit  => Site.GS
    case _: ObservingMode.GmosNorthImaging   => Site.GN
    case _: ObservingMode.GmosSouthImaging   => Site.GS
    case _: ObservingMode.Flamingos2LongSlit => Site.GS
    case _: ObservingMode.Igrins2LongSlit    => Site.GN
    case _: ObservingMode.GhostIfu           => Site.GS

  def toBasicConfiguration: BasicConfiguration = this match
    case n: ObservingMode.GmosNorthLongSlit                =>
      BasicConfiguration.GmosNorthLongSlit(n.grating, n.filter, n.fpu, n.centralWavelength)
    case s: ObservingMode.GmosSouthLongSlit                =>
      BasicConfiguration.GmosSouthLongSlit(s.grating, s.filter, s.fpu, s.centralWavelength)
    case ObservingMode.GmosNorthImaging(filters = filters) =>
      BasicConfiguration.GmosNorthImaging(filters.map(_.filter))
    case ObservingMode.GmosSouthImaging(filters = filters) =>
      BasicConfiguration.GmosSouthImaging(filters.map(_.filter))
    case f: ObservingMode.Flamingos2LongSlit               =>
      BasicConfiguration.Flamingos2LongSlit(f.disperser, f.filter, f.fpu)
    case _: ObservingMode.Igrins2LongSlit                  =>
      BasicConfiguration.Igrins2LongSlit
    case g: ObservingMode.GhostIfu                         =>
      val red  = ItcGhostDetector(
        timeAndCount = g.red.timeAndCount,
        binning = g.red.binning,
        readMode = g.red.readMode
      )
      val blue = ItcGhostDetector(
        timeAndCount = g.blue.timeAndCount,
        binning = g.blue.binning,
        readMode = g.blue.readMode
      )
      BasicConfiguration.GhostIfu(g.resolutionMode, g.signalToNoiseAt, red = red, blue = blue)

  def agsWavelength: AGSWavelength = toBasicConfiguration.agsWavelength

  def conditionsWavelength: Wavelength = toBasicConfiguration.conditionsWavelength

  def centralWv: Option[CentralWavelength] = toBasicConfiguration.centralWv

}

object ObservingMode:
  given Decoder[WavelengthDither] =
    Decoder.instance:
      _.downField("picometers").as[Int].map(WavelengthDither.intPicometers.get)

  given Decoder[ObservingMode] =
    Decoder
      .instance: c =>
        c.downField("gmosNorthLongSlit")
          .as[GmosNorthLongSlit]
          .orElse:
            c.downField("gmosSouthLongSlit").as[GmosSouthLongSlit]
          .orElse:
            c.downField("gmosNorthImaging").as[GmosNorthImaging]
          .orElse:
            c.downField("gmosSouthImaging").as[GmosSouthImaging]
          .orElse:
            c.downField("flamingos2LongSlit").as[Flamingos2LongSlit]
          .orElse:
            c.downField("igrins2LongSlit").as[Igrins2LongSlit]
          .orElse:
            c.downField("ghostIfu").as[GhostIfu]
          .orElse:
            DecodingFailure("Could not decode ObservingMode", c.history).asLeft

  case class GmosNorthLongSlit(
    initialGrating:            GmosNorthGrating,
    grating:                   GmosNorthGrating,
    initialFilter:             Option[GmosNorthFilter],
    filter:                    Option[GmosNorthFilter],
    initialFpu:                GmosNorthFpu,
    fpu:                       GmosNorthFpu,
    initialCentralWavelength:  CentralWavelength,
    centralWavelength:         CentralWavelength,
    defaultXBin:               GmosXBinning,
    explicitXBin:              Option[GmosXBinning],
    defaultYBin:               GmosYBinning,
    explicitYBin:              Option[GmosYBinning],
    defaultAmpReadMode:        GmosAmpReadMode,
    explicitAmpReadMode:       Option[GmosAmpReadMode],
    defaultAmpGain:            GmosAmpGain,
    explicitAmpGain:           Option[GmosAmpGain],
    defaultRoi:                GmosRoi,
    explicitRoi:               Option[GmosRoi],
    defaultWavelengthDithers:  NonEmptyList[WavelengthDither],
    explicitWavelengthDithers: Option[NonEmptyList[WavelengthDither]],
    defaultOffsets:            NonEmptyList[Offset.Q],
    explicitOffsets:           Option[NonEmptyList[Offset.Q]],
    exposureTimeMode:          ExposureTimeMode,
    acquisition:               GmosNorthLongSlit.Acquisition
  ) extends ObservingMode(Instrument.GmosNorth) derives Eq:
    val xBin: GmosXBinning                                =
      explicitXBin.getOrElse(defaultXBin)
    val yBin: GmosYBinning                                =
      explicitYBin.getOrElse(defaultYBin)
    val ampReadMode: GmosAmpReadMode                      =
      explicitAmpReadMode.getOrElse(defaultAmpReadMode)
    val ampGain: GmosAmpGain                              =
      explicitAmpGain.getOrElse(defaultAmpGain)
    val roi: GmosRoi                                      =
      explicitRoi.getOrElse(defaultRoi)
    val wavelengthDithers: NonEmptyList[WavelengthDither] =
      explicitWavelengthDithers.getOrElse(defaultWavelengthDithers)
    val spatialOffsets: NonEmptyList[Offset.Q]            =
      explicitOffsets.getOrElse(defaultOffsets)

    def isCustomized: Boolean =
      initialGrating =!= grating ||
        initialFilter =!= filter ||
        initialFpu =!= fpu ||
        initialCentralWavelength =!= centralWavelength ||
        explicitXBin.exists(_ =!= defaultXBin) ||
        explicitYBin.exists(_ =!= defaultYBin) ||
        explicitAmpReadMode.exists(_ =!= defaultAmpReadMode) ||
        explicitAmpGain.exists(_ =!= defaultAmpGain) ||
        explicitRoi.exists(_ =!= defaultRoi) ||
        explicitWavelengthDithers.exists(_ =!= defaultWavelengthDithers) ||
        explicitOffsets.exists(_ =!= defaultOffsets) ||
        acquisition.isCustomized

    def revertCustomizations: GmosNorthLongSlit =
      this.copy(
        grating = this.initialGrating,
        filter = this.initialFilter,
        fpu = this.initialFpu,
        centralWavelength = this.initialCentralWavelength,
        explicitXBin = None,
        explicitYBin = None,
        explicitAmpReadMode = None,
        explicitAmpGain = None,
        explicitRoi = None,
        explicitWavelengthDithers = None,
        explicitOffsets = None,
        acquisition = acquisition.revertCustomizations
      )

  object GmosNorthLongSlit:
    case class Acquisition(
      defaultFilter:    GmosNorthFilter,
      explicitFilter:   Option[GmosNorthFilter],
      defaultRoi:       GmosLongSlitAcquisitionRoi,
      explicitRoi:      Option[GmosLongSlitAcquisitionRoi],
      exposureTimeMode: ExposureTimeMode
    ) derives Decoder,
          Eq:
      val filter                            = explicitFilter.getOrElse(defaultFilter)
      val roi                               = explicitRoi.getOrElse(defaultRoi)
      def isCustomized: Boolean             =
        explicitFilter.exists(_ =!= defaultFilter) ||
          explicitRoi.exists(_ =!= defaultRoi)
      def revertCustomizations: Acquisition =
        this.copy(explicitFilter = None, explicitRoi = None)

    object Acquisition:
      val defaultFilter: Lens[Acquisition, GmosNorthFilter]                  =
        Focus[Acquisition](_.defaultFilter)
      val explicitFilter: Lens[Acquisition, Option[GmosNorthFilter]]         =
        Focus[Acquisition](_.explicitFilter)
      val defaultRoi: Lens[Acquisition, GmosLongSlitAcquisitionRoi]          =
        Focus[Acquisition](_.defaultRoi)
      val explicitRoi: Lens[Acquisition, Option[GmosLongSlitAcquisitionRoi]] =
        Focus[Acquisition](_.explicitRoi)
      val exposureTimeMode: Lens[Acquisition, ExposureTimeMode]              =
        Focus[Acquisition](_.exposureTimeMode)

    given Decoder[GmosNorthLongSlit] = deriveDecoder

    val initialGrating: Lens[GmosNorthLongSlit, GmosNorthGrating]                                  =
      Focus[GmosNorthLongSlit](_.initialGrating)
    val grating: Lens[GmosNorthLongSlit, GmosNorthGrating]                                         =
      Focus[GmosNorthLongSlit](_.grating)
    val initialFilter: Lens[GmosNorthLongSlit, Option[GmosNorthFilter]]                            =
      Focus[GmosNorthLongSlit](_.initialFilter)
    val filter: Lens[GmosNorthLongSlit, Option[GmosNorthFilter]]                                   =
      Focus[GmosNorthLongSlit](_.filter)
    val initialFpu: Lens[GmosNorthLongSlit, GmosNorthFpu]                                          =
      Focus[GmosNorthLongSlit](_.initialFpu)
    val fpu: Lens[GmosNorthLongSlit, GmosNorthFpu]                                                 =
      Focus[GmosNorthLongSlit](_.fpu)
    val initialCentralWavelength: Lens[GmosNorthLongSlit, CentralWavelength]                       =
      Focus[GmosNorthLongSlit](_.initialCentralWavelength)
    val centralWavelength: Lens[GmosNorthLongSlit, CentralWavelength]                              =
      Focus[GmosNorthLongSlit](_.centralWavelength)
    val defaultXBin: Lens[GmosNorthLongSlit, GmosXBinning]                                         =
      Focus[GmosNorthLongSlit](_.defaultXBin)
    val explicitXBin: Lens[GmosNorthLongSlit, Option[GmosXBinning]]                                =
      Focus[GmosNorthLongSlit](_.explicitXBin)
    val defaultYBin: Lens[GmosNorthLongSlit, GmosYBinning]                                         =
      Focus[GmosNorthLongSlit](_.defaultYBin)
    val explicitYBin: Lens[GmosNorthLongSlit, Option[GmosYBinning]]                                =
      Focus[GmosNorthLongSlit](_.explicitYBin)
    val defaultAmpReadMode: Lens[GmosNorthLongSlit, GmosAmpReadMode]                               =
      Focus[GmosNorthLongSlit](_.defaultAmpReadMode)
    val explicitAmpReadMode: Lens[GmosNorthLongSlit, Option[GmosAmpReadMode]]                      =
      Focus[GmosNorthLongSlit](_.explicitAmpReadMode)
    val defaultAmpGain: Lens[GmosNorthLongSlit, GmosAmpGain]                                       =
      Focus[GmosNorthLongSlit](_.defaultAmpGain)
    val explicitAmpGain: Lens[GmosNorthLongSlit, Option[GmosAmpGain]]                              =
      Focus[GmosNorthLongSlit](_.explicitAmpGain)
    val defaultRoi: Lens[GmosNorthLongSlit, GmosRoi]                                               =
      Focus[GmosNorthLongSlit](_.defaultRoi)
    val explicitRoi: Lens[GmosNorthLongSlit, Option[GmosRoi]]                                      =
      Focus[GmosNorthLongSlit](_.explicitRoi)
    val defaultWavelengthDithers: Lens[GmosNorthLongSlit, NonEmptyList[WavelengthDither]]          =
      Focus[GmosNorthLongSlit](_.defaultWavelengthDithers)
    val explicitWavelengthDithers: Lens[GmosNorthLongSlit, Option[NonEmptyList[WavelengthDither]]] =
      Focus[GmosNorthLongSlit](_.explicitWavelengthDithers)
    val defaultOffsets: Lens[GmosNorthLongSlit, NonEmptyList[Offset.Q]]                            =
      Focus[GmosNorthLongSlit](_.defaultOffsets)
    val explicitOffsets: Lens[GmosNorthLongSlit, Option[NonEmptyList[Offset.Q]]]                   =
      Focus[GmosNorthLongSlit](_.explicitOffsets)
    val exposureTimeMode: Lens[GmosNorthLongSlit, ExposureTimeMode]                                =
      Focus[GmosNorthLongSlit](_.exposureTimeMode)
    val acquisition: Lens[GmosNorthLongSlit, GmosNorthLongSlit.Acquisition]                        =
      Focus[GmosNorthLongSlit](_.acquisition)

  case class GmosSouthLongSlit(
    initialGrating:            GmosSouthGrating,
    grating:                   GmosSouthGrating,
    initialFilter:             Option[GmosSouthFilter],
    filter:                    Option[GmosSouthFilter],
    initialFpu:                GmosSouthFpu,
    fpu:                       GmosSouthFpu,
    initialCentralWavelength:  CentralWavelength,
    centralWavelength:         CentralWavelength,
    defaultXBin:               GmosXBinning,
    explicitXBin:              Option[GmosXBinning],
    defaultYBin:               GmosYBinning,
    explicitYBin:              Option[GmosYBinning],
    defaultAmpReadMode:        GmosAmpReadMode,
    explicitAmpReadMode:       Option[GmosAmpReadMode],
    defaultAmpGain:            GmosAmpGain,
    explicitAmpGain:           Option[GmosAmpGain],
    defaultRoi:                GmosRoi,
    explicitRoi:               Option[GmosRoi],
    defaultWavelengthDithers:  NonEmptyList[WavelengthDither],
    explicitWavelengthDithers: Option[NonEmptyList[WavelengthDither]],
    defaultOffsets:            NonEmptyList[Offset.Q],
    explicitOffsets:           Option[NonEmptyList[Offset.Q]],
    exposureTimeMode:          ExposureTimeMode,
    acquisition:               GmosSouthLongSlit.Acquisition
  ) extends ObservingMode(Instrument.GmosSouth) derives Eq:
    val xBin: GmosXBinning                                =
      explicitXBin.getOrElse(defaultXBin)
    val yBin: GmosYBinning                                =
      explicitYBin.getOrElse(defaultYBin)
    val ampReadMode: GmosAmpReadMode                      =
      explicitAmpReadMode.getOrElse(defaultAmpReadMode)
    val ampGain: GmosAmpGain                              =
      explicitAmpGain.getOrElse(defaultAmpGain)
    val roi: GmosRoi                                      =
      explicitRoi.getOrElse(defaultRoi)
    val wavelengthDithers: NonEmptyList[WavelengthDither] =
      explicitWavelengthDithers.getOrElse(defaultWavelengthDithers)
    val spatialOffsets: NonEmptyList[Offset.Q]            =
      explicitOffsets.getOrElse(defaultOffsets)

    def isCustomized: Boolean =
      initialGrating =!= grating ||
        initialFilter =!= filter ||
        initialFpu =!= fpu ||
        initialCentralWavelength =!= centralWavelength ||
        explicitXBin.exists(_ =!= defaultXBin) ||
        explicitYBin.exists(_ =!= defaultYBin) ||
        explicitAmpReadMode.exists(_ =!= defaultAmpReadMode) ||
        explicitAmpGain.exists(_ =!= defaultAmpGain) ||
        explicitRoi.exists(_ =!= defaultRoi) ||
        explicitWavelengthDithers.exists(_ =!= defaultWavelengthDithers) ||
        explicitOffsets.exists(_ =!= defaultOffsets) ||
        acquisition.isCustomized

    def revertCustomizations: GmosSouthLongSlit =
      this.copy(
        grating = this.initialGrating,
        filter = this.initialFilter,
        fpu = this.initialFpu,
        centralWavelength = this.initialCentralWavelength,
        explicitXBin = None,
        explicitYBin = None,
        explicitAmpReadMode = None,
        explicitAmpGain = None,
        explicitRoi = None,
        explicitWavelengthDithers = None,
        explicitOffsets = None,
        acquisition = acquisition.revertCustomizations
      )

  object GmosSouthLongSlit:
    case class Acquisition(
      defaultFilter:    GmosSouthFilter,
      explicitFilter:   Option[GmosSouthFilter],
      defaultRoi:       GmosLongSlitAcquisitionRoi,
      explicitRoi:      Option[GmosLongSlitAcquisitionRoi],
      exposureTimeMode: ExposureTimeMode
    ) derives Decoder,
          Eq:
      val filter                            = explicitFilter.getOrElse(defaultFilter)
      val roi                               = explicitRoi.getOrElse(defaultRoi)
      def isCustomized: Boolean             =
        explicitFilter.exists(_ =!= defaultFilter) ||
          explicitRoi.exists(_ =!= defaultRoi)
      def revertCustomizations: Acquisition =
        this.copy(explicitFilter = None, explicitRoi = None)

    object Acquisition:
      val defaultFilter: Lens[Acquisition, GmosSouthFilter]                  =
        Focus[Acquisition](_.defaultFilter)
      val explicitFilter: Lens[Acquisition, Option[GmosSouthFilter]]         =
        Focus[Acquisition](_.explicitFilter)
      val defaultRoi: Lens[Acquisition, GmosLongSlitAcquisitionRoi]          =
        Focus[Acquisition](_.defaultRoi)
      val explicitRoi: Lens[Acquisition, Option[GmosLongSlitAcquisitionRoi]] =
        Focus[Acquisition](_.explicitRoi)
      val exposureTimeMode: Lens[Acquisition, ExposureTimeMode]              =
        Focus[Acquisition](_.exposureTimeMode)

    given Decoder[GmosSouthLongSlit] = deriveDecoder

    val initialGrating: Lens[GmosSouthLongSlit, GmosSouthGrating]                                  =
      Focus[GmosSouthLongSlit](_.initialGrating)
    val grating: Lens[GmosSouthLongSlit, GmosSouthGrating]                                         =
      Focus[GmosSouthLongSlit](_.grating)
    val initialFilter: Lens[GmosSouthLongSlit, Option[GmosSouthFilter]]                            =
      Focus[GmosSouthLongSlit](_.initialFilter)
    val filter: Lens[GmosSouthLongSlit, Option[GmosSouthFilter]]                                   =
      Focus[GmosSouthLongSlit](_.filter)
    val initialFpu: Lens[GmosSouthLongSlit, GmosSouthFpu]                                          =
      Focus[GmosSouthLongSlit](_.initialFpu)
    val fpu: Lens[GmosSouthLongSlit, GmosSouthFpu]                                                 =
      Focus[GmosSouthLongSlit](_.fpu)
    val initialCentralWavelength: Lens[GmosSouthLongSlit, CentralWavelength]                       =
      Focus[GmosSouthLongSlit](_.initialCentralWavelength)
    val centralWavelength: Lens[GmosSouthLongSlit, CentralWavelength]                              =
      Focus[GmosSouthLongSlit](_.centralWavelength)
    val defaultXBin: Lens[GmosSouthLongSlit, GmosXBinning]                                         =
      Focus[GmosSouthLongSlit](_.defaultXBin)
    val explicitXBin: Lens[GmosSouthLongSlit, Option[GmosXBinning]]                                =
      Focus[GmosSouthLongSlit](_.explicitXBin)
    val defaultYBin: Lens[GmosSouthLongSlit, GmosYBinning]                                         =
      Focus[GmosSouthLongSlit](_.defaultYBin)
    val explicitYBin: Lens[GmosSouthLongSlit, Option[GmosYBinning]]                                =
      Focus[GmosSouthLongSlit](_.explicitYBin)
    val defaultAmpReadMode: Lens[GmosSouthLongSlit, GmosAmpReadMode]                               =
      Focus[GmosSouthLongSlit](_.defaultAmpReadMode)
    val explicitAmpReadMode: Lens[GmosSouthLongSlit, Option[GmosAmpReadMode]]                      =
      Focus[GmosSouthLongSlit](_.explicitAmpReadMode)
    val defaultAmpGain: Lens[GmosSouthLongSlit, GmosAmpGain]                                       =
      Focus[GmosSouthLongSlit](_.defaultAmpGain)
    val explicitAmpGain: Lens[GmosSouthLongSlit, Option[GmosAmpGain]]                              =
      Focus[GmosSouthLongSlit](_.explicitAmpGain)
    val defaultRoi: Lens[GmosSouthLongSlit, GmosRoi]                                               =
      Focus[GmosSouthLongSlit](_.defaultRoi)
    val explicitRoi: Lens[GmosSouthLongSlit, Option[GmosRoi]]                                      =
      Focus[GmosSouthLongSlit](_.explicitRoi)
    val defaultWavelengthDithers: Lens[GmosSouthLongSlit, NonEmptyList[WavelengthDither]]          =
      Focus[GmosSouthLongSlit](_.defaultWavelengthDithers)
    val explicitWavelengthDithers: Lens[GmosSouthLongSlit, Option[NonEmptyList[WavelengthDither]]] =
      Focus[GmosSouthLongSlit](_.explicitWavelengthDithers)
    val defaultOffsets: Lens[GmosSouthLongSlit, NonEmptyList[Offset.Q]]                            =
      Focus[GmosSouthLongSlit](_.defaultOffsets)
    val explicitOffsets: Lens[GmosSouthLongSlit, Option[NonEmptyList[Offset.Q]]]                   =
      Focus[GmosSouthLongSlit](_.explicitOffsets)
    val exposureTimeMode: Lens[GmosSouthLongSlit, ExposureTimeMode]                                =
      Focus[GmosSouthLongSlit](_.exposureTimeMode)
    val acquisition: Lens[GmosSouthLongSlit, GmosSouthLongSlit.Acquisition]                        =
      Focus[GmosSouthLongSlit](_.acquisition)

  case class GmosNorthImaging(
    variant:             GmosImagingVariant,
    initialFilters:      NonEmptyList[GmosNorthImaging.ImagingFilter],
    filters:             NonEmptyList[GmosNorthImaging.ImagingFilter],
    defaultBin:          GmosBinning,
    explicitBin:         Option[GmosBinning],
    defaultAmpReadMode:  GmosAmpReadMode,
    explicitAmpReadMode: Option[GmosAmpReadMode],
    defaultAmpGain:      GmosAmpGain,
    explicitAmpGain:     Option[GmosAmpGain],
    defaultRoi:          GmosRoi,
    explicitRoi:         Option[GmosRoi]
  ) extends ObservingMode(Instrument.GmosNorth) derives Eq:
    lazy val bin: GmosBinning             = explicitBin.getOrElse(defaultBin)
    lazy val ampReadMode: GmosAmpReadMode = explicitAmpReadMode.getOrElse(defaultAmpReadMode)
    lazy val ampGain: GmosAmpGain         = explicitAmpGain.getOrElse(defaultAmpGain)
    lazy val roi: GmosRoi                 = explicitRoi.getOrElse(defaultRoi)

    def isCustomized: Boolean =
      initialFilters =!= filters ||
        explicitBin.exists(_ =!= defaultBin) ||
        explicitAmpReadMode.exists(_ =!= defaultAmpReadMode) ||
        explicitAmpGain.exists(_ =!= defaultAmpGain) ||
        explicitRoi.exists(_ =!= defaultRoi)

    def revertCustomizations: GmosNorthImaging =
      this.copy(
        filters = this.initialFilters,
        explicitBin = None,
        explicitAmpReadMode = None,
        explicitAmpGain = None,
        explicitRoi = None
      )

  object GmosNorthImaging:
    case class ImagingFilter(filter: GmosNorthFilter, exposureTimeMode: ExposureTimeMode)
        derives Decoder,
          Eq

    object ImagingFilter:
      val filter: Lens[ImagingFilter, GmosNorthFilter]            = Focus[ImagingFilter](_.filter)
      val exposureTimeMode: Lens[ImagingFilter, ExposureTimeMode] =
        Focus[ImagingFilter](_.exposureTimeMode)

    given Decoder[GmosNorthImaging] = deriveDecoder

    val variant: Lens[GmosNorthImaging, GmosImagingVariant]                  =
      Focus[GmosNorthImaging](_.variant)
    val initialFilters: Lens[GmosNorthImaging, NonEmptyList[ImagingFilter]]  =
      Focus[GmosNorthImaging](_.initialFilters)
    val filters: Lens[GmosNorthImaging, NonEmptyList[ImagingFilter]]         =
      Focus[GmosNorthImaging](_.filters)
    val defaultBin: Lens[GmosNorthImaging, GmosBinning]                      =
      Focus[GmosNorthImaging](_.defaultBin)
    val explicitBin: Lens[GmosNorthImaging, Option[GmosBinning]]             =
      Focus[GmosNorthImaging](_.explicitBin)
    val defaultAmpReadMode: Lens[GmosNorthImaging, GmosAmpReadMode]          =
      Focus[GmosNorthImaging](_.defaultAmpReadMode)
    val explicitAmpReadMode: Lens[GmosNorthImaging, Option[GmosAmpReadMode]] =
      Focus[GmosNorthImaging](_.explicitAmpReadMode)
    val defaultAmpGain: Lens[GmosNorthImaging, GmosAmpGain]                  =
      Focus[GmosNorthImaging](_.defaultAmpGain)
    val explicitAmpGain: Lens[GmosNorthImaging, Option[GmosAmpGain]]         =
      Focus[GmosNorthImaging](_.explicitAmpGain)
    val defaultRoi: Lens[GmosNorthImaging, GmosRoi]                          =
      Focus[GmosNorthImaging](_.defaultRoi)
    val explicitRoi: Lens[GmosNorthImaging, Option[GmosRoi]]                 =
      Focus[GmosNorthImaging](_.explicitRoi)

  case class GmosSouthImaging(
    variant:             GmosImagingVariant,
    initialFilters:      NonEmptyList[GmosSouthImaging.ImagingFilter],
    filters:             NonEmptyList[GmosSouthImaging.ImagingFilter],
    defaultBin:          GmosBinning,
    explicitBin:         Option[GmosBinning],
    defaultAmpReadMode:  GmosAmpReadMode,
    explicitAmpReadMode: Option[GmosAmpReadMode],
    defaultAmpGain:      GmosAmpGain,
    explicitAmpGain:     Option[GmosAmpGain],
    defaultRoi:          GmosRoi,
    explicitRoi:         Option[GmosRoi]
  ) extends ObservingMode(Instrument.GmosSouth) derives Eq:
    lazy val bin: GmosBinning             = explicitBin.getOrElse(defaultBin)
    lazy val ampReadMode: GmosAmpReadMode = explicitAmpReadMode.getOrElse(defaultAmpReadMode)
    lazy val ampGain: GmosAmpGain         = explicitAmpGain.getOrElse(defaultAmpGain)
    lazy val roi: GmosRoi                 = explicitRoi.getOrElse(defaultRoi)

    def isCustomized: Boolean =
      initialFilters =!= filters ||
        explicitBin.exists(_ =!= defaultBin) ||
        explicitAmpReadMode.exists(_ =!= defaultAmpReadMode) ||
        explicitAmpGain.exists(_ =!= defaultAmpGain) ||
        explicitRoi.exists(_ =!= defaultRoi)

    def revertCustomizations: GmosSouthImaging =
      this.copy(
        filters = this.initialFilters,
        explicitBin = None,
        explicitAmpReadMode = None,
        explicitAmpGain = None,
        explicitRoi = None
      )

  object GmosSouthImaging:
    case class ImagingFilter(filter: GmosSouthFilter, exposureTimeMode: ExposureTimeMode)
        derives Decoder,
          Eq

    object ImagingFilter:
      val filter: Lens[ImagingFilter, GmosSouthFilter]            = Focus[ImagingFilter](_.filter)
      val exposureTimeMode: Lens[ImagingFilter, ExposureTimeMode] =
        Focus[ImagingFilter](_.exposureTimeMode)

    given Decoder[GmosSouthImaging] = deriveDecoder

    val variant: Lens[GmosSouthImaging, GmosImagingVariant]                  =
      Focus[GmosSouthImaging](_.variant)
    val initialFilters: Lens[GmosSouthImaging, NonEmptyList[ImagingFilter]]  =
      Focus[GmosSouthImaging](_.initialFilters)
    val filters: Lens[GmosSouthImaging, NonEmptyList[ImagingFilter]]         =
      Focus[GmosSouthImaging](_.filters)
    val defaultBin: Lens[GmosSouthImaging, GmosBinning]                      =
      Focus[GmosSouthImaging](_.defaultBin)
    val explicitBin: Lens[GmosSouthImaging, Option[GmosBinning]]             =
      Focus[GmosSouthImaging](_.explicitBin)
    val defaultAmpReadMode: Lens[GmosSouthImaging, GmosAmpReadMode]          =
      Focus[GmosSouthImaging](_.defaultAmpReadMode)
    val explicitAmpReadMode: Lens[GmosSouthImaging, Option[GmosAmpReadMode]] =
      Focus[GmosSouthImaging](_.explicitAmpReadMode)
    val defaultAmpGain: Lens[GmosSouthImaging, GmosAmpGain]                  =
      Focus[GmosSouthImaging](_.defaultAmpGain)
    val explicitAmpGain: Lens[GmosSouthImaging, Option[GmosAmpGain]]         =
      Focus[GmosSouthImaging](_.explicitAmpGain)
    val defaultRoi: Lens[GmosSouthImaging, GmosRoi]                          =
      Focus[GmosSouthImaging](_.defaultRoi)
    val explicitRoi: Lens[GmosSouthImaging, Option[GmosRoi]]                 =
      Focus[GmosSouthImaging](_.explicitRoi)

  case class Flamingos2LongSlit(
    initialDisperser:    Flamingos2Disperser,
    disperser:           Flamingos2Disperser,
    initialFilter:       Flamingos2Filter,
    filter:              Flamingos2Filter,
    initialFpu:          Flamingos2Fpu,
    fpu:                 Flamingos2Fpu,
    explicitReadMode:    Option[Flamingos2ReadMode],
    explicitReads:       Option[Flamingos2Reads],
    defaultDecker:       Flamingos2Decker,
    explicitDecker:      Option[Flamingos2Decker],
    defaultReadoutMode:  Flamingos2ReadoutMode,
    explicitReadoutMode: Option[Flamingos2ReadoutMode],
    defaultOffsets:      NonEmptyList[Offset],
    explicitOffsets:     Option[NonEmptyList[Offset]],
    exposureTimeMode:    ExposureTimeMode,
    acquisition:         Flamingos2LongSlit.Acquisition
  ) extends ObservingMode(Instrument.GmosSouth) derives Eq:
    val decker: Flamingos2Decker           =
      explicitDecker.getOrElse(defaultDecker)
    val readoutMode: Flamingos2ReadoutMode =
      explicitReadoutMode.getOrElse(defaultReadoutMode)
    val offsets: NonEmptyList[Offset]      =
      explicitOffsets.getOrElse(defaultOffsets)
    val readMode: Flamingos2ReadMode       =
      explicitReadMode.getOrElse(Flamingos2ReadMode.Bright)

    def isCustomized: Boolean =
      initialDisperser =!= disperser ||
        initialFilter =!= filter ||
        initialFpu =!= fpu ||
        explicitReadMode.isDefined ||
        explicitReads.isDefined ||
        explicitDecker.exists(_ =!= defaultDecker) ||
        explicitReadoutMode.exists(_ =!= defaultReadoutMode) ||
        explicitOffsets.exists(_ =!= defaultOffsets) ||
        acquisition.isCustomized

    def revertCustomizations: Flamingos2LongSlit =
      this.copy(
        disperser = this.initialDisperser,
        filter = this.initialFilter,
        fpu = this.initialFpu,
        explicitReadMode = None,
        explicitReads = None,
        explicitDecker = None,
        explicitReadoutMode = None,
        explicitOffsets = None,
        acquisition = acquisition.revertCustomizations
      )

  object Flamingos2LongSlit:
    case class Acquisition(
      defaultFilter:    Flamingos2Filter,
      explicitFilter:   Option[Flamingos2Filter],
      exposureTimeMode: ExposureTimeMode
    ) derives Decoder,
          Eq:
      def isCustomized: Boolean             =
        explicitFilter.exists(_ =!= defaultFilter)
      def revertCustomizations: Acquisition =
        this.copy(explicitFilter = None)

    object Acquisition:
      val defaultFilter: Lens[Acquisition, Flamingos2Filter]          =
        Focus[Acquisition](_.defaultFilter)
      val explicitFilter: Lens[Acquisition, Option[Flamingos2Filter]] =
        Focus[Acquisition](_.explicitFilter)
      val exposureTimeMode: Lens[Acquisition, ExposureTimeMode]       =
        Focus[Acquisition](_.exposureTimeMode)

    given Decoder[Flamingos2LongSlit] = deriveDecoder

    val initialDisperser: Lens[Flamingos2LongSlit, Flamingos2Disperser]              =
      Focus[Flamingos2LongSlit](_.initialDisperser)
    val disperser: Lens[Flamingos2LongSlit, Flamingos2Disperser]                     =
      Focus[Flamingos2LongSlit](_.disperser)
    val initialFilter: Lens[Flamingos2LongSlit, Flamingos2Filter]                    =
      Focus[Flamingos2LongSlit](_.initialFilter)
    val filter: Lens[Flamingos2LongSlit, Flamingos2Filter]                           =
      Focus[Flamingos2LongSlit](_.filter)
    val initialFpu: Lens[Flamingos2LongSlit, Flamingos2Fpu]                          =
      Focus[Flamingos2LongSlit](_.initialFpu)
    val fpu: Lens[Flamingos2LongSlit, Flamingos2Fpu]                                 =
      Focus[Flamingos2LongSlit](_.fpu)
    val explicitReadMode: Lens[Flamingos2LongSlit, Option[Flamingos2ReadMode]]       =
      Focus[Flamingos2LongSlit](_.explicitReadMode)
    val explicitReads: Lens[Flamingos2LongSlit, Option[Flamingos2Reads]]             =
      Focus[Flamingos2LongSlit](_.explicitReads)
    val defaultDecker: Lens[Flamingos2LongSlit, Flamingos2Decker]                    =
      Focus[Flamingos2LongSlit](_.defaultDecker)
    val explicitDecker: Lens[Flamingos2LongSlit, Option[Flamingos2Decker]]           =
      Focus[Flamingos2LongSlit](_.explicitDecker)
    val defaultReadoutMode: Lens[Flamingos2LongSlit, Flamingos2ReadoutMode]          =
      Focus[Flamingos2LongSlit](_.defaultReadoutMode)
    val explicitReadoutMode: Lens[Flamingos2LongSlit, Option[Flamingos2ReadoutMode]] =
      Focus[Flamingos2LongSlit](_.explicitReadoutMode)
    val defaultOffsets: Lens[Flamingos2LongSlit, NonEmptyList[Offset]]               =
      Focus[Flamingos2LongSlit](_.defaultOffsets)
    val explicitOffsets: Lens[Flamingos2LongSlit, Option[NonEmptyList[Offset]]]      =
      Focus[Flamingos2LongSlit](_.explicitOffsets)
    val exposureTimeMode: Lens[Flamingos2LongSlit, ExposureTimeMode]                 =
      Focus[Flamingos2LongSlit](_.exposureTimeMode)
    val acquisition: Lens[Flamingos2LongSlit, Flamingos2LongSlit.Acquisition]        =
      Focus[Flamingos2LongSlit](_.acquisition)

  case class Igrins2LongSlit(
    exposureTimeMode:      ExposureTimeMode,
    defaultOffsetMode:     Igrins2OffsetMode,
    explicitOffsetMode:    Option[Igrins2OffsetMode],
    defaultSaveSVCImages:  Boolean,
    explicitSaveSVCImages: Option[Boolean],
    defaultOffsets:        NonEmptyList[Offset],
    explicitOffsets:       Option[NonEmptyList[Offset]]
  ) extends ObservingMode(Instrument.Igrins2) derives Eq:
    val offsetMode: Igrins2OffsetMode = explicitOffsetMode.getOrElse(defaultOffsetMode)
    val saveSVCImages: Boolean        = explicitSaveSVCImages.getOrElse(defaultSaveSVCImages)
    val offsets: NonEmptyList[Offset] = explicitOffsets.getOrElse(defaultOffsets)

    def isCustomized: Boolean =
      explicitOffsetMode.exists(_ =!= defaultOffsetMode) ||
        explicitSaveSVCImages.exists(_ =!= defaultSaveSVCImages) ||
        explicitOffsets.exists(_ =!= defaultOffsets)

    def revertCustomizations: Igrins2LongSlit =
      this.copy(explicitOffsetMode = None, explicitSaveSVCImages = None, explicitOffsets = None)

  object Igrins2LongSlit:
    given Decoder[Igrins2LongSlit] = deriveDecoder

    val exposureTimeMode: Lens[Igrins2LongSlit, ExposureTimeMode]            =
      Focus[Igrins2LongSlit](_.exposureTimeMode)
    val defaultOffsetMode: Lens[Igrins2LongSlit, Igrins2OffsetMode]          =
      Focus[Igrins2LongSlit](_.defaultOffsetMode)
    val explicitOffsetMode: Lens[Igrins2LongSlit, Option[Igrins2OffsetMode]] =
      Focus[Igrins2LongSlit](_.explicitOffsetMode)
    val defaultSaveSVCImages: Lens[Igrins2LongSlit, Boolean]                 =
      Focus[Igrins2LongSlit](_.defaultSaveSVCImages)
    val explicitSaveSVCImages: Lens[Igrins2LongSlit, Option[Boolean]]        =
      Focus[Igrins2LongSlit](_.explicitSaveSVCImages)
    val defaultOffsets: Lens[Igrins2LongSlit, NonEmptyList[Offset]]          =
      Focus[Igrins2LongSlit](_.defaultOffsets)
    val explicitOffsets: Lens[Igrins2LongSlit, Option[NonEmptyList[Offset]]] =
      Focus[Igrins2LongSlit](_.explicitOffsets)

  case class GhostIfu(
    resolutionMode:       GhostResolutionMode,
    signalToNoiseAt:      Wavelength,
    stepCount:            PosInt,
    red:                  GhostIfu.GhostDetector,
    blue:                 GhostIfu.GhostDetector,
    defaultIfu1Agitator:  GhostIfu1FiberAgitator,
    explicitIfu1Agitator: Option[GhostIfu1FiberAgitator],
    defaultIfu2Agitator:  GhostIfu2FiberAgitator,
    explicitIfu2Agitator: Option[GhostIfu2FiberAgitator]
  ) extends ObservingMode(Instrument.Ghost) derives Eq:
    val ifu1Agitator: GhostIfu1FiberAgitator = explicitIfu1Agitator.getOrElse(defaultIfu1Agitator)
    val ifu2Agitator: GhostIfu2FiberAgitator = explicitIfu2Agitator.getOrElse(defaultIfu2Agitator)
    def isCustomized: Boolean                =
      stepCount.value =!= 1 ||
        explicitIfu1Agitator.exists(_ =!= defaultIfu1Agitator) ||
        explicitIfu2Agitator.exists(_ =!= defaultIfu2Agitator) ||
        red.isCustomized ||
        blue.isCustomized
    def revertCustomizations: GhostIfu       =
      this.copy(stepCount = PosInt.unsafeFrom(1),
                explicitIfu1Agitator = None,
                explicitIfu2Agitator = None,
                red = red.revertCustomizations,
                blue = blue.revertCustomizations
      )

  object GhostIfu:
    // TODO: When the ODB API has the signalToNoiseAt value, we can switch to deriving the decoder
    given Decoder[GhostIfu] = Decoder.instance: c =>
      for {
        resolutionMode       <- c.downField("resolutionMode").as[GhostResolutionMode]
        stepCount            <- c.downField("stepCount").as[PosInt]
        red                  <- c.downField("red").as[GhostIfu.GhostDetector]
        blue                 <- c.downField("blue").as[GhostIfu.GhostDetector]
        defaultIfu1Agitator  <- c.downField("defaultIfu1Agitator").as[GhostIfu1FiberAgitator]
        explicitIfu1Agitator <-
          c.downField("explicitIfu1Agitator").as[Option[GhostIfu1FiberAgitator]]
        defaultIfu2Agitator  <- c.downField("defaultIfu2Agitator").as[GhostIfu2FiberAgitator]
        explicitIfu2Agitator <-
          c.downField("explicitIfu2Agitator").as[Option[GhostIfu2FiberAgitator]]
      } yield GhostIfu(
        resolutionMode,
        red.timeAndCount.at, // Temporary: Not yet in the ODB API
        stepCount,
        red,
        blue,
        defaultIfu1Agitator,
        explicitIfu1Agitator,
        defaultIfu2Agitator,
        explicitIfu2Agitator
      )

    case class GhostDetector(
      timeAndCount:     ExposureTimeMode.TimeAndCountMode,
      defaultBinning:   GhostBinning,
      explicitBinning:  Option[GhostBinning],
      defaultReadMode:  GhostReadMode,
      explicitReadMode: Option[GhostReadMode]
    ) derives Eq:
      val binning: GhostBinning               = explicitBinning.getOrElse(defaultBinning)
      val readMode: GhostReadMode             = explicitReadMode.getOrElse(defaultReadMode)
      def isCustomized: Boolean               =
        explicitBinning.exists(_ =!= defaultBinning) ||
          explicitReadMode.exists(_ =!= defaultReadMode)
      def revertCustomizations: GhostDetector =
        this.copy(explicitBinning = None, explicitReadMode = None)

    object GhostDetector:
      val timeAndCount: Lens[GhostDetector, ExposureTimeMode.TimeAndCountMode] =
        Focus[GhostDetector](_.timeAndCount)
      val defaultBinning: Lens[GhostDetector, GhostBinning]                    =
        Focus[GhostDetector](_.defaultBinning)
      val explicitBinning: Lens[GhostDetector, Option[GhostBinning]]           =
        Focus[GhostDetector](_.explicitBinning)
      val defaultReadMode: Lens[GhostDetector, GhostReadMode]                  =
        Focus[GhostDetector](_.defaultReadMode)
      val explicitReadMode: Lens[GhostDetector, Option[GhostReadMode]]         =
        Focus[GhostDetector](_.explicitReadMode)

      given Decoder[GhostIfu.GhostDetector] = Decoder.instance: c =>
        for {
          timeAndCount     <-
            c.downField("exposureTimeMode")
              .as[ExposureTimeMode]
              .flatMap: etm =>
                ExposureTimeMode.timeAndCount
                  .getOption(etm)
                  .toRight(
                    DecodingFailure("Expected TimeAndCountMode for GHOST detector", c.history)
                  )
          defaultBinning   <- c.downField("defaultBinning").as[GhostBinning]
          explicitBinning  <- c.downField("explicitBinning").as[Option[GhostBinning]]
          defaultReadMode  <- c.downField("defaultReadMode").as[GhostReadMode]
          explicitReadMode <- c.downField("explicitReadMode").as[Option[GhostReadMode]]
        } yield GhostDetector(timeAndCount,
                              defaultBinning,
                              explicitBinning,
                              defaultReadMode,
                              explicitReadMode
        )

    val resolutionMode: Lens[GhostIfu, GhostResolutionMode] =
      Focus[GhostIfu](_.resolutionMode)

    val signalToNoiseAt: Lens[GhostIfu, Wavelength] =
      Focus[GhostIfu](_.signalToNoiseAt)

    val stepCount: Lens[GhostIfu, PosInt] =
      Focus[GhostIfu](_.stepCount)

    val red: Lens[GhostIfu, GhostIfu.GhostDetector]  = Focus[GhostIfu](_.red)
    val blue: Lens[GhostIfu, GhostIfu.GhostDetector] = Focus[GhostIfu](_.blue)

    val defaultIfu1Agitator: Lens[GhostIfu, GhostIfu1FiberAgitator]          =
      Focus[GhostIfu](_.defaultIfu1Agitator)
    val explicitIfu1Agitator: Lens[GhostIfu, Option[GhostIfu1FiberAgitator]] =
      Focus[GhostIfu](_.explicitIfu1Agitator)
    val defaultIfu2Agitator: Lens[GhostIfu, GhostIfu2FiberAgitator]          =
      Focus[GhostIfu](_.defaultIfu2Agitator)
    val explicitIfu2Agitator: Lens[GhostIfu, Option[GhostIfu2FiberAgitator]] =
      Focus[GhostIfu](_.explicitIfu2Agitator)

  val gmosNorthLongSlit: Prism[ObservingMode, GmosNorthLongSlit] =
    GenPrism[ObservingMode, GmosNorthLongSlit]

  val gmosSouthLongSlit: Prism[ObservingMode, GmosSouthLongSlit] =
    GenPrism[ObservingMode, GmosSouthLongSlit]

  val gmosNorthImaging: Prism[ObservingMode, GmosNorthImaging] =
    GenPrism[ObservingMode, GmosNorthImaging]

  val gmosSouthImaging: Prism[ObservingMode, GmosSouthImaging] =
    GenPrism[ObservingMode, GmosSouthImaging]

  val flamingos2LongSlit: Prism[ObservingMode, Flamingos2LongSlit] =
    GenPrism[ObservingMode, Flamingos2LongSlit]

  val igrins2LongSlit: Prism[ObservingMode, Igrins2LongSlit] =
    GenPrism[ObservingMode, Igrins2LongSlit]

  val ghostIfu: Prism[ObservingMode, GhostIfu] =
    GenPrism[ObservingMode, GhostIfu]

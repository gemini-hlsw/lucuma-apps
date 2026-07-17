// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.generic.semiauto.*
import io.circe.refined.given
import lucuma.core.enums.*
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDither
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMode
import lucuma.core.model.sequence.gnirs.GnirsFocusMotorStepsValue
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.core.model.sequence.gnirs.defaultIfuTelescopeConfigs
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.itc.ItcGhostDetector
import lucuma.odb.json.angle.decoder.given
import lucuma.odb.json.coordinates.query.given
import lucuma.odb.json.offset.decoder.given
import lucuma.odb.json.stepconfig.given
import lucuma.odb.json.time.decoder.given
import lucuma.odb.json.wavelength
import lucuma.odb.json.wavelength.decoder.given
import lucuma.schemas.decoders.given
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism

sealed abstract class ObservingMode(val instrument: Option[Instrument])
    extends Product
    with Serializable derives Eq {
  def isCustomized: Boolean

  def obsModeType: ObservingModeType = this match
    case _: ObservingMode.GmosNorthLongSlit  => ObservingModeType.GmosNorthLongSlit
    case _: ObservingMode.GmosSouthLongSlit  => ObservingModeType.GmosSouthLongSlit
    case _: ObservingMode.GmosNorthImaging   => ObservingModeType.GmosNorthImaging
    case _: ObservingMode.GmosSouthImaging   => ObservingModeType.GmosSouthImaging
    case _: ObservingMode.Flamingos2Imaging  => ObservingModeType.Flamingos2Imaging
    case _: ObservingMode.Flamingos2LongSlit => ObservingModeType.Flamingos2LongSlit
    case _: ObservingMode.Igrins2LongSlit    => ObservingModeType.Igrins2LongSlit
    case _: ObservingMode.GnirsImaging       => ObservingModeType.GnirsImaging
    case g: ObservingMode.GnirsSpectroscopy  =>
      g.fpu match
        case GnirsFpu.Spectroscopy.Slit(_) => ObservingModeType.GnirsLongSlit
        case GnirsFpu.Spectroscopy.Ifu(_)  => ObservingModeType.GnirsIfu
    case _: ObservingMode.GhostIfu           => ObservingModeType.GhostIfu
    case v: ObservingMode.Visitor            => v.mode
    case _: ObservingMode.KeckExchange       => ObservingModeType.ExchangeKeck
    case _: ObservingMode.SubaruExchange     => ObservingModeType.ExchangeSubaru

  def gmosFpuAlternative: Option[Either[GmosNorthFpu, GmosSouthFpu]] = this match
    case o: ObservingMode.GmosNorthLongSlit => o.fpu.asLeft.some
    case o: ObservingMode.GmosSouthLongSlit => o.fpu.asRight.some
    case _                                  => none

  def siteFor: Option[Site] = this match
    case _: ObservingMode.GmosNorthLongSlit  => Site.GN.some
    case _: ObservingMode.GmosSouthLongSlit  => Site.GS.some
    case _: ObservingMode.GmosNorthImaging   => Site.GN.some
    case _: ObservingMode.GmosSouthImaging   => Site.GS.some
    case _: ObservingMode.Flamingos2Imaging  => Site.GS.some
    case _: ObservingMode.Flamingos2LongSlit => Site.GS.some
    case _: ObservingMode.Igrins2LongSlit    => Site.GN.some
    case _: ObservingMode.GnirsImaging       => Site.GN.some
    case _: ObservingMode.GnirsSpectroscopy  => Site.GN.some
    case _: ObservingMode.GhostIfu           => Site.GS.some
    case v: ObservingMode.Visitor            => v.toBasicConfiguration.siteFor
    case _: ObservingMode.KeckExchange       => none
    case _: ObservingMode.SubaruExchange     => none

  def toBasicConfiguration: BasicConfiguration = this match
    case n: ObservingMode.GmosNorthLongSlit                        =>
      BasicConfiguration.GmosNorthLongSlit(n.grating, n.filter, n.fpu, n.centralWavelength)
    case s: ObservingMode.GmosSouthLongSlit                        =>
      BasicConfiguration.GmosSouthLongSlit(s.grating, s.filter, s.fpu, s.centralWavelength)
    case ObservingMode.GmosNorthImaging(filters = filters)         =>
      BasicConfiguration.GmosNorthImaging(filters.map(_.filter))
    case ObservingMode.GmosSouthImaging(filters = filters)         =>
      BasicConfiguration.GmosSouthImaging(filters.map(_.filter))
    case ObservingMode.Flamingos2Imaging(filters = filters)        =>
      BasicConfiguration.Flamingos2Imaging(filters.map(_.filter))
    case f: ObservingMode.Flamingos2LongSlit                       =>
      BasicConfiguration.Flamingos2LongSlit(f.disperser, f.filter, f.fpu)
    case _: ObservingMode.Igrins2LongSlit                          =>
      BasicConfiguration.Igrins2LongSlit
    case g: ObservingMode.GnirsImaging                             =>
      BasicConfiguration.GnirsImaging(g.filters.map(_.filter), g.camera)
    case g: ObservingMode.GnirsSpectroscopy                        =>
      BasicConfiguration
        .GnirsSpectroscopy(g.filter, g.fpu, g.prism, g.grating, g.camera, g.centralWavelength)
    case g: ObservingMode.GhostIfu                                 =>
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
      BasicConfiguration.GhostIfu(g.resolutionMode,
                                  g.stepCount,
                                  g.signalToNoiseAt,
                                  red = red,
                                  blue = blue
      )
    case v: ObservingMode.Visitor                                  =>
      BasicConfiguration.Visitor(v.mode, v.centralWavelength, v.agsDiameter, v.scienceFovDiameter)
    case ObservingMode.KeckExchange(keckInstrument, requested)     =>
      BasicConfiguration.KeckExchange(keckInstrument, requested)
    case ObservingMode.SubaruExchange(subaruInstrument, requested) =>
      BasicConfiguration.SubaruExchange(subaruInstrument, requested)

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
            c.downField("flamingos2Imaging").as[Flamingos2Imaging]
          .orElse:
            c.downField("flamingos2LongSlit").as[Flamingos2LongSlit]
          .orElse:
            c.downField("igrins2LongSlit").as[Igrins2LongSlit]
          .orElse:
            c.downField("gnirsImaging").as[GnirsImaging]
          .orElse:
            c.downField("gnirsSpectroscopy").as[GnirsSpectroscopy]
          .orElse:
            c.downField("ghostIfu").as[GhostIfu]
          .orElse:
            c.downField("visitor").as[Visitor]
          .orElse:
            c.downField("exchange").as[ObservingMode.KeckExchange]
          .orElse:
            c.downField("exchange").as[ObservingMode.SubaruExchange]
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
  ) extends ObservingMode(Instrument.GmosNorth.some) derives Eq:
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
  ) extends ObservingMode(Instrument.GmosSouth.some) derives Eq:
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
    variant:             ImagingVariant,
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
  ) extends ObservingMode(Instrument.GmosNorth.some) derives Eq:
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

    val variant: Lens[GmosNorthImaging, ImagingVariant]                      =
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
    variant:             ImagingVariant,
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
  ) extends ObservingMode(Instrument.GmosSouth.some) derives Eq:
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

    val variant: Lens[GmosSouthImaging, ImagingVariant]                      =
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
  ) extends ObservingMode(Instrument.GmosSouth.some) derives Eq:
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

  case class Flamingos2Imaging(
    initialFilters:      NonEmptyList[Flamingos2Imaging.ImagingFilter],
    filters:             NonEmptyList[Flamingos2Imaging.ImagingFilter],
    defaultReadMode:     Flamingos2ReadMode,
    explicitReadMode:    Option[Flamingos2ReadMode],
    defaultReads:        Flamingos2Reads,
    explicitReads:       Option[Flamingos2Reads],
    defaultDecker:       Flamingos2Decker,
    explicitDecker:      Option[Flamingos2Decker],
    defaultReadoutMode:  Flamingos2ReadoutMode,
    explicitReadoutMode: Option[Flamingos2ReadoutMode],
    variant:             ImagingVariant
  ) extends ObservingMode(Instrument.Flamingos2.some) derives Eq:
    val readMode: Flamingos2ReadMode       =
      explicitReadMode.getOrElse(defaultReadMode)
    val reads: Flamingos2Reads             =
      explicitReads.getOrElse(defaultReads)
    val decker: Flamingos2Decker           =
      explicitDecker.getOrElse(defaultDecker)
    val readoutMode: Flamingos2ReadoutMode =
      explicitReadoutMode.getOrElse(defaultReadoutMode)

    def isCustomized: Boolean =
      initialFilters =!= filters ||
        explicitReadMode.exists(_ =!= defaultReadMode) ||
        explicitReads.exists(_ =!= defaultReads) ||
        explicitDecker.exists(_ =!= defaultDecker) ||
        explicitReadoutMode.exists(_ =!= defaultReadoutMode)

    def revertCustomizations: Flamingos2Imaging =
      this.copy(
        filters = this.initialFilters,
        explicitReadMode = None,
        explicitReads = None,
        explicitDecker = None,
        explicitReadoutMode = None
      )

  object Flamingos2Imaging:
    case class ImagingFilter(filter: Flamingos2Filter, exposureTimeMode: ExposureTimeMode)
        derives Decoder,
          Eq

    object ImagingFilter:
      val filter: Lens[ImagingFilter, Flamingos2Filter]           = Focus[ImagingFilter](_.filter)
      val exposureTimeMode: Lens[ImagingFilter, ExposureTimeMode] =
        Focus[ImagingFilter](_.exposureTimeMode)

    given Decoder[Flamingos2Imaging] = deriveDecoder

    val initialFilters: Lens[Flamingos2Imaging, NonEmptyList[ImagingFilter]]        =
      Focus[Flamingos2Imaging](_.initialFilters)
    val filters: Lens[Flamingos2Imaging, NonEmptyList[ImagingFilter]]               =
      Focus[Flamingos2Imaging](_.filters)
    val defaultReadMode: Lens[Flamingos2Imaging, Flamingos2ReadMode]                =
      Focus[Flamingos2Imaging](_.defaultReadMode)
    val explicitReadMode: Lens[Flamingos2Imaging, Option[Flamingos2ReadMode]]       =
      Focus[Flamingos2Imaging](_.explicitReadMode)
    val defaultReads: Lens[Flamingos2Imaging, Flamingos2Reads]                      =
      Focus[Flamingos2Imaging](_.defaultReads)
    val explicitReads: Lens[Flamingos2Imaging, Option[Flamingos2Reads]]             =
      Focus[Flamingos2Imaging](_.explicitReads)
    val defaultDecker: Lens[Flamingos2Imaging, Flamingos2Decker]                    =
      Focus[Flamingos2Imaging](_.defaultDecker)
    val explicitDecker: Lens[Flamingos2Imaging, Option[Flamingos2Decker]]           =
      Focus[Flamingos2Imaging](_.explicitDecker)
    val defaultReadoutMode: Lens[Flamingos2Imaging, Flamingos2ReadoutMode]          =
      Focus[Flamingos2Imaging](_.defaultReadoutMode)
    val explicitReadoutMode: Lens[Flamingos2Imaging, Option[Flamingos2ReadoutMode]] =
      Focus[Flamingos2Imaging](_.explicitReadoutMode)
    val variant: Lens[Flamingos2Imaging, ImagingVariant]                            =
      Focus[Flamingos2Imaging](_.variant)

  case class Igrins2LongSlit(
    exposureTimeMode:      ExposureTimeMode,
    defaultOffsetMode:     SlitOffsetMode,
    explicitOffsetMode:    Option[SlitOffsetMode],
    defaultSaveSVCImages:  Boolean,
    explicitSaveSVCImages: Option[Boolean],
    defaultOffsets:        NonEmptyList[Offset],
    explicitOffsets:       Option[NonEmptyList[Offset]]
  ) extends ObservingMode(Instrument.Igrins2.some) derives Eq:
    val offsetMode: SlitOffsetMode    = explicitOffsetMode.getOrElse(defaultOffsetMode)
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
    val defaultOffsetMode: Lens[Igrins2LongSlit, SlitOffsetMode]             =
      Focus[Igrins2LongSlit](_.defaultOffsetMode)
    val explicitOffsetMode: Lens[Igrins2LongSlit, Option[SlitOffsetMode]]    =
      Focus[Igrins2LongSlit](_.explicitOffsetMode)
    val defaultSaveSVCImages: Lens[Igrins2LongSlit, Boolean]                 =
      Focus[Igrins2LongSlit](_.defaultSaveSVCImages)
    val explicitSaveSVCImages: Lens[Igrins2LongSlit, Option[Boolean]]        =
      Focus[Igrins2LongSlit](_.explicitSaveSVCImages)
    val defaultOffsets: Lens[Igrins2LongSlit, NonEmptyList[Offset]]          =
      Focus[Igrins2LongSlit](_.defaultOffsets)
    val explicitOffsets: Lens[Igrins2LongSlit, Option[NonEmptyList[Offset]]] =
      Focus[Igrins2LongSlit](_.explicitOffsets)

  case class GnirsImaging(
    initialFilters:    NonEmptyList[GnirsImaging.ImagingFilter],
    filters:           NonEmptyList[GnirsImaging.ImagingFilter],
    camera:            GnirsCamera,
    coadds:            PosInt,
    explicitReadMode:  Option[GnirsReadMode],
    defaultWellDepth:  GnirsWellDepth,
    explicitWellDepth: Option[GnirsWellDepth],
    variant:           ImagingVariant
  ) extends ObservingMode(Instrument.Gnirs.some) derives Eq:
    val wellDepth: GnirsWellDepth =
      explicitWellDepth.getOrElse(defaultWellDepth)

    def isCustomized: Boolean =
      initialFilters =!= filters ||
        explicitReadMode.isDefined ||
        explicitWellDepth.exists(_ =!= defaultWellDepth)

    def revertCustomizations: GnirsImaging =
      this.copy(
        filters = this.initialFilters,
        explicitReadMode = None,
        explicitWellDepth = None
      )

  object GnirsImaging:
    case class ImagingFilter(filter: GnirsFilter, exposureTimeMode: ExposureTimeMode)
        derives Decoder,
          Eq

    object ImagingFilter:
      val filter: Lens[ImagingFilter, GnirsFilter]                = Focus[ImagingFilter](_.filter)
      val exposureTimeMode: Lens[ImagingFilter, ExposureTimeMode] =
        Focus[ImagingFilter](_.exposureTimeMode)

    given Decoder[GnirsImaging] = deriveDecoder

    val initialFilters: Lens[GnirsImaging, NonEmptyList[ImagingFilter]] =
      Focus[GnirsImaging](_.initialFilters)
    val filters: Lens[GnirsImaging, NonEmptyList[ImagingFilter]]        =
      Focus[GnirsImaging](_.filters)
    val camera: Lens[GnirsImaging, GnirsCamera]                         =
      Focus[GnirsImaging](_.camera)
    val coadds: Lens[GnirsImaging, PosInt]                              =
      Focus[GnirsImaging](_.coadds)
    val explicitReadMode: Lens[GnirsImaging, Option[GnirsReadMode]]     =
      Focus[GnirsImaging](_.explicitReadMode)
    val defaultWellDepth: Lens[GnirsImaging, GnirsWellDepth]            =
      Focus[GnirsImaging](_.defaultWellDepth)
    val explicitWellDepth: Lens[GnirsImaging, Option[GnirsWellDepth]]   =
      Focus[GnirsImaging](_.explicitWellDepth)
    val variant: Lens[GnirsImaging, ImagingVariant]                     =
      Focus[GnirsImaging](_.variant)

  case class GnirsSpectroscopy(
    initialGrating:           GnirsGrating,
    grating:                  GnirsGrating,
    initialFilter:            GnirsFilter,
    filter:                   GnirsFilter,
    subMode:                  GnirsSpectroscopy.SubMode,
    initialPrism:             GnirsPrism,
    prism:                    GnirsPrism,
    initialCamera:            GnirsCamera,
    camera:                   GnirsCamera,
    initialCentralWavelength: CentralWavelength,
    centralWavelength:        CentralWavelength,
    defaultDecker:            GnirsDecker,
    explicitDecker:           Option[GnirsDecker],
    explicitReadMode:         Option[GnirsReadMode],
    defaultWellDepth:         GnirsWellDepth,
    explicitWellDepth:        Option[GnirsWellDepth],
    explicitFocusMotorSteps:  Option[GnirsFocusMotorStepsValue],
    exposureTimeMode:         ExposureTimeMode,
    coadds:                   PosInt,
    acquisition:              GnirsSpectroscopy.Acquisition
  ) extends ObservingMode(Instrument.Gnirs.some):
    val decker: GnirsDecker       =
      explicitDecker.getOrElse(defaultDecker)
    val wellDepth: GnirsWellDepth =
      explicitWellDepth.getOrElse(defaultWellDepth)

    def fpu: GnirsFpu.Spectroscopy                                 =
      subMode.fold(s => GnirsFpu.Spectroscopy.Slit(s.fpu), i => GnirsFpu.Spectroscopy.Ifu(i.fpu))
    def initialFpu: GnirsFpu.Spectroscopy                          =
      subMode.fold(
        s => GnirsFpu.Spectroscopy.Slit(s.initialFpu),
        i => GnirsFpu.Spectroscopy.Ifu(i.initialFpu)
      )
    def defaultTelescopeConfigsSlit: Option[SlitTelescopeConfigs]  =
      GnirsSpectroscopy.SubMode.slit.getOption(subMode).map(_.defaultTelescopeConfigs)
    def explicitTelescopeConfigsSlit: Option[SlitTelescopeConfigs] =
      GnirsSpectroscopy.SubMode.slit.getOption(subMode).flatMap(_.explicitTelescopeConfigs)
    def telescopeConfigsSlit: Option[SlitTelescopeConfigs]         =
      explicitTelescopeConfigsSlit.orElse(defaultTelescopeConfigsSlit)
    def telescopeConfigsIfu: Option[NonEmptyList[TelescopeConfig]] =
      GnirsSpectroscopy.SubMode.ifu.getOption(subMode).map(_.telescopeConfigs)

    def isCustomized: Boolean =
      initialGrating =!= grating ||
        initialFilter =!= filter ||
        initialPrism =!= prism ||
        initialCamera =!= camera ||
        initialCentralWavelength =!= centralWavelength ||
        explicitDecker.exists(_ =!= defaultDecker) ||
        explicitReadMode.isDefined ||
        explicitWellDepth.exists(_ =!= defaultWellDepth) ||
        explicitFocusMotorSteps.isDefined ||
        subMode.isCustomized ||
        acquisition.isCustomized

    def revertCustomizations: GnirsSpectroscopy =
      this.copy(
        grating = this.initialGrating,
        filter = this.initialFilter,
        subMode = this.subMode.reverted,
        prism = this.initialPrism,
        camera = this.initialCamera,
        centralWavelength = this.initialCentralWavelength,
        explicitDecker = None,
        explicitReadMode = None,
        explicitWellDepth = None,
        explicitFocusMotorSteps = None,
        acquisition = acquisition.revertCustomizations
      )

  object GnirsSpectroscopy:
    case class Acquisition(
      explicitAcquisitionMode: Option[GnirsAcquisitionMode],
      explicitFilter:          Option[GnirsFilter],
      exposureTimeMode:        ExposureTimeMode,
      coadds:                  PosInt
    ) derives Eq {
      def isCustomized: Boolean =
        explicitAcquisitionMode.isDefined ||
          explicitFilter.isDefined

      def revertCustomizations: Acquisition =
        this.copy(explicitAcquisitionMode = none, explicitFilter = none)
    }

    object Acquisition {
      given Decoder[Acquisition] = Decoder.instance: c =>
        for
          explicitAcquisitionType <-
            c.downField("explicitAcquisitionType").as[Option[GnirsAcquisitionType]]
          skyOffset               <- c.downField("skyOffset").as[Option[Offset]]
          explicitAcquisitionMode  =
            explicitAcquisitionType.map(GnirsAcquisitionMode.forTypeAndOffset(_, skyOffset))
          explicitFilter          <- c.downField("explicitFilter").as[Option[GnirsFilter]]
          exposureTimeMode        <- c.downField("exposureTimeMode").as[ExposureTimeMode]
          coadds                  <- c.downField("coadds").as[PosInt]
        yield Acquisition(
          explicitAcquisitionMode,
          explicitFilter,
          exposureTimeMode,
          coadds
        )

      val explicitAcquisitionMode: Lens[Acquisition, Option[GnirsAcquisitionMode]] =
        Focus[Acquisition](_.explicitAcquisitionMode)
      val explicitFilter: Lens[Acquisition, Option[GnirsFilter]]                   =
        Focus[Acquisition](_.explicitFilter)
      val exposureTimeMode: Lens[Acquisition, ExposureTimeMode]                    =
        Focus[Acquisition](_.exposureTimeMode)
      val coadds: Lens[Acquisition, PosInt]                                        =
        Focus[Acquisition](_.coadds)
    }

    sealed trait SubMode derives Eq:
      def fold[A](fs: SubMode.Slit => A, fi: SubMode.Ifu => A): A
      def isCustomized: Boolean
      def reverted: SubMode

    object SubMode:
      case class Slit(
        initialFpu:               GnirsFpuSlit,
        fpu:                      GnirsFpuSlit,
        defaultTelescopeConfigs:  SlitTelescopeConfigs,
        explicitTelescopeConfigs: Option[SlitTelescopeConfigs]
      ) extends SubMode derives Eq:
        def fold[A](fs: Slit => A, fi: Ifu => A): A = fs(this)
        def isCustomized: Boolean                   =
          initialFpu =!= fpu || explicitTelescopeConfigs.exists(_ =!= defaultTelescopeConfigs)
        def reverted: Slit                          =
          copy(fpu = initialFpu, explicitTelescopeConfigs = None)

      object Slit:
        val fpu: Lens[Slit, GnirsFpuSlit]                                      = Focus[Slit](_.fpu)
        val explicitTelescopeConfigs: Lens[Slit, Option[SlitTelescopeConfigs]] =
          Focus[Slit](_.explicitTelescopeConfigs)
        given Decoder[Slit]                                                    = deriveDecoder

      case class Ifu(
        initialFpu:       GnirsFpuIfu,
        fpu:              GnirsFpuIfu,
        telescopeConfigs: NonEmptyList[TelescopeConfig]
      ) extends SubMode derives Eq:
        def fold[A](fs: Slit => A, fi: Ifu => A): A = fi(this)
        // No server default; the canonical seed is the FPU's first preset.
        def isCustomized: Boolean                   =
          initialFpu =!= fpu || telescopeConfigs =!= defaultIfuTelescopeConfigs(fpu)
        def reverted: Ifu                           =
          copy(fpu = initialFpu, telescopeConfigs = defaultIfuTelescopeConfigs(initialFpu))

      object Ifu:
        val fpu: Lens[Ifu, GnirsFpuIfu]                                = Focus[Ifu](_.fpu)
        val telescopeConfigs: Lens[Ifu, NonEmptyList[TelescopeConfig]] =
          Focus[Ifu](_.telescopeConfigs)
        given Decoder[Ifu]                                             = deriveDecoder

      val slit: Prism[SubMode, Slit] = GenPrism[SubMode, Slit]
      val ifu: Prism[SubMode, Ifu]   = GenPrism[SubMode, Ifu]

    given Decoder[GnirsSpectroscopy] = Decoder.instance: c =>
      for
        initialGrating           <- c.downField("initialGrating").as[GnirsGrating]
        grating                  <- c.downField("grating").as[GnirsGrating]
        initialFilter            <- c.downField("initialFilter").as[GnirsFilter]
        filter                   <- c.downField("filter").as[GnirsFilter]
        subMode                  <- c.downField("slit")
                                      .as[SubMode.Slit]
                                      .orElse:
                                        c.downField("ifu").as[SubMode.Ifu]
        initialPrism             <- c.downField("initialPrism").as[GnirsPrism]
        prism                    <- c.downField("prism").as[GnirsPrism]
        initialCamera            <- c.downField("initialCamera").as[GnirsCamera]
        camera                   <- c.downField("camera").as[GnirsCamera]
        initialCentralWavelength <- c.downField("initialCentralWavelength").as[CentralWavelength]
        centralWavelength        <- c.downField("centralWavelength").as[CentralWavelength]
        defaultDecker            <- c.downField("defaultDecker").as[GnirsDecker]
        explicitDecker           <- c.downField("explicitDecker").as[Option[GnirsDecker]]
        explicitReadMode         <- c.downField("explicitReadMode").as[Option[GnirsReadMode]]
        defaultWellDepth         <- c.downField("defaultWellDepth").as[GnirsWellDepth]
        explicitWellDepth        <- c.downField("explicitWellDepth").as[Option[GnirsWellDepth]]
        explicitFocusMotorSteps  <-
          c.downField("explicitFocusMotorSteps").as[Option[GnirsFocusMotorStepsValue]]
        exposureTimeMode         <- c.downField("exposureTimeMode").as[ExposureTimeMode]
        coadds                   <- c.downField("coadds").as[PosInt]
        acquisition              <- c.downField("acquisition").as[Acquisition]
      yield GnirsSpectroscopy(
        initialGrating,
        grating,
        initialFilter,
        filter,
        subMode,
        initialPrism,
        prism,
        initialCamera,
        camera,
        initialCentralWavelength,
        centralWavelength,
        defaultDecker,
        explicitDecker,
        explicitReadMode,
        defaultWellDepth,
        explicitWellDepth,
        explicitFocusMotorSteps,
        exposureTimeMode,
        coadds,
        acquisition
      )

    given Eq[GnirsSpectroscopy] = Eq.by: x => // We use tuples since there are too many fields.
      (
        (x.initialGrating, x.grating),
        (x.initialFilter, x.filter),
        x.subMode,
        (x.initialPrism, x.prism),
        (x.initialCamera, x.camera),
        (x.initialCentralWavelength, x.centralWavelength),
        (x.defaultDecker, x.explicitDecker),
        x.explicitReadMode,
        (x.defaultWellDepth, x.explicitWellDepth),
        x.exposureTimeMode,
        x.coadds,
        x.acquisition
      )

    val initialGrating: Lens[GnirsSpectroscopy, GnirsGrating]                               =
      Focus[GnirsSpectroscopy](_.initialGrating)
    val grating: Lens[GnirsSpectroscopy, GnirsGrating]                                      =
      Focus[GnirsSpectroscopy](_.grating)
    val initialFilter: Lens[GnirsSpectroscopy, GnirsFilter]                                 =
      Focus[GnirsSpectroscopy](_.initialFilter)
    val filter: Lens[GnirsSpectroscopy, GnirsFilter]                                        =
      Focus[GnirsSpectroscopy](_.filter)
    val subMode: Lens[GnirsSpectroscopy, SubMode]                                           =
      Focus[GnirsSpectroscopy](_.subMode)
    val initialPrism: Lens[GnirsSpectroscopy, GnirsPrism]                                   =
      Focus[GnirsSpectroscopy](_.initialPrism)
    val prism: Lens[GnirsSpectroscopy, GnirsPrism]                                          =
      Focus[GnirsSpectroscopy](_.prism)
    val initialCamera: Lens[GnirsSpectroscopy, GnirsCamera]                                 =
      Focus[GnirsSpectroscopy](_.initialCamera)
    val camera: Lens[GnirsSpectroscopy, GnirsCamera]                                        =
      Focus[GnirsSpectroscopy](_.camera)
    val initialCentralWavelength: Lens[GnirsSpectroscopy, CentralWavelength]                =
      Focus[GnirsSpectroscopy](_.initialCentralWavelength)
    val centralWavelength: Lens[GnirsSpectroscopy, CentralWavelength]                       =
      Focus[GnirsSpectroscopy](_.centralWavelength)
    val defaultDecker: Lens[GnirsSpectroscopy, GnirsDecker]                                 =
      Focus[GnirsSpectroscopy](_.defaultDecker)
    val explicitDecker: Lens[GnirsSpectroscopy, Option[GnirsDecker]]                        =
      Focus[GnirsSpectroscopy](_.explicitDecker)
    val explicitReadMode: Lens[GnirsSpectroscopy, Option[GnirsReadMode]]                    =
      Focus[GnirsSpectroscopy](_.explicitReadMode)
    val defaultWellDepth: Lens[GnirsSpectroscopy, GnirsWellDepth]                           =
      Focus[GnirsSpectroscopy](_.defaultWellDepth)
    val explicitWellDepth: Lens[GnirsSpectroscopy, Option[GnirsWellDepth]]                  =
      Focus[GnirsSpectroscopy](_.explicitWellDepth)
    val explicitFocusMotorSteps: Lens[GnirsSpectroscopy, Option[GnirsFocusMotorStepsValue]] =
      Focus[GnirsSpectroscopy](_.explicitFocusMotorSteps)
    val exposureTimeMode: Lens[GnirsSpectroscopy, ExposureTimeMode]                         =
      Focus[GnirsSpectroscopy](_.exposureTimeMode)
    val coadds: Lens[GnirsSpectroscopy, PosInt]                                             =
      Focus[GnirsSpectroscopy](_.coadds)
    val acquisition: Lens[GnirsSpectroscopy, GnirsSpectroscopy.Acquisition]                 =
      Focus[GnirsSpectroscopy](_.acquisition)

  case class GhostIfu(
    resolutionMode:       GhostResolutionMode,
    skyPosition:          Option[Coordinates],
    signalToNoiseAt:      Wavelength,
    stepCount:            PosInt,
    red:                  GhostIfu.GhostDetector,
    blue:                 GhostIfu.GhostDetector,
    defaultIfu1Agitator:  GhostIfu1FiberAgitator,
    explicitIfu1Agitator: Option[GhostIfu1FiberAgitator],
    defaultIfu2Agitator:  GhostIfu2FiberAgitator,
    explicitIfu2Agitator: Option[GhostIfu2FiberAgitator]
  ) extends ObservingMode(Instrument.Ghost.some) derives Eq:
    val ifu1Agitator: GhostIfu1FiberAgitator = explicitIfu1Agitator.getOrElse(defaultIfu1Agitator)
    val ifu2Agitator: GhostIfu2FiberAgitator = explicitIfu2Agitator.getOrElse(defaultIfu2Agitator)
    def isCustomized: Boolean                =
      explicitIfu1Agitator.exists(_ =!= defaultIfu1Agitator) ||
        explicitIfu2Agitator.exists(_ =!= defaultIfu2Agitator) ||
        red.isCustomized ||
        blue.isCustomized
    def revertCustomizations: GhostIfu       =
      this.copy(
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
        skyPosition          <- c.downField("skyPosition").as[Option[Coordinates]]
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
        skyPosition,
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

    val skyPosition: Lens[GhostIfu, Option[Coordinates]] =
      Focus[GhostIfu](_.skyPosition)

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

  case class Visitor(
    mode:               VisitorObservingModeType,
    centralWavelength:  CentralWavelength,
    agsDiameter:        Angle,
    scienceFovDiameter: Angle,
    name:               Option[NonEmptyString],
    totalRequestTime:   Option[TimeSpan]
  ) extends ObservingMode(mode.instrument.some) derives Eq:
    def isCustomized: Boolean = false

  object Visitor:
    val mode: Lens[Visitor, VisitorObservingModeType]       =
      Focus[Visitor](_.mode)
    val centralWavelength: Lens[Visitor, CentralWavelength] =
      Focus[Visitor](_.centralWavelength)
    val agsDiameter: Lens[Visitor, Angle]                   =
      Focus[Visitor](_.agsDiameter)
    val scienceFovDiameter: Lens[Visitor, Angle]            =
      Focus[Visitor](_.scienceFovDiameter)
    val name: Lens[Visitor, Option[NonEmptyString]]         =
      Focus[Visitor](_.name)
    val totalRequestTime: Lens[Visitor, Option[TimeSpan]]   =
      Focus[Visitor](_.totalRequestTime)

    given Decoder[Visitor] = Decoder.instance: c =>
      for
        mode <- c.downField("mode").as[VisitorObservingModeType]
        cw   <- c.downField("centralWavelength").as[Wavelength]
        gsms <- c.downField("agsDiameter").as[Angle]
        fov  <- c.downField("scienceFovDiameter").as[Angle]
        name <- c.downField("name").as[Option[NonEmptyString]]
        trt  <- c.downField("totalRequestTime").as[Option[TimeSpan]]
      yield Visitor(mode, CentralWavelength(cw), gsms, fov, name, trt)

  case class KeckExchange(
    keckInstrument:   KeckInstrument,
    totalRequestTime: TimeSpan
  ) extends ObservingMode(none) derives Eq:
    val isCustomized: Boolean = false

  object KeckExchange:
    val Default: KeckExchange =
      KeckExchange(Enumerated[KeckInstrument].all.head, TimeSpan.Zero)

    val keckInstrument: Lens[KeckExchange, KeckInstrument] =
      Focus[KeckExchange](_.keckInstrument)
    val totalRequestTime: Lens[KeckExchange, TimeSpan]     =
      Focus[KeckExchange](_.totalRequestTime)

    given Decoder[KeckExchange] = Decoder.instance: c =>
      for
        keckInstrument   <- c.downField("keckInstrument").as[KeckInstrument]
        totalRequestTime <- c.downField("totalRequestTime").as[TimeSpan]
      yield KeckExchange(keckInstrument, totalRequestTime)

  case class SubaruExchange(
    subaruInstrument: SubaruInstrument,
    totalRequestTime: TimeSpan
  ) extends ObservingMode(none) derives Eq:
    val isCustomized: Boolean = false

  object SubaruExchange:
    val Default: SubaruExchange =
      SubaruExchange(Enumerated[SubaruInstrument].all.head, TimeSpan.Zero)

    val subaruInstrument: Lens[SubaruExchange, SubaruInstrument] =
      Focus[SubaruExchange](_.subaruInstrument)
    val totalRequestTime: Lens[SubaruExchange, TimeSpan]         =
      Focus[SubaruExchange](_.totalRequestTime)

    given Decoder[SubaruExchange] = Decoder.instance: c =>
      for
        subaruInstrument <- c.downField("subaruInstrument").as[SubaruInstrument]
        totalRequestTime <- c.downField("totalRequestTime").as[TimeSpan]
      yield SubaruExchange(subaruInstrument, totalRequestTime)

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

  val flamingos2Imaging: Prism[ObservingMode, Flamingos2Imaging] =
    GenPrism[ObservingMode, Flamingos2Imaging]

  val gnirsImaging: Prism[ObservingMode, GnirsImaging] =
    GenPrism[ObservingMode, GnirsImaging]

  val igrins2LongSlit: Prism[ObservingMode, Igrins2LongSlit] =
    GenPrism[ObservingMode, Igrins2LongSlit]

  val gnirsSpectroscopy: Prism[ObservingMode, GnirsSpectroscopy] =
    GenPrism[ObservingMode, GnirsSpectroscopy]

  val ghostIfu: Prism[ObservingMode, GhostIfu] =
    GenPrism[ObservingMode, GhostIfu]

  val visitor: Prism[ObservingMode, Visitor] =
    GenPrism[ObservingMode, Visitor]

  val keckExchange: Prism[ObservingMode, KeckExchange] =
    GenPrism[ObservingMode, KeckExchange]

  val subaruExchange: Prism[ObservingMode, SubaruExchange] =
    GenPrism[ObservingMode, SubaruExchange]

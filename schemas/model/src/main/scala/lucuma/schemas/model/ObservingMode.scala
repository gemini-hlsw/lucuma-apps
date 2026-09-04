// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import lucuma.core.model.Attachment
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.GmosIfuAnalysis
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.TelluricType
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMode
import lucuma.core.model.sequence.gnirs.GnirsFocusMotorStepsValue
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.core.model.sequence.gnirs.defaultIfuTelescopeConfigs
import lucuma.core.model.sequence.igrins2.SvcDefaultExposure
import lucuma.core.model.sequence.igrins2.SvcDefaultTelescopeConfigs
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.itc.ItcGhostDetector
import lucuma.odb.json.angle.decoder.given
import lucuma.odb.json.coordinates.query.given
import lucuma.odb.json.offset.decoder.given
import lucuma.odb.json.stepconfig.given
import lucuma.odb.json.tellurictype.decoder.given
import lucuma.odb.json.time.decoder.given
import lucuma.odb.json.wavelength
import lucuma.odb.json.wavelength.decoder.given
import lucuma.refined.*
import lucuma.schemas.decoders.given
import monocle.Focus
import monocle.Lens
import monocle.Optional
import monocle.Prism
import monocle.macros.GenPrism

sealed abstract class ObservingMode(val instrument: Option[Instrument])
    extends Product
    with Serializable derives Eq {
  def isCustomized: Boolean

  def obsModeType: ObservingModeType = this match
    case _: ObservingMode.GmosNorthLongSlit  => ObservingModeType.GmosNorthLongSlit
    case _: ObservingMode.GmosSouthLongSlit  => ObservingModeType.GmosSouthLongSlit
    case _: ObservingMode.GmosNorthMos       => ObservingModeType.GmosNorthMos
    case _: ObservingMode.GmosSouthMos       => ObservingModeType.GmosSouthMos
    case _: ObservingMode.GmosNorthIfu       => ObservingModeType.GmosNorthIfu
    case _: ObservingMode.GmosSouthIfu       => ObservingModeType.GmosSouthIfu
    case _: ObservingMode.GmosNorthImaging   => ObservingModeType.GmosNorthImaging
    case _: ObservingMode.GmosSouthImaging   => ObservingModeType.GmosSouthImaging
    case _: ObservingMode.Flamingos2Imaging  => ObservingModeType.Flamingos2Imaging
    case _: ObservingMode.Flamingos2LongSlit => ObservingModeType.Flamingos2LongSlit
    case _: ObservingMode.Flamingos2Mos      => ObservingModeType.Flamingos2Mos
    case _: ObservingMode.Igrins2LongSlit    => ObservingModeType.Igrins2LongSlit
    case _: ObservingMode.GnirsImaging       => ObservingModeType.GnirsImaging
    case _: ObservingMode.GnirsLongSlit      => ObservingModeType.GnirsLongSlit
    case _: ObservingMode.GnirsIfu           => ObservingModeType.GnirsIfu
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
    case _: ObservingMode.GmosNorthMos       => Site.GN.some
    case _: ObservingMode.GmosSouthMos       => Site.GS.some
    case _: ObservingMode.GmosNorthIfu       => Site.GN.some
    case _: ObservingMode.GmosSouthIfu       => Site.GS.some
    case _: ObservingMode.GmosNorthImaging   => Site.GN.some
    case _: ObservingMode.GmosSouthImaging   => Site.GS.some
    case _: ObservingMode.Flamingos2Imaging  => Site.GS.some
    case _: ObservingMode.Flamingos2LongSlit => Site.GS.some
    case _: ObservingMode.Flamingos2Mos      => Site.GS.some
    case _: ObservingMode.Igrins2LongSlit    => Site.GN.some
    case _: ObservingMode.GnirsImaging       => Site.GN.some
    case _: ObservingMode.GnirsLongSlit      => Site.GN.some
    case _: ObservingMode.GnirsIfu           => Site.GN.some
    case _: ObservingMode.GhostIfu           => Site.GS.some
    case v: ObservingMode.Visitor            => v.toBasicConfiguration.siteFor
    case _: ObservingMode.KeckExchange       => none
    case _: ObservingMode.SubaruExchange     => none

  def toBasicConfiguration: BasicConfiguration = this match
    case n: ObservingMode.GmosNorthLongSlit                        =>
      BasicConfiguration.GmosNorthLongSlit(n.grating, n.filter, n.fpu, n.centralWavelength)
    case s: ObservingMode.GmosSouthLongSlit                        =>
      BasicConfiguration.GmosSouthLongSlit(s.grating, s.filter, s.fpu, s.centralWavelength)
    case n: ObservingMode.GmosNorthMos                             =>
      BasicConfiguration.GmosNorthMos(n.grating,
                                      n.filter,
                                      n.customMask.slitWidth,
                                      n.centralWavelength
      )
    case s: ObservingMode.GmosSouthMos                             =>
      BasicConfiguration.GmosSouthMos(s.grating,
                                      s.filter,
                                      s.customMask.slitWidth,
                                      s.centralWavelength
      )
    case n: ObservingMode.GmosNorthIfu                             =>
      BasicConfiguration.GmosNorthIfu(n.grating, n.filter, n.fpu, n.centralWavelength)
    case s: ObservingMode.GmosSouthIfu                             =>
      BasicConfiguration.GmosSouthIfu(s.grating, s.filter, s.fpu, s.centralWavelength)
    case ObservingMode.GmosNorthImaging(filters = filters)         =>
      BasicConfiguration.GmosNorthImaging(filters.map(_.filter))
    case ObservingMode.GmosSouthImaging(filters = filters)         =>
      BasicConfiguration.GmosSouthImaging(filters.map(_.filter))
    case ObservingMode.Flamingos2Imaging(filters = filters)        =>
      BasicConfiguration.Flamingos2Imaging(filters.map(_.filter))
    case f: ObservingMode.Flamingos2LongSlit                       =>
      BasicConfiguration.Flamingos2LongSlit(f.disperser, f.filter, f.fpu)
    case f: ObservingMode.Flamingos2Mos                            =>
      BasicConfiguration.Flamingos2Mos(f.disperser, f.filter, f.customMask.slitWidth)
    case _: ObservingMode.Igrins2LongSlit                          =>
      BasicConfiguration.Igrins2LongSlit
    case g: ObservingMode.GnirsImaging                             =>
      BasicConfiguration.GnirsImaging(g.filters.map(_.filter), g.camera)
    case g: ObservingMode.GnirsLongSlit                            =>
      BasicConfiguration
        .GnirsSpectroscopy(g.filter,
                           GnirsFpu.Spectroscopy.Slit(g.fpu),
                           g.prism,
                           g.grating,
                           g.camera,
                           g.centralWavelengths.head.centralWavelength
        )
    case g: ObservingMode.GnirsIfu                                 =>
      BasicConfiguration
        .GnirsSpectroscopy(g.filter,
                           GnirsFpu.Spectroscopy.Ifu(g.fpu),
                           g.prism,
                           g.grating,
                           g.camera,
                           g.centralWavelengths.head.centralWavelength
        )
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
            c.downField("gmosNorthMos").as[GmosNorthMos]
          .orElse:
            c.downField("gmosSouthMos").as[GmosSouthMos]
          .orElse:
            c.downField("gmosNorthIfu").as[GmosNorthIfu]
          .orElse:
            c.downField("gmosSouthIfu").as[GmosSouthIfu]
          .orElse:
            c.downField("gmosNorthImaging").as[GmosNorthImaging]
          .orElse:
            c.downField("gmosSouthImaging").as[GmosSouthImaging]
          .orElse:
            c.downField("flamingos2Imaging").as[Flamingos2Imaging]
          .orElse:
            c.downField("flamingos2LongSlit").as[Flamingos2LongSlit]
          .orElse:
            c.downField("flamingos2Mos").as[Flamingos2Mos]
          .orElse:
            c.downField("igrins2LongSlit").as[Igrins2LongSlit]
          .orElse:
            c.downField("gnirsImaging").as[GnirsImaging]
          .orElse:
            c.downField("gnirsLongSlit").as[GnirsLongSlit]
          .orElse:
            c.downField("gnirsIfu").as[GnirsIfu]
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
    defaultTelescopeConfigs:   SlitTelescopeConfigs,
    explicitTelescopeConfigs:  Option[SlitTelescopeConfigs],
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
    val telescopeConfigs: SlitTelescopeConfigs            =
      explicitTelescopeConfigs.getOrElse(defaultTelescopeConfigs)

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
        explicitTelescopeConfigs.exists(_ =!= defaultTelescopeConfigs) ||
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
        explicitTelescopeConfigs = None,
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
    val defaultTelescopeConfigs: Lens[GmosNorthLongSlit, SlitTelescopeConfigs]                     =
      Focus[GmosNorthLongSlit](_.defaultTelescopeConfigs)
    val explicitTelescopeConfigs: Lens[GmosNorthLongSlit, Option[SlitTelescopeConfigs]]            =
      Focus[GmosNorthLongSlit](_.explicitTelescopeConfigs)
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
    defaultTelescopeConfigs:   SlitTelescopeConfigs,
    explicitTelescopeConfigs:  Option[SlitTelescopeConfigs],
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
    val telescopeConfigs: SlitTelescopeConfigs            =
      explicitTelescopeConfigs.getOrElse(defaultTelescopeConfigs)

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
        explicitTelescopeConfigs.exists(_ =!= defaultTelescopeConfigs) ||
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
        explicitTelescopeConfigs = None,
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
    val defaultTelescopeConfigs: Lens[GmosSouthLongSlit, SlitTelescopeConfigs]                     =
      Focus[GmosSouthLongSlit](_.defaultTelescopeConfigs)
    val explicitTelescopeConfigs: Lens[GmosSouthLongSlit, Option[SlitTelescopeConfigs]]            =
      Focus[GmosSouthLongSlit](_.explicitTelescopeConfigs)
    val exposureTimeMode: Lens[GmosSouthLongSlit, ExposureTimeMode]                                =
      Focus[GmosSouthLongSlit](_.exposureTimeMode)
    val acquisition: Lens[GmosSouthLongSlit, GmosSouthLongSlit.Acquisition]                        =
      Focus[GmosSouthLongSlit](_.acquisition)

  case class GmosCustomMask(
    attachmentId: Option[Attachment.Id],
    slitWidth:    GmosCustomSlitWidth
  ) derives Decoder,
        Eq

  object GmosCustomMask:
    val attachmentId: Lens[GmosCustomMask, Option[Attachment.Id]] =
      Focus[GmosCustomMask](_.attachmentId)
    val slitWidth: Lens[GmosCustomMask, GmosCustomSlitWidth]      =
      Focus[GmosCustomMask](_.slitWidth)

  case class GmosNorthMos(
    initialGrating:            GmosNorthGrating,
    grating:                   GmosNorthGrating,
    initialFilter:             Option[GmosNorthFilter],
    filter:                    Option[GmosNorthFilter],
    initialSlitWidth:          GmosCustomSlitWidth,
    customMask:                GmosCustomMask,
    initialCentralWavelength:  CentralWavelength,
    centralWavelength:         CentralWavelength,
    acquisitionType:           GmosMosAcquisitionType,
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
    defaultTelescopeConfigs:   NonEmptyList[TelescopeConfig],
    explicitTelescopeConfigs:  Option[NonEmptyList[TelescopeConfig]],
    exposureTimeMode:          ExposureTimeMode,
    acquisition:               GmosNorthMos.Acquisition
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
    val telescopeConfigs: NonEmptyList[TelescopeConfig]   =
      explicitTelescopeConfigs.getOrElse(defaultTelescopeConfigs)

    def isCustomized: Boolean =
      initialGrating =!= grating ||
        initialFilter =!= filter ||
        initialSlitWidth =!= customMask.slitWidth ||
        initialCentralWavelength =!= centralWavelength ||
        explicitXBin.exists(_ =!= defaultXBin) ||
        explicitYBin.exists(_ =!= defaultYBin) ||
        explicitAmpReadMode.exists(_ =!= defaultAmpReadMode) ||
        explicitAmpGain.exists(_ =!= defaultAmpGain) ||
        explicitRoi.exists(_ =!= defaultRoi) ||
        explicitWavelengthDithers.exists(_ =!= defaultWavelengthDithers) ||
        explicitTelescopeConfigs.exists(_ =!= defaultTelescopeConfigs) ||
        acquisition.isCustomized

    def revertCustomizations: GmosNorthMos =
      this.copy(
        grating = this.initialGrating,
        filter = this.initialFilter,
        customMask = GmosCustomMask.slitWidth.replace(this.initialSlitWidth)(this.customMask),
        centralWavelength = this.initialCentralWavelength,
        explicitXBin = None,
        explicitYBin = None,
        explicitAmpReadMode = None,
        explicitAmpGain = None,
        explicitRoi = None,
        explicitWavelengthDithers = None,
        explicitTelescopeConfigs = None,
        acquisition = acquisition.revertCustomizations
      )

  object GmosNorthMos:
    case class Acquisition(
      defaultFilter:    GmosNorthFilter,
      explicitFilter:   Option[GmosNorthFilter],
      exposureTimeMode: ExposureTimeMode
    ) derives Decoder,
          Eq:
      val filter                            = explicitFilter.getOrElse(defaultFilter)
      def isCustomized: Boolean             =
        explicitFilter.exists(_ =!= defaultFilter)
      def revertCustomizations: Acquisition =
        this.copy(explicitFilter = None)

    object Acquisition:
      val defaultFilter: Lens[Acquisition, GmosNorthFilter]          =
        Focus[Acquisition](_.defaultFilter)
      val explicitFilter: Lens[Acquisition, Option[GmosNorthFilter]] =
        Focus[Acquisition](_.explicitFilter)
      val exposureTimeMode: Lens[Acquisition, ExposureTimeMode]      =
        Focus[Acquisition](_.exposureTimeMode)

    given Decoder[GmosNorthMos] = deriveDecoder

    val initialGrating: Lens[GmosNorthMos, GmosNorthGrating]                                  =
      Focus[GmosNorthMos](_.initialGrating)
    val grating: Lens[GmosNorthMos, GmosNorthGrating]                                         =
      Focus[GmosNorthMos](_.grating)
    val initialFilter: Lens[GmosNorthMos, Option[GmosNorthFilter]]                            =
      Focus[GmosNorthMos](_.initialFilter)
    val filter: Lens[GmosNorthMos, Option[GmosNorthFilter]]                                   =
      Focus[GmosNorthMos](_.filter)
    val initialSlitWidth: Lens[GmosNorthMos, GmosCustomSlitWidth]                             =
      Focus[GmosNorthMos](_.initialSlitWidth)
    val customMask: Lens[GmosNorthMos, GmosCustomMask]                                        =
      Focus[GmosNorthMos](_.customMask)
    val initialCentralWavelength: Lens[GmosNorthMos, CentralWavelength]                       =
      Focus[GmosNorthMos](_.initialCentralWavelength)
    val centralWavelength: Lens[GmosNorthMos, CentralWavelength]                              =
      Focus[GmosNorthMos](_.centralWavelength)
    val acquisitionType: Lens[GmosNorthMos, GmosMosAcquisitionType]                           =
      Focus[GmosNorthMos](_.acquisitionType)
    val defaultXBin: Lens[GmosNorthMos, GmosXBinning]                                         =
      Focus[GmosNorthMos](_.defaultXBin)
    val explicitXBin: Lens[GmosNorthMos, Option[GmosXBinning]]                                =
      Focus[GmosNorthMos](_.explicitXBin)
    val defaultYBin: Lens[GmosNorthMos, GmosYBinning]                                         =
      Focus[GmosNorthMos](_.defaultYBin)
    val explicitYBin: Lens[GmosNorthMos, Option[GmosYBinning]]                                =
      Focus[GmosNorthMos](_.explicitYBin)
    val defaultAmpReadMode: Lens[GmosNorthMos, GmosAmpReadMode]                               =
      Focus[GmosNorthMos](_.defaultAmpReadMode)
    val explicitAmpReadMode: Lens[GmosNorthMos, Option[GmosAmpReadMode]]                      =
      Focus[GmosNorthMos](_.explicitAmpReadMode)
    val defaultAmpGain: Lens[GmosNorthMos, GmosAmpGain]                                       =
      Focus[GmosNorthMos](_.defaultAmpGain)
    val explicitAmpGain: Lens[GmosNorthMos, Option[GmosAmpGain]]                              =
      Focus[GmosNorthMos](_.explicitAmpGain)
    val defaultRoi: Lens[GmosNorthMos, GmosRoi]                                               =
      Focus[GmosNorthMos](_.defaultRoi)
    val explicitRoi: Lens[GmosNorthMos, Option[GmosRoi]]                                      =
      Focus[GmosNorthMos](_.explicitRoi)
    val defaultWavelengthDithers: Lens[GmosNorthMos, NonEmptyList[WavelengthDither]]          =
      Focus[GmosNorthMos](_.defaultWavelengthDithers)
    val explicitWavelengthDithers: Lens[GmosNorthMos, Option[NonEmptyList[WavelengthDither]]] =
      Focus[GmosNorthMos](_.explicitWavelengthDithers)
    val defaultTelescopeConfigs: Lens[GmosNorthMos, NonEmptyList[TelescopeConfig]]            =
      Focus[GmosNorthMos](_.defaultTelescopeConfigs)
    val explicitTelescopeConfigs: Lens[GmosNorthMos, Option[NonEmptyList[TelescopeConfig]]]   =
      Focus[GmosNorthMos](_.explicitTelescopeConfigs)
    val exposureTimeMode: Lens[GmosNorthMos, ExposureTimeMode]                                =
      Focus[GmosNorthMos](_.exposureTimeMode)
    val acquisition: Lens[GmosNorthMos, GmosNorthMos.Acquisition]                             =
      Focus[GmosNorthMos](_.acquisition)

  case class GmosSouthMos(
    initialGrating:            GmosSouthGrating,
    grating:                   GmosSouthGrating,
    initialFilter:             Option[GmosSouthFilter],
    filter:                    Option[GmosSouthFilter],
    initialSlitWidth:          GmosCustomSlitWidth,
    customMask:                GmosCustomMask,
    initialCentralWavelength:  CentralWavelength,
    centralWavelength:         CentralWavelength,
    acquisitionType:           GmosMosAcquisitionType,
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
    defaultTelescopeConfigs:   NonEmptyList[TelescopeConfig],
    explicitTelescopeConfigs:  Option[NonEmptyList[TelescopeConfig]],
    exposureTimeMode:          ExposureTimeMode,
    acquisition:               GmosSouthMos.Acquisition
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
    val telescopeConfigs: NonEmptyList[TelescopeConfig]   =
      explicitTelescopeConfigs.getOrElse(defaultTelescopeConfigs)

    def isCustomized: Boolean =
      initialGrating =!= grating ||
        initialFilter =!= filter ||
        initialSlitWidth =!= customMask.slitWidth ||
        initialCentralWavelength =!= centralWavelength ||
        explicitXBin.exists(_ =!= defaultXBin) ||
        explicitYBin.exists(_ =!= defaultYBin) ||
        explicitAmpReadMode.exists(_ =!= defaultAmpReadMode) ||
        explicitAmpGain.exists(_ =!= defaultAmpGain) ||
        explicitRoi.exists(_ =!= defaultRoi) ||
        explicitWavelengthDithers.exists(_ =!= defaultWavelengthDithers) ||
        explicitTelescopeConfigs.exists(_ =!= defaultTelescopeConfigs) ||
        acquisition.isCustomized

    def revertCustomizations: GmosSouthMos =
      this.copy(
        grating = this.initialGrating,
        filter = this.initialFilter,
        customMask = GmosCustomMask.slitWidth.replace(this.initialSlitWidth)(this.customMask),
        centralWavelength = this.initialCentralWavelength,
        explicitXBin = None,
        explicitYBin = None,
        explicitAmpReadMode = None,
        explicitAmpGain = None,
        explicitRoi = None,
        explicitWavelengthDithers = None,
        explicitTelescopeConfigs = None,
        acquisition = acquisition.revertCustomizations
      )

  object GmosSouthMos:
    case class Acquisition(
      defaultFilter:    GmosSouthFilter,
      explicitFilter:   Option[GmosSouthFilter],
      exposureTimeMode: ExposureTimeMode
    ) derives Decoder,
          Eq:
      val filter                            = explicitFilter.getOrElse(defaultFilter)
      def isCustomized: Boolean             =
        explicitFilter.exists(_ =!= defaultFilter)
      def revertCustomizations: Acquisition =
        this.copy(explicitFilter = None)

    object Acquisition:
      val defaultFilter: Lens[Acquisition, GmosSouthFilter]          =
        Focus[Acquisition](_.defaultFilter)
      val explicitFilter: Lens[Acquisition, Option[GmosSouthFilter]] =
        Focus[Acquisition](_.explicitFilter)
      val exposureTimeMode: Lens[Acquisition, ExposureTimeMode]      =
        Focus[Acquisition](_.exposureTimeMode)

    given Decoder[GmosSouthMos] = deriveDecoder

    val initialGrating: Lens[GmosSouthMos, GmosSouthGrating]                                  =
      Focus[GmosSouthMos](_.initialGrating)
    val grating: Lens[GmosSouthMos, GmosSouthGrating]                                         =
      Focus[GmosSouthMos](_.grating)
    val initialFilter: Lens[GmosSouthMos, Option[GmosSouthFilter]]                            =
      Focus[GmosSouthMos](_.initialFilter)
    val filter: Lens[GmosSouthMos, Option[GmosSouthFilter]]                                   =
      Focus[GmosSouthMos](_.filter)
    val initialSlitWidth: Lens[GmosSouthMos, GmosCustomSlitWidth]                             =
      Focus[GmosSouthMos](_.initialSlitWidth)
    val customMask: Lens[GmosSouthMos, GmosCustomMask]                                        =
      Focus[GmosSouthMos](_.customMask)
    val initialCentralWavelength: Lens[GmosSouthMos, CentralWavelength]                       =
      Focus[GmosSouthMos](_.initialCentralWavelength)
    val centralWavelength: Lens[GmosSouthMos, CentralWavelength]                              =
      Focus[GmosSouthMos](_.centralWavelength)
    val acquisitionType: Lens[GmosSouthMos, GmosMosAcquisitionType]                           =
      Focus[GmosSouthMos](_.acquisitionType)
    val defaultXBin: Lens[GmosSouthMos, GmosXBinning]                                         =
      Focus[GmosSouthMos](_.defaultXBin)
    val explicitXBin: Lens[GmosSouthMos, Option[GmosXBinning]]                                =
      Focus[GmosSouthMos](_.explicitXBin)
    val defaultYBin: Lens[GmosSouthMos, GmosYBinning]                                         =
      Focus[GmosSouthMos](_.defaultYBin)
    val explicitYBin: Lens[GmosSouthMos, Option[GmosYBinning]]                                =
      Focus[GmosSouthMos](_.explicitYBin)
    val defaultAmpReadMode: Lens[GmosSouthMos, GmosAmpReadMode]                               =
      Focus[GmosSouthMos](_.defaultAmpReadMode)
    val explicitAmpReadMode: Lens[GmosSouthMos, Option[GmosAmpReadMode]]                      =
      Focus[GmosSouthMos](_.explicitAmpReadMode)
    val defaultAmpGain: Lens[GmosSouthMos, GmosAmpGain]                                       =
      Focus[GmosSouthMos](_.defaultAmpGain)
    val explicitAmpGain: Lens[GmosSouthMos, Option[GmosAmpGain]]                              =
      Focus[GmosSouthMos](_.explicitAmpGain)
    val defaultRoi: Lens[GmosSouthMos, GmosRoi]                                               =
      Focus[GmosSouthMos](_.defaultRoi)
    val explicitRoi: Lens[GmosSouthMos, Option[GmosRoi]]                                      =
      Focus[GmosSouthMos](_.explicitRoi)
    val defaultWavelengthDithers: Lens[GmosSouthMos, NonEmptyList[WavelengthDither]]          =
      Focus[GmosSouthMos](_.defaultWavelengthDithers)
    val explicitWavelengthDithers: Lens[GmosSouthMos, Option[NonEmptyList[WavelengthDither]]] =
      Focus[GmosSouthMos](_.explicitWavelengthDithers)
    val defaultTelescopeConfigs: Lens[GmosSouthMos, NonEmptyList[TelescopeConfig]]            =
      Focus[GmosSouthMos](_.defaultTelescopeConfigs)
    val explicitTelescopeConfigs: Lens[GmosSouthMos, Option[NonEmptyList[TelescopeConfig]]]   =
      Focus[GmosSouthMos](_.explicitTelescopeConfigs)
    val exposureTimeMode: Lens[GmosSouthMos, ExposureTimeMode]                                =
      Focus[GmosSouthMos](_.exposureTimeMode)
    val acquisition: Lens[GmosSouthMos, GmosSouthMos.Acquisition]                             =
      Focus[GmosSouthMos](_.acquisition)

  case class GmosNorthIfu(
    initialGrating:            GmosNorthGrating,
    grating:                   GmosNorthGrating,
    initialFilter:             Option[GmosNorthFilter],
    filter:                    Option[GmosNorthFilter],
    initialFpu:                GmosNorthIfuFpu,
    fpu:                       GmosNorthIfuFpu,
    initialCentralWavelength:  CentralWavelength,
    centralWavelength:         CentralWavelength,
    defaultIfuAnalysis:        GmosIfuAnalysis,
    explicitIfuAnalysis:       Option[GmosIfuAnalysis],
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
    defaultTelescopeConfigs:   NonEmptyList[TelescopeConfig],
    explicitTelescopeConfigs:  Option[NonEmptyList[TelescopeConfig]],
    exposureTimeMode:          ExposureTimeMode,
    acquisition:               GmosNorthIfu.Acquisition
  ) extends ObservingMode(Instrument.GmosNorth.some) derives Eq:
    val ifuAnalysis: GmosIfuAnalysis                      =
      explicitIfuAnalysis.getOrElse(defaultIfuAnalysis)
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
    val telescopeConfigs: NonEmptyList[TelescopeConfig]   =
      explicitTelescopeConfigs.getOrElse(defaultTelescopeConfigs)

    def isCustomized: Boolean =
      initialGrating =!= grating ||
        initialFilter =!= filter ||
        initialFpu =!= fpu ||
        initialCentralWavelength =!= centralWavelength ||
        explicitIfuAnalysis.exists(_ =!= defaultIfuAnalysis) ||
        explicitXBin.exists(_ =!= defaultXBin) ||
        explicitYBin.exists(_ =!= defaultYBin) ||
        explicitAmpReadMode.exists(_ =!= defaultAmpReadMode) ||
        explicitAmpGain.exists(_ =!= defaultAmpGain) ||
        explicitRoi.exists(_ =!= defaultRoi) ||
        explicitWavelengthDithers.exists(_ =!= defaultWavelengthDithers) ||
        explicitTelescopeConfigs.exists(_ =!= defaultTelescopeConfigs) ||
        acquisition.isCustomized

    def revertCustomizations: GmosNorthIfu =
      this.copy(
        grating = this.initialGrating,
        filter = this.initialFilter,
        fpu = this.initialFpu,
        centralWavelength = this.initialCentralWavelength,
        explicitIfuAnalysis = None,
        explicitXBin = None,
        explicitYBin = None,
        explicitAmpReadMode = None,
        explicitAmpGain = None,
        explicitRoi = None,
        explicitWavelengthDithers = None,
        explicitTelescopeConfigs = None,
        acquisition = acquisition.revertCustomizations
      )

  object GmosNorthIfu:
    case class Acquisition(
      defaultFilter:    GmosNorthFilter,
      explicitFilter:   Option[GmosNorthFilter],
      defaultRoi:       GmosIfuAcquisitionRoi,
      explicitRoi:      Option[GmosIfuAcquisitionRoi],
      exposureTimeMode: ExposureTimeMode
    ) derives Decoder,
          Eq:
      val filter: GmosNorthFilter           = explicitFilter.getOrElse(defaultFilter)
      val roi: GmosIfuAcquisitionRoi        = explicitRoi.getOrElse(defaultRoi)
      def isCustomized: Boolean             =
        explicitFilter.exists(_ =!= defaultFilter) || explicitRoi.exists(_ =!= defaultRoi)
      def revertCustomizations: Acquisition =
        this.copy(explicitFilter = None, explicitRoi = None)

    object Acquisition:
      val defaultFilter: Lens[Acquisition, GmosNorthFilter]             =
        Focus[Acquisition](_.defaultFilter)
      val explicitFilter: Lens[Acquisition, Option[GmosNorthFilter]]    =
        Focus[Acquisition](_.explicitFilter)
      val defaultRoi: Lens[Acquisition, GmosIfuAcquisitionRoi]          =
        Focus[Acquisition](_.defaultRoi)
      val explicitRoi: Lens[Acquisition, Option[GmosIfuAcquisitionRoi]] =
        Focus[Acquisition](_.explicitRoi)
      val exposureTimeMode: Lens[Acquisition, ExposureTimeMode]         =
        Focus[Acquisition](_.exposureTimeMode)

    given Decoder[GmosNorthIfu] = deriveDecoder

    val initialGrating: Lens[GmosNorthIfu, GmosNorthGrating]                                  =
      Focus[GmosNorthIfu](_.initialGrating)
    val grating: Lens[GmosNorthIfu, GmosNorthGrating]                                         =
      Focus[GmosNorthIfu](_.grating)
    val initialFilter: Lens[GmosNorthIfu, Option[GmosNorthFilter]]                            =
      Focus[GmosNorthIfu](_.initialFilter)
    val filter: Lens[GmosNorthIfu, Option[GmosNorthFilter]]                                   =
      Focus[GmosNorthIfu](_.filter)
    val initialFpu: Lens[GmosNorthIfu, GmosNorthIfuFpu]                                       =
      Focus[GmosNorthIfu](_.initialFpu)
    val fpu: Lens[GmosNorthIfu, GmosNorthIfuFpu]                                              =
      Focus[GmosNorthIfu](_.fpu)
    val initialCentralWavelength: Lens[GmosNorthIfu, CentralWavelength]                       =
      Focus[GmosNorthIfu](_.initialCentralWavelength)
    val centralWavelength: Lens[GmosNorthIfu, CentralWavelength]                              =
      Focus[GmosNorthIfu](_.centralWavelength)
    val defaultIfuAnalysis: Lens[GmosNorthIfu, GmosIfuAnalysis]                               =
      Focus[GmosNorthIfu](_.defaultIfuAnalysis)
    val explicitIfuAnalysis: Lens[GmosNorthIfu, Option[GmosIfuAnalysis]]                      =
      Focus[GmosNorthIfu](_.explicitIfuAnalysis)
    val defaultXBin: Lens[GmosNorthIfu, GmosXBinning]                                         =
      Focus[GmosNorthIfu](_.defaultXBin)
    val explicitXBin: Lens[GmosNorthIfu, Option[GmosXBinning]]                                =
      Focus[GmosNorthIfu](_.explicitXBin)
    val defaultYBin: Lens[GmosNorthIfu, GmosYBinning]                                         =
      Focus[GmosNorthIfu](_.defaultYBin)
    val explicitYBin: Lens[GmosNorthIfu, Option[GmosYBinning]]                                =
      Focus[GmosNorthIfu](_.explicitYBin)
    val defaultAmpReadMode: Lens[GmosNorthIfu, GmosAmpReadMode]                               =
      Focus[GmosNorthIfu](_.defaultAmpReadMode)
    val explicitAmpReadMode: Lens[GmosNorthIfu, Option[GmosAmpReadMode]]                      =
      Focus[GmosNorthIfu](_.explicitAmpReadMode)
    val defaultAmpGain: Lens[GmosNorthIfu, GmosAmpGain]                                       =
      Focus[GmosNorthIfu](_.defaultAmpGain)
    val explicitAmpGain: Lens[GmosNorthIfu, Option[GmosAmpGain]]                              =
      Focus[GmosNorthIfu](_.explicitAmpGain)
    val defaultRoi: Lens[GmosNorthIfu, GmosRoi]                                               =
      Focus[GmosNorthIfu](_.defaultRoi)
    val explicitRoi: Lens[GmosNorthIfu, Option[GmosRoi]]                                      =
      Focus[GmosNorthIfu](_.explicitRoi)
    val defaultWavelengthDithers: Lens[GmosNorthIfu, NonEmptyList[WavelengthDither]]          =
      Focus[GmosNorthIfu](_.defaultWavelengthDithers)
    val explicitWavelengthDithers: Lens[GmosNorthIfu, Option[NonEmptyList[WavelengthDither]]] =
      Focus[GmosNorthIfu](_.explicitWavelengthDithers)
    val defaultTelescopeConfigs: Lens[GmosNorthIfu, NonEmptyList[TelescopeConfig]]            =
      Focus[GmosNorthIfu](_.defaultTelescopeConfigs)
    val explicitTelescopeConfigs: Lens[GmosNorthIfu, Option[NonEmptyList[TelescopeConfig]]]   =
      Focus[GmosNorthIfu](_.explicitTelescopeConfigs)
    val exposureTimeMode: Lens[GmosNorthIfu, ExposureTimeMode]                                =
      Focus[GmosNorthIfu](_.exposureTimeMode)
    val acquisition: Lens[GmosNorthIfu, GmosNorthIfu.Acquisition]                             =
      Focus[GmosNorthIfu](_.acquisition)

  case class GmosSouthIfu(
    initialGrating:            GmosSouthGrating,
    grating:                   GmosSouthGrating,
    initialFilter:             Option[GmosSouthFilter],
    filter:                    Option[GmosSouthFilter],
    initialFpu:                GmosSouthIfuFpu,
    fpu:                       GmosSouthIfuFpu,
    initialCentralWavelength:  CentralWavelength,
    centralWavelength:         CentralWavelength,
    defaultIfuAnalysis:        GmosIfuAnalysis,
    explicitIfuAnalysis:       Option[GmosIfuAnalysis],
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
    defaultTelescopeConfigs:   NonEmptyList[TelescopeConfig],
    explicitTelescopeConfigs:  Option[NonEmptyList[TelescopeConfig]],
    exposureTimeMode:          ExposureTimeMode,
    acquisition:               GmosSouthIfu.Acquisition
  ) extends ObservingMode(Instrument.GmosSouth.some) derives Eq:
    val ifuAnalysis: GmosIfuAnalysis                      =
      explicitIfuAnalysis.getOrElse(defaultIfuAnalysis)
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
    val telescopeConfigs: NonEmptyList[TelescopeConfig]   =
      explicitTelescopeConfigs.getOrElse(defaultTelescopeConfigs)

    def isCustomized: Boolean =
      initialGrating =!= grating ||
        initialFilter =!= filter ||
        initialFpu =!= fpu ||
        initialCentralWavelength =!= centralWavelength ||
        explicitIfuAnalysis.exists(_ =!= defaultIfuAnalysis) ||
        explicitXBin.exists(_ =!= defaultXBin) ||
        explicitYBin.exists(_ =!= defaultYBin) ||
        explicitAmpReadMode.exists(_ =!= defaultAmpReadMode) ||
        explicitAmpGain.exists(_ =!= defaultAmpGain) ||
        explicitRoi.exists(_ =!= defaultRoi) ||
        explicitWavelengthDithers.exists(_ =!= defaultWavelengthDithers) ||
        explicitTelescopeConfigs.exists(_ =!= defaultTelescopeConfigs) ||
        acquisition.isCustomized

    def revertCustomizations: GmosSouthIfu =
      this.copy(
        grating = this.initialGrating,
        filter = this.initialFilter,
        fpu = this.initialFpu,
        centralWavelength = this.initialCentralWavelength,
        explicitIfuAnalysis = None,
        explicitXBin = None,
        explicitYBin = None,
        explicitAmpReadMode = None,
        explicitAmpGain = None,
        explicitRoi = None,
        explicitWavelengthDithers = None,
        explicitTelescopeConfigs = None,
        acquisition = acquisition.revertCustomizations
      )

  object GmosSouthIfu:
    case class Acquisition(
      defaultFilter:    GmosSouthFilter,
      explicitFilter:   Option[GmosSouthFilter],
      defaultRoi:       GmosIfuAcquisitionRoi,
      explicitRoi:      Option[GmosIfuAcquisitionRoi],
      exposureTimeMode: ExposureTimeMode
    ) derives Decoder,
          Eq:
      val filter: GmosSouthFilter           = explicitFilter.getOrElse(defaultFilter)
      val roi: GmosIfuAcquisitionRoi        = explicitRoi.getOrElse(defaultRoi)
      def isCustomized: Boolean             =
        explicitFilter.exists(_ =!= defaultFilter) || explicitRoi.exists(_ =!= defaultRoi)
      def revertCustomizations: Acquisition =
        this.copy(explicitFilter = None, explicitRoi = None)

    object Acquisition:
      val defaultFilter: Lens[Acquisition, GmosSouthFilter]             =
        Focus[Acquisition](_.defaultFilter)
      val explicitFilter: Lens[Acquisition, Option[GmosSouthFilter]]    =
        Focus[Acquisition](_.explicitFilter)
      val defaultRoi: Lens[Acquisition, GmosIfuAcquisitionRoi]          =
        Focus[Acquisition](_.defaultRoi)
      val explicitRoi: Lens[Acquisition, Option[GmosIfuAcquisitionRoi]] =
        Focus[Acquisition](_.explicitRoi)
      val exposureTimeMode: Lens[Acquisition, ExposureTimeMode]         =
        Focus[Acquisition](_.exposureTimeMode)

    given Decoder[GmosSouthIfu] = deriveDecoder

    val initialGrating: Lens[GmosSouthIfu, GmosSouthGrating]                                  =
      Focus[GmosSouthIfu](_.initialGrating)
    val grating: Lens[GmosSouthIfu, GmosSouthGrating]                                         =
      Focus[GmosSouthIfu](_.grating)
    val initialFilter: Lens[GmosSouthIfu, Option[GmosSouthFilter]]                            =
      Focus[GmosSouthIfu](_.initialFilter)
    val filter: Lens[GmosSouthIfu, Option[GmosSouthFilter]]                                   =
      Focus[GmosSouthIfu](_.filter)
    val initialFpu: Lens[GmosSouthIfu, GmosSouthIfuFpu]                                       =
      Focus[GmosSouthIfu](_.initialFpu)
    val fpu: Lens[GmosSouthIfu, GmosSouthIfuFpu]                                              =
      Focus[GmosSouthIfu](_.fpu)
    val initialCentralWavelength: Lens[GmosSouthIfu, CentralWavelength]                       =
      Focus[GmosSouthIfu](_.initialCentralWavelength)
    val centralWavelength: Lens[GmosSouthIfu, CentralWavelength]                              =
      Focus[GmosSouthIfu](_.centralWavelength)
    val defaultIfuAnalysis: Lens[GmosSouthIfu, GmosIfuAnalysis]                               =
      Focus[GmosSouthIfu](_.defaultIfuAnalysis)
    val explicitIfuAnalysis: Lens[GmosSouthIfu, Option[GmosIfuAnalysis]]                      =
      Focus[GmosSouthIfu](_.explicitIfuAnalysis)
    val defaultXBin: Lens[GmosSouthIfu, GmosXBinning]                                         =
      Focus[GmosSouthIfu](_.defaultXBin)
    val explicitXBin: Lens[GmosSouthIfu, Option[GmosXBinning]]                                =
      Focus[GmosSouthIfu](_.explicitXBin)
    val defaultYBin: Lens[GmosSouthIfu, GmosYBinning]                                         =
      Focus[GmosSouthIfu](_.defaultYBin)
    val explicitYBin: Lens[GmosSouthIfu, Option[GmosYBinning]]                                =
      Focus[GmosSouthIfu](_.explicitYBin)
    val defaultAmpReadMode: Lens[GmosSouthIfu, GmosAmpReadMode]                               =
      Focus[GmosSouthIfu](_.defaultAmpReadMode)
    val explicitAmpReadMode: Lens[GmosSouthIfu, Option[GmosAmpReadMode]]                      =
      Focus[GmosSouthIfu](_.explicitAmpReadMode)
    val defaultAmpGain: Lens[GmosSouthIfu, GmosAmpGain]                                       =
      Focus[GmosSouthIfu](_.defaultAmpGain)
    val explicitAmpGain: Lens[GmosSouthIfu, Option[GmosAmpGain]]                              =
      Focus[GmosSouthIfu](_.explicitAmpGain)
    val defaultRoi: Lens[GmosSouthIfu, GmosRoi]                                               =
      Focus[GmosSouthIfu](_.defaultRoi)
    val explicitRoi: Lens[GmosSouthIfu, Option[GmosRoi]]                                      =
      Focus[GmosSouthIfu](_.explicitRoi)
    val defaultWavelengthDithers: Lens[GmosSouthIfu, NonEmptyList[WavelengthDither]]          =
      Focus[GmosSouthIfu](_.defaultWavelengthDithers)
    val explicitWavelengthDithers: Lens[GmosSouthIfu, Option[NonEmptyList[WavelengthDither]]] =
      Focus[GmosSouthIfu](_.explicitWavelengthDithers)
    val defaultTelescopeConfigs: Lens[GmosSouthIfu, NonEmptyList[TelescopeConfig]]            =
      Focus[GmosSouthIfu](_.defaultTelescopeConfigs)
    val explicitTelescopeConfigs: Lens[GmosSouthIfu, Option[NonEmptyList[TelescopeConfig]]]   =
      Focus[GmosSouthIfu](_.explicitTelescopeConfigs)
    val exposureTimeMode: Lens[GmosSouthIfu, ExposureTimeMode]                                =
      Focus[GmosSouthIfu](_.exposureTimeMode)
    val acquisition: Lens[GmosSouthIfu, GmosSouthIfu.Acquisition]                             =
      Focus[GmosSouthIfu](_.acquisition)

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
    initialDisperser:         Flamingos2Disperser,
    disperser:                Flamingos2Disperser,
    initialFilter:            Flamingos2Filter,
    filter:                   Flamingos2Filter,
    initialFpu:               Flamingos2Fpu,
    fpu:                      Flamingos2Fpu,
    explicitReadMode:         Option[Flamingos2ReadMode],
    explicitReads:            Option[Flamingos2Reads],
    defaultDecker:            Flamingos2Decker,
    explicitDecker:           Option[Flamingos2Decker],
    defaultReadoutMode:       Flamingos2ReadoutMode,
    explicitReadoutMode:      Option[Flamingos2ReadoutMode],
    defaultTelescopeConfigs:  SlitTelescopeConfigs,
    explicitTelescopeConfigs: Option[SlitTelescopeConfigs],
    exposureTimeMode:         ExposureTimeMode,
    telluricType:             TelluricType,
    acquisition:              Flamingos2LongSlit.Acquisition
  ) extends ObservingMode(Instrument.Flamingos2.some) derives Eq:
    val decker: Flamingos2Decker               =
      explicitDecker.getOrElse(defaultDecker)
    val readoutMode: Flamingos2ReadoutMode     =
      explicitReadoutMode.getOrElse(defaultReadoutMode)
    val telescopeConfigs: SlitTelescopeConfigs =
      explicitTelescopeConfigs.getOrElse(defaultTelescopeConfigs)
    val readMode: Flamingos2ReadMode           =
      explicitReadMode.getOrElse(Flamingos2ReadMode.Bright)

    def isCustomized: Boolean =
      initialDisperser =!= disperser ||
        initialFilter =!= filter ||
        initialFpu =!= fpu ||
        explicitReadMode.isDefined ||
        explicitReads.isDefined ||
        explicitDecker.exists(_ =!= defaultDecker) ||
        explicitReadoutMode.exists(_ =!= defaultReadoutMode) ||
        explicitTelescopeConfigs.exists(_ =!= defaultTelescopeConfigs) ||
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
        explicitTelescopeConfigs = None,
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

    val initialDisperser: Lens[Flamingos2LongSlit, Flamingos2Disperser]                  =
      Focus[Flamingos2LongSlit](_.initialDisperser)
    val disperser: Lens[Flamingos2LongSlit, Flamingos2Disperser]                         =
      Focus[Flamingos2LongSlit](_.disperser)
    val initialFilter: Lens[Flamingos2LongSlit, Flamingos2Filter]                        =
      Focus[Flamingos2LongSlit](_.initialFilter)
    val filter: Lens[Flamingos2LongSlit, Flamingos2Filter]                               =
      Focus[Flamingos2LongSlit](_.filter)
    val initialFpu: Lens[Flamingos2LongSlit, Flamingos2Fpu]                              =
      Focus[Flamingos2LongSlit](_.initialFpu)
    val fpu: Lens[Flamingos2LongSlit, Flamingos2Fpu]                                     =
      Focus[Flamingos2LongSlit](_.fpu)
    val explicitReadMode: Lens[Flamingos2LongSlit, Option[Flamingos2ReadMode]]           =
      Focus[Flamingos2LongSlit](_.explicitReadMode)
    val explicitReads: Lens[Flamingos2LongSlit, Option[Flamingos2Reads]]                 =
      Focus[Flamingos2LongSlit](_.explicitReads)
    val defaultDecker: Lens[Flamingos2LongSlit, Flamingos2Decker]                        =
      Focus[Flamingos2LongSlit](_.defaultDecker)
    val explicitDecker: Lens[Flamingos2LongSlit, Option[Flamingos2Decker]]               =
      Focus[Flamingos2LongSlit](_.explicitDecker)
    val defaultReadoutMode: Lens[Flamingos2LongSlit, Flamingos2ReadoutMode]              =
      Focus[Flamingos2LongSlit](_.defaultReadoutMode)
    val explicitReadoutMode: Lens[Flamingos2LongSlit, Option[Flamingos2ReadoutMode]]     =
      Focus[Flamingos2LongSlit](_.explicitReadoutMode)
    val defaultTelescopeConfigs: Lens[Flamingos2LongSlit, SlitTelescopeConfigs]          =
      Focus[Flamingos2LongSlit](_.defaultTelescopeConfigs)
    val explicitTelescopeConfigs: Lens[Flamingos2LongSlit, Option[SlitTelescopeConfigs]] =
      Focus[Flamingos2LongSlit](_.explicitTelescopeConfigs)
    val exposureTimeMode: Lens[Flamingos2LongSlit, ExposureTimeMode]                     =
      Focus[Flamingos2LongSlit](_.exposureTimeMode)
    val telluricType: Lens[Flamingos2LongSlit, TelluricType]                             =
      Focus[Flamingos2LongSlit](_.telluricType)
    val acquisition: Lens[Flamingos2LongSlit, Flamingos2LongSlit.Acquisition]            =
      Focus[Flamingos2LongSlit](_.acquisition)

  case class Flamingos2CustomMask(
    attachmentId: Option[Attachment.Id],
    slitWidth:    Flamingos2CustomSlitWidth
  ) derives Decoder,
        Eq

  object Flamingos2CustomMask:
    val attachmentId: Lens[Flamingos2CustomMask, Option[Attachment.Id]]  =
      Focus[Flamingos2CustomMask](_.attachmentId)
    val slitWidth: Lens[Flamingos2CustomMask, Flamingos2CustomSlitWidth] =
      Focus[Flamingos2CustomMask](_.slitWidth)

  case class Flamingos2Mos(
    initialDisperser:         Flamingos2Disperser,
    disperser:                Flamingos2Disperser,
    initialFilter:            Flamingos2Filter,
    filter:                   Flamingos2Filter,
    initialSlitWidth:         Flamingos2CustomSlitWidth,
    customMask:               Flamingos2CustomMask,
    explicitReadMode:         Option[Flamingos2ReadMode],
    explicitReads:            Option[Flamingos2Reads],
    defaultDecker:            Flamingos2Decker,
    explicitDecker:           Option[Flamingos2Decker],
    defaultReadoutMode:       Flamingos2ReadoutMode,
    explicitReadoutMode:      Option[Flamingos2ReadoutMode],
    defaultTelescopeConfigs:  SlitTelescopeConfigs,
    explicitTelescopeConfigs: Option[SlitTelescopeConfigs],
    exposureTimeMode:         ExposureTimeMode,
    telluricType:             TelluricType,
    acquisition:              Flamingos2Mos.Acquisition
  ) extends ObservingMode(Instrument.Flamingos2.some) derives Eq:
    val decker: Flamingos2Decker               =
      explicitDecker.getOrElse(defaultDecker)
    val readoutMode: Flamingos2ReadoutMode     =
      explicitReadoutMode.getOrElse(defaultReadoutMode)
    val telescopeConfigs: SlitTelescopeConfigs =
      explicitTelescopeConfigs.getOrElse(defaultTelescopeConfigs)
    val readMode: Flamingos2ReadMode           =
      explicitReadMode.getOrElse(Flamingos2ReadMode.Bright)

    def isCustomized: Boolean =
      initialDisperser =!= disperser ||
        initialFilter =!= filter ||
        initialSlitWidth =!= customMask.slitWidth ||
        explicitReadMode.isDefined ||
        explicitReads.isDefined ||
        explicitDecker.exists(_ =!= defaultDecker) ||
        explicitReadoutMode.exists(_ =!= defaultReadoutMode) ||
        explicitTelescopeConfigs.exists(_ =!= defaultTelescopeConfigs) ||
        acquisition.isCustomized

    def revertCustomizations: Flamingos2Mos =
      this.copy(
        disperser = this.initialDisperser,
        filter = this.initialFilter,
        customMask = Flamingos2CustomMask.slitWidth.replace(this.initialSlitWidth)(this.customMask),
        explicitReadMode = None,
        explicitReads = None,
        explicitDecker = None,
        explicitReadoutMode = None,
        explicitTelescopeConfigs = None,
        acquisition = this.acquisition.revertCustomizations
      )

  object Flamingos2Mos:
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

    given Decoder[Flamingos2Mos] = deriveDecoder

    val initialDisperser: Lens[Flamingos2Mos, Flamingos2Disperser]                  =
      Focus[Flamingos2Mos](_.initialDisperser)
    val disperser: Lens[Flamingos2Mos, Flamingos2Disperser]                         =
      Focus[Flamingos2Mos](_.disperser)
    val initialFilter: Lens[Flamingos2Mos, Flamingos2Filter]                        =
      Focus[Flamingos2Mos](_.initialFilter)
    val filter: Lens[Flamingos2Mos, Flamingos2Filter]                               =
      Focus[Flamingos2Mos](_.filter)
    val initialSlitWidth: Lens[Flamingos2Mos, Flamingos2CustomSlitWidth]            =
      Focus[Flamingos2Mos](_.initialSlitWidth)
    val customMask: Lens[Flamingos2Mos, Flamingos2CustomMask]                       =
      Focus[Flamingos2Mos](_.customMask)
    val explicitReadMode: Lens[Flamingos2Mos, Option[Flamingos2ReadMode]]           =
      Focus[Flamingos2Mos](_.explicitReadMode)
    val explicitReads: Lens[Flamingos2Mos, Option[Flamingos2Reads]]                 =
      Focus[Flamingos2Mos](_.explicitReads)
    val defaultDecker: Lens[Flamingos2Mos, Flamingos2Decker]                        =
      Focus[Flamingos2Mos](_.defaultDecker)
    val explicitDecker: Lens[Flamingos2Mos, Option[Flamingos2Decker]]               =
      Focus[Flamingos2Mos](_.explicitDecker)
    val defaultReadoutMode: Lens[Flamingos2Mos, Flamingos2ReadoutMode]              =
      Focus[Flamingos2Mos](_.defaultReadoutMode)
    val explicitReadoutMode: Lens[Flamingos2Mos, Option[Flamingos2ReadoutMode]]     =
      Focus[Flamingos2Mos](_.explicitReadoutMode)
    val defaultTelescopeConfigs: Lens[Flamingos2Mos, SlitTelescopeConfigs]          =
      Focus[Flamingos2Mos](_.defaultTelescopeConfigs)
    val explicitTelescopeConfigs: Lens[Flamingos2Mos, Option[SlitTelescopeConfigs]] =
      Focus[Flamingos2Mos](_.explicitTelescopeConfigs)
    val exposureTimeMode: Lens[Flamingos2Mos, ExposureTimeMode]                     =
      Focus[Flamingos2Mos](_.exposureTimeMode)
    val telluricType: Lens[Flamingos2Mos, TelluricType]                             =
      Focus[Flamingos2Mos](_.telluricType)
    val acquisition: Lens[Flamingos2Mos, Acquisition]                               =
      Focus[Flamingos2Mos](_.acquisition)

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
    exposureTimeMode:         ExposureTimeMode,
    svc:                      Option[Igrins2LongSlit.Svc],
    defaultTelescopeConfigs:  SlitTelescopeConfigs,
    explicitTelescopeConfigs: Option[SlitTelescopeConfigs],
    telluricType:             TelluricType
  ) extends ObservingMode(Instrument.Igrins2.some) derives Eq:
    val telescopeConfigs: SlitTelescopeConfigs =
      explicitTelescopeConfigs.getOrElse(defaultTelescopeConfigs)

    def isCustomized: Boolean =
      explicitTelescopeConfigs.exists(_ =!= defaultTelescopeConfigs) ||
        svc.exists(_.isCustomized)

    def revertCustomizations: Igrins2LongSlit =
      this.copy(explicitTelescopeConfigs = None, svc = svc.map(_.revertCustomizations))

  object Igrins2LongSlit:
    given Decoder[Igrins2LongSlit] = deriveDecoder

    val exposureTimeMode: Lens[Igrins2LongSlit, ExposureTimeMode]                     =
      Focus[Igrins2LongSlit](_.exposureTimeMode)
    val svc: Lens[Igrins2LongSlit, Option[Svc]]                                       =
      Focus[Igrins2LongSlit](_.svc)
    val defaultTelescopeConfigs: Lens[Igrins2LongSlit, SlitTelescopeConfigs]          =
      Focus[Igrins2LongSlit](_.defaultTelescopeConfigs)
    val explicitTelescopeConfigs: Lens[Igrins2LongSlit, Option[SlitTelescopeConfigs]] =
      Focus[Igrins2LongSlit](_.explicitTelescopeConfigs)
    val telluricType: Lens[Igrins2LongSlit, TelluricType]                             =
      Focus[Igrins2LongSlit](_.telluricType)

    // Slit-Viewing Camera acquisition configuration.
    case class Svc(
      defaultExposure:          TimeSpan,
      explicitExposure:         Option[TimeSpan],
      defaultTelescopeConfigs:  NonEmptyList[TelescopeConfig],
      explicitTelescopeConfigs: Option[NonEmptyList[TelescopeConfig]]
    ) derives Decoder,
          Eq:
      val exposure: TimeSpan =
        explicitExposure.getOrElse(defaultExposure)

      val telescopeConfigs: NonEmptyList[TelescopeConfig] =
        explicitTelescopeConfigs.getOrElse(defaultTelescopeConfigs)

      def isCustomized: Boolean =
        explicitExposure.exists(_ =!= defaultExposure) ||
          explicitTelescopeConfigs.exists(_ =!= defaultTelescopeConfigs)

      def revertCustomizations: Svc =
        copy(explicitExposure = none, explicitTelescopeConfigs = none)

    object Svc:
      val defaultExposure: Lens[Svc, TimeSpan]                                       =
        Focus[Svc](_.defaultExposure)
      val explicitExposure: Lens[Svc, Option[TimeSpan]]                              =
        Focus[Svc](_.explicitExposure)
      val defaultTelescopeConfigs: Lens[Svc, NonEmptyList[TelescopeConfig]]          =
        Focus[Svc](_.defaultTelescopeConfigs)
      val explicitTelescopeConfigs: Lens[Svc, Option[NonEmptyList[TelescopeConfig]]] =
        Focus[Svc](_.explicitTelescopeConfigs)

      // Client-side defaults for IGRINS-2 SVC.
      val Default: Svc =
        Svc(
          defaultExposure = SvcDefaultExposure,
          explicitExposure = none,
          defaultTelescopeConfigs = SvcDefaultTelescopeConfigs,
          explicitTelescopeConfigs = none
        )

  case class GnirsImaging(
    initialFilters:    NonEmptyList[GnirsImaging.ImagingFilter],
    filters:           NonEmptyList[GnirsImaging.ImagingFilter],
    camera:            GnirsCamera,
    explicitReadMode:  Option[GnirsReadMode],
    defaultWellDepth:  GnirsWellDepth,
    explicitWellDepth: Option[GnirsWellDepth],
    variant:           ImagingVariant,
    acquisition:       GnirsImaging.Acquisition
  ) extends ObservingMode(Instrument.Gnirs.some) derives Eq:
    val wellDepth: GnirsWellDepth =
      explicitWellDepth.getOrElse(defaultWellDepth)

    def isCustomized: Boolean =
      initialFilters =!= filters ||
        explicitReadMode.isDefined ||
        explicitWellDepth.exists(_ =!= defaultWellDepth) ||
        acquisition.isCustomized

    def revertCustomizations: GnirsImaging =
      this.copy(
        filters = this.initialFilters,
        explicitReadMode = None,
        explicitWellDepth = None,
        acquisition = this.acquisition.revertCustomizations
      )

  object GnirsImaging:
    /**
     * One GNIRS imaging science configuration: a filter with the exposure time mode and coadds that
     * apply to it. Coadds are always 1 for a signal-to-noise exposure time mode, which does not
     * support them.
     */
    case class ImagingFilter(
      filter:           GnirsFilter,
      exposureTimeMode: ExposureTimeMode,
      coadds:           PosInt
    ) derives Decoder,
          Eq

    object ImagingFilter:
      val filter: Lens[ImagingFilter, GnirsFilter]                = Focus[ImagingFilter](_.filter)
      val exposureTimeMode: Lens[ImagingFilter, ExposureTimeMode] =
        Focus[ImagingFilter](_.exposureTimeMode)
      val coadds: Lens[ImagingFilter, PosInt]                     = Focus[ImagingFilter](_.coadds)

    /**
     * GNIRS imaging acquisition customization. `exposureTimeMode` is the effective mode: the
     * signal-to-noise value the ODB derives from the ITC brightness classification, unless
     * `explicitExposureTimeMode` overrides it. `coadds` always has a value (the ODB returns no
     * default for it) so it takes no part in isCustomized, but reverting resets it to 1, since a
     * derived mode is always signal-to-noise, which does not support coadds.
     */
    case class Acquisition(
      explicitAcquisitionMode:  Option[GnirsAcquisitionMode],
      explicitFilter:           Option[GnirsFilter],
      exposureTimeMode:         ExposureTimeMode,
      explicitExposureTimeMode: Option[ExposureTimeMode],
      coadds:                   PosInt
    ) derives Eq {
      def isCustomized: Boolean =
        explicitAcquisitionMode.isDefined ||
          explicitFilter.isDefined ||
          explicitExposureTimeMode.isDefined

      def revertCustomizations: Acquisition =
        this.copy(
          explicitAcquisitionMode = none,
          explicitFilter = none,
          explicitExposureTimeMode = none,
          coadds = 1.refined
        )
    }

    object Acquisition {
      // The API splits the acquisition mode across `explicitAcquisitionType` and
      // `skyOffset`; recombine them into the single GnirsAcquisitionMode we model. The
      // ODB sends a sky offset exactly when the type is FAINT, so the default is unused.
      given Decoder[Acquisition] = Decoder.instance: c =>
        for
          explicitAcquisitionType <-
            c.downField("explicitAcquisitionType").as[Option[GnirsAcquisitionType]]
          skyOffset               <- c.downField("skyOffset").as[Option[Offset]]
          explicitAcquisitionMode  =
            explicitAcquisitionType.map:
              GnirsAcquisitionMode.forTypeAndOffset(
                _,
                skyOffset.getOrElse(GnirsAcquisitionMode.Faint.DefaultImagingSkyOffset)
              )
          explicitFilter          <- c.downField("explicitFilter").as[Option[GnirsFilter]]
          exposureTimeMode        <- c.downField("exposureTimeMode").as[ExposureTimeMode]
          explicitEtm             <-
            c.downField("explicitExposureTimeMode").as[Option[ExposureTimeMode]]
          coadds                  <- c.downField("coadds").as[PosInt]
        yield Acquisition(
          explicitAcquisitionMode,
          explicitFilter,
          exposureTimeMode,
          explicitEtm,
          coadds
        )

      val explicitAcquisitionMode: Lens[Acquisition, Option[GnirsAcquisitionMode]] =
        Focus[Acquisition](_.explicitAcquisitionMode)
      val explicitFilter: Lens[Acquisition, Option[GnirsFilter]]                   =
        Focus[Acquisition](_.explicitFilter)
      val exposureTimeMode: Lens[Acquisition, ExposureTimeMode]                    =
        Focus[Acquisition](_.exposureTimeMode)
      val explicitExposureTimeMode: Lens[Acquisition, Option[ExposureTimeMode]]    =
        Focus[Acquisition](_.explicitExposureTimeMode)
      val coadds: Lens[Acquisition, PosInt]                                        =
        Focus[Acquisition](_.coadds)
    }

    given Decoder[GnirsImaging] = deriveDecoder

    val initialFilters: Lens[GnirsImaging, NonEmptyList[ImagingFilter]] =
      Focus[GnirsImaging](_.initialFilters)
    val filters: Lens[GnirsImaging, NonEmptyList[ImagingFilter]]        =
      Focus[GnirsImaging](_.filters)
    val camera: Lens[GnirsImaging, GnirsCamera]                         =
      Focus[GnirsImaging](_.camera)
    val explicitReadMode: Lens[GnirsImaging, Option[GnirsReadMode]]     =
      Focus[GnirsImaging](_.explicitReadMode)
    val defaultWellDepth: Lens[GnirsImaging, GnirsWellDepth]            =
      Focus[GnirsImaging](_.defaultWellDepth)
    val explicitWellDepth: Lens[GnirsImaging, Option[GnirsWellDepth]]   =
      Focus[GnirsImaging](_.explicitWellDepth)
    val variant: Lens[GnirsImaging, ImagingVariant]                     =
      Focus[GnirsImaging](_.variant)
    val acquisition: Lens[GnirsImaging, Acquisition]                    =
      Focus[GnirsImaging](_.acquisition)

  /**
   * One GNIRS spectroscopy science configuration: a central wavelength with the exposure time mode
   * and coadds that apply there. Shared by the long slit and the IFU.
   */
  case class GnirsCentralWavelengthConfig(
    centralWavelength: CentralWavelength,
    exposureTimeMode:  ExposureTimeMode,
    coadds:            PosInt
  ) derives Decoder,
        Eq

  object GnirsCentralWavelengthConfig:
    val centralWavelength: Lens[GnirsCentralWavelengthConfig, CentralWavelength] =
      Focus[GnirsCentralWavelengthConfig](_.centralWavelength)
    val exposureTimeMode: Lens[GnirsCentralWavelengthConfig, ExposureTimeMode]   =
      Focus[GnirsCentralWavelengthConfig](_.exposureTimeMode)
    val coadds: Lens[GnirsCentralWavelengthConfig, PosInt]                       =
      Focus[GnirsCentralWavelengthConfig](_.coadds)

  /**
   * GNIRS spectroscopy acquisition customization, shared by the long slit and the IFU.
   * `exposureTimeMode` is the effective mode: the signal-to-noise value the ODB derives from the
   * ITC brightness classification, unless `explicitExposureTimeMode` overrides it. `coadds` always
   * has a value (the ODB returns no default for it) so it takes no part in isCustomized, but
   * reverting resets it to 1, since a derived mode is always signal-to-noise, which does not
   * support coadds.
   */
  case class GnirsSpectroscopyAcquisition(
    explicitAcquisitionMode:  Option[GnirsAcquisitionMode],
    explicitFilter:           Option[GnirsFilter],
    exposureTimeMode:         ExposureTimeMode,
    explicitExposureTimeMode: Option[ExposureTimeMode],
    coadds:                   PosInt
  ) derives Eq:
    def isCustomized: Boolean =
      explicitAcquisitionMode.isDefined ||
        explicitFilter.isDefined ||
        explicitExposureTimeMode.isDefined

    def revertCustomizations: GnirsSpectroscopyAcquisition =
      copy(
        explicitAcquisitionMode = none,
        explicitFilter = none,
        explicitExposureTimeMode = none,
        coadds = 1.refined
      )

  object GnirsSpectroscopyAcquisition:
    // The ODB sends a sky offset exactly when the type is FAINT, so the default is unused.
    given Decoder[GnirsSpectroscopyAcquisition] = Decoder.instance: c =>
      for
        explicitAcquisitionType <-
          c.downField("explicitAcquisitionType").as[Option[GnirsAcquisitionType]]
        skyOffset               <- c.downField("skyOffset").as[Option[Offset]]
        explicitAcquisitionMode  =
          explicitAcquisitionType.map:
            GnirsAcquisitionMode.forTypeAndOffset(
              _,
              skyOffset.getOrElse(GnirsAcquisitionMode.Faint.DefaultSlitSkyOffset)
            )
        explicitFilter          <- c.downField("explicitFilter").as[Option[GnirsFilter]]
        exposureTimeMode        <- c.downField("exposureTimeMode").as[ExposureTimeMode]
        explicitEtm             <-
          c.downField("explicitExposureTimeMode").as[Option[ExposureTimeMode]]
        coadds                  <- c.downField("coadds").as[PosInt]
      yield GnirsSpectroscopyAcquisition(
        explicitAcquisitionMode,
        explicitFilter,
        exposureTimeMode,
        explicitEtm,
        coadds
      )

    val explicitAcquisitionMode: Lens[GnirsSpectroscopyAcquisition, Option[GnirsAcquisitionMode]] =
      Focus[GnirsSpectroscopyAcquisition](_.explicitAcquisitionMode)
    val explicitFilter: Lens[GnirsSpectroscopyAcquisition, Option[GnirsFilter]]                   =
      Focus[GnirsSpectroscopyAcquisition](_.explicitFilter)
    val exposureTimeMode: Lens[GnirsSpectroscopyAcquisition, ExposureTimeMode]                    =
      Focus[GnirsSpectroscopyAcquisition](_.exposureTimeMode)
    val explicitExposureTimeMode: Lens[GnirsSpectroscopyAcquisition, Option[ExposureTimeMode]]    =
      Focus[GnirsSpectroscopyAcquisition](_.explicitExposureTimeMode)
    val coadds: Lens[GnirsSpectroscopyAcquisition, PosInt]                                        =
      Focus[GnirsSpectroscopyAcquisition](_.coadds)

  case class GnirsLongSlit(
    initialGrating:            GnirsGrating,
    grating:                   GnirsGrating,
    initialFilter:             GnirsFilter,
    filter:                    GnirsFilter,
    initialFpu:                GnirsFpuSlit,
    fpu:                       GnirsFpuSlit,
    defaultTelescopeConfigs:   SlitTelescopeConfigs,
    explicitTelescopeConfigs:  Option[SlitTelescopeConfigs],
    initialPrism:              GnirsPrism,
    prism:                     GnirsPrism,
    initialCamera:             GnirsCamera,
    camera:                    GnirsCamera,
    initialCentralWavelengths: NonEmptyList[GnirsCentralWavelengthConfig],
    centralWavelengths:        NonEmptyList[GnirsCentralWavelengthConfig],
    defaultDecker:             GnirsDecker,
    explicitDecker:            Option[GnirsDecker],
    explicitReadMode:          Option[GnirsReadMode],
    defaultWellDepth:          GnirsWellDepth,
    explicitWellDepth:         Option[GnirsWellDepth],
    explicitFocusMotorSteps:   Option[GnirsFocusMotorStepsValue],
    telluricType:              TelluricType,
    acquisition:               GnirsSpectroscopyAcquisition
  ) extends ObservingMode(Instrument.Gnirs.some) derives Eq:
    val decker: GnirsDecker       =
      explicitDecker.getOrElse(defaultDecker)
    val wellDepth: GnirsWellDepth =
      explicitWellDepth.getOrElse(defaultWellDepth)

    def telescopeConfigs: SlitTelescopeConfigs =
      explicitTelescopeConfigs.getOrElse(defaultTelescopeConfigs)

    def isCustomized: Boolean =
      initialFpu =!= fpu ||
        explicitTelescopeConfigs.exists(_ =!= defaultTelescopeConfigs) ||
        initialGrating =!= grating ||
        initialFilter =!= filter ||
        initialPrism =!= prism ||
        initialCamera =!= camera ||
        initialCentralWavelengths =!= centralWavelengths ||
        explicitDecker.exists(_ =!= defaultDecker) ||
        explicitReadMode.isDefined ||
        explicitWellDepth.exists(_ =!= defaultWellDepth) ||
        explicitFocusMotorSteps.isDefined ||
        acquisition.isCustomized

    def revertCustomizations: GnirsLongSlit =
      copy(
        fpu = initialFpu,
        explicitTelescopeConfigs = None,
        grating = initialGrating,
        filter = initialFilter,
        prism = initialPrism,
        camera = initialCamera,
        centralWavelengths = initialCentralWavelengths,
        explicitDecker = None,
        explicitReadMode = None,
        explicitWellDepth = None,
        explicitFocusMotorSteps = None,
        acquisition = acquisition.revertCustomizations
      )

  object GnirsLongSlit:
    given Decoder[GnirsLongSlit] = deriveDecoder

    val initialGrating: Lens[GnirsLongSlit, GnirsGrating]                                          =
      Focus[GnirsLongSlit](_.initialGrating)
    val grating: Lens[GnirsLongSlit, GnirsGrating]                                                 =
      Focus[GnirsLongSlit](_.grating)
    val initialFilter: Lens[GnirsLongSlit, GnirsFilter]                                            =
      Focus[GnirsLongSlit](_.initialFilter)
    val filter: Lens[GnirsLongSlit, GnirsFilter]                                                   =
      Focus[GnirsLongSlit](_.filter)
    val initialFpu: Lens[GnirsLongSlit, GnirsFpuSlit]                                              =
      Focus[GnirsLongSlit](_.initialFpu)
    val fpu: Lens[GnirsLongSlit, GnirsFpuSlit]                                                     =
      Focus[GnirsLongSlit](_.fpu)
    val defaultTelescopeConfigs: Lens[GnirsLongSlit, SlitTelescopeConfigs]                         =
      Focus[GnirsLongSlit](_.defaultTelescopeConfigs)
    val explicitTelescopeConfigs: Lens[GnirsLongSlit, Option[SlitTelescopeConfigs]]                =
      Focus[GnirsLongSlit](_.explicitTelescopeConfigs)
    val initialPrism: Lens[GnirsLongSlit, GnirsPrism]                                              =
      Focus[GnirsLongSlit](_.initialPrism)
    val prism: Lens[GnirsLongSlit, GnirsPrism]                                                     =
      Focus[GnirsLongSlit](_.prism)
    val initialCamera: Lens[GnirsLongSlit, GnirsCamera]                                            =
      Focus[GnirsLongSlit](_.initialCamera)
    val camera: Lens[GnirsLongSlit, GnirsCamera]                                                   =
      Focus[GnirsLongSlit](_.camera)
    val initialCentralWavelengths: Lens[GnirsLongSlit, NonEmptyList[GnirsCentralWavelengthConfig]] =
      Focus[GnirsLongSlit](_.initialCentralWavelengths)
    val centralWavelengths: Lens[GnirsLongSlit, NonEmptyList[GnirsCentralWavelengthConfig]]        =
      Focus[GnirsLongSlit](_.centralWavelengths)
    val defaultDecker: Lens[GnirsLongSlit, GnirsDecker]                                            =
      Focus[GnirsLongSlit](_.defaultDecker)
    val explicitDecker: Lens[GnirsLongSlit, Option[GnirsDecker]]                                   =
      Focus[GnirsLongSlit](_.explicitDecker)
    val explicitReadMode: Lens[GnirsLongSlit, Option[GnirsReadMode]]                               =
      Focus[GnirsLongSlit](_.explicitReadMode)
    val defaultWellDepth: Lens[GnirsLongSlit, GnirsWellDepth]                                      =
      Focus[GnirsLongSlit](_.defaultWellDepth)
    val explicitWellDepth: Lens[GnirsLongSlit, Option[GnirsWellDepth]]                             =
      Focus[GnirsLongSlit](_.explicitWellDepth)
    val explicitFocusMotorSteps: Lens[GnirsLongSlit, Option[GnirsFocusMotorStepsValue]]            =
      Focus[GnirsLongSlit](_.explicitFocusMotorSteps)
    val telluricType: Lens[GnirsLongSlit, TelluricType]                                            =
      Focus[GnirsLongSlit](_.telluricType)
    val acquisition: Lens[GnirsLongSlit, GnirsSpectroscopyAcquisition]                             =
      Focus[GnirsLongSlit](_.acquisition)

  case class GnirsIfu(
    initialGrating:            GnirsGrating,
    grating:                   GnirsGrating,
    initialFilter:             GnirsFilter,
    filter:                    GnirsFilter,
    initialFpu:                GnirsFpuIfu,
    fpu:                       GnirsFpuIfu,
    telescopeConfigs:          NonEmptyList[TelescopeConfig],
    initialPrism:              GnirsPrism,
    prism:                     GnirsPrism,
    initialCamera:             GnirsCamera,
    camera:                    GnirsCamera,
    initialCentralWavelengths: NonEmptyList[GnirsCentralWavelengthConfig],
    centralWavelengths:        NonEmptyList[GnirsCentralWavelengthConfig],
    defaultDecker:             GnirsDecker,
    explicitDecker:            Option[GnirsDecker],
    explicitReadMode:          Option[GnirsReadMode],
    defaultWellDepth:          GnirsWellDepth,
    explicitWellDepth:         Option[GnirsWellDepth],
    explicitFocusMotorSteps:   Option[GnirsFocusMotorStepsValue],
    telluricType:              TelluricType,
    acquisition:               GnirsSpectroscopyAcquisition
  ) extends ObservingMode(Instrument.Gnirs.some) derives Eq:
    val decker: GnirsDecker       =
      explicitDecker.getOrElse(defaultDecker)
    val wellDepth: GnirsWellDepth =
      explicitWellDepth.getOrElse(defaultWellDepth)

    def isCustomized: Boolean =
      initialFpu =!= fpu ||
        // No server default; the canonical seed is the FPU's first preset.
        telescopeConfigs =!= defaultIfuTelescopeConfigs(fpu) ||
        initialGrating =!= grating ||
        initialFilter =!= filter ||
        initialPrism =!= prism ||
        initialCamera =!= camera ||
        initialCentralWavelengths =!= centralWavelengths ||
        explicitDecker.exists(_ =!= defaultDecker) ||
        explicitReadMode.isDefined ||
        explicitWellDepth.exists(_ =!= defaultWellDepth) ||
        explicitFocusMotorSteps.isDefined ||
        acquisition.isCustomized

    def revertCustomizations: GnirsIfu =
      copy(
        fpu = initialFpu,
        telescopeConfigs = defaultIfuTelescopeConfigs(initialFpu),
        grating = initialGrating,
        filter = initialFilter,
        prism = initialPrism,
        camera = initialCamera,
        centralWavelengths = initialCentralWavelengths,
        explicitDecker = None,
        explicitReadMode = None,
        explicitWellDepth = None,
        explicitFocusMotorSteps = None,
        acquisition = acquisition.revertCustomizations
      )

  object GnirsIfu:
    given Decoder[GnirsIfu] = deriveDecoder

    val initialGrating: Lens[GnirsIfu, GnirsGrating]                                          =
      Focus[GnirsIfu](_.initialGrating)
    val grating: Lens[GnirsIfu, GnirsGrating]                                                 =
      Focus[GnirsIfu](_.grating)
    val initialFilter: Lens[GnirsIfu, GnirsFilter]                                            =
      Focus[GnirsIfu](_.initialFilter)
    val filter: Lens[GnirsIfu, GnirsFilter]                                                   =
      Focus[GnirsIfu](_.filter)
    val initialFpu: Lens[GnirsIfu, GnirsFpuIfu]                                               =
      Focus[GnirsIfu](_.initialFpu)
    val fpu: Lens[GnirsIfu, GnirsFpuIfu]                                                      =
      Focus[GnirsIfu](_.fpu)
    val telescopeConfigs: Lens[GnirsIfu, NonEmptyList[TelescopeConfig]]                       =
      Focus[GnirsIfu](_.telescopeConfigs)
    val initialPrism: Lens[GnirsIfu, GnirsPrism]                                              =
      Focus[GnirsIfu](_.initialPrism)
    val prism: Lens[GnirsIfu, GnirsPrism]                                                     =
      Focus[GnirsIfu](_.prism)
    val initialCamera: Lens[GnirsIfu, GnirsCamera]                                            =
      Focus[GnirsIfu](_.initialCamera)
    val camera: Lens[GnirsIfu, GnirsCamera]                                                   =
      Focus[GnirsIfu](_.camera)
    val initialCentralWavelengths: Lens[GnirsIfu, NonEmptyList[GnirsCentralWavelengthConfig]] =
      Focus[GnirsIfu](_.initialCentralWavelengths)
    val centralWavelengths: Lens[GnirsIfu, NonEmptyList[GnirsCentralWavelengthConfig]]        =
      Focus[GnirsIfu](_.centralWavelengths)
    val defaultDecker: Lens[GnirsIfu, GnirsDecker]                                            =
      Focus[GnirsIfu](_.defaultDecker)
    val explicitDecker: Lens[GnirsIfu, Option[GnirsDecker]]                                   =
      Focus[GnirsIfu](_.explicitDecker)
    val explicitReadMode: Lens[GnirsIfu, Option[GnirsReadMode]]                               =
      Focus[GnirsIfu](_.explicitReadMode)
    val defaultWellDepth: Lens[GnirsIfu, GnirsWellDepth]                                      =
      Focus[GnirsIfu](_.defaultWellDepth)
    val explicitWellDepth: Lens[GnirsIfu, Option[GnirsWellDepth]]                             =
      Focus[GnirsIfu](_.explicitWellDepth)
    val explicitFocusMotorSteps: Lens[GnirsIfu, Option[GnirsFocusMotorStepsValue]]            =
      Focus[GnirsIfu](_.explicitFocusMotorSteps)
    val telluricType: Lens[GnirsIfu, TelluricType]                                            =
      Focus[GnirsIfu](_.telluricType)
    val acquisition: Lens[GnirsIfu, GnirsSpectroscopyAcquisition]                             =
      Focus[GnirsIfu](_.acquisition)

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

  val gmosNorthMos: Prism[ObservingMode, GmosNorthMos] =
    GenPrism[ObservingMode, GmosNorthMos]

  val gmosSouthMos: Prism[ObservingMode, GmosSouthMos] =
    GenPrism[ObservingMode, GmosSouthMos]

  val gmosNorthIfu: Prism[ObservingMode, GmosNorthIfu] =
    GenPrism[ObservingMode, GmosNorthIfu]

  val gmosSouthIfu: Prism[ObservingMode, GmosSouthIfu] =
    GenPrism[ObservingMode, GmosSouthIfu]

  val flamingos2Mos: Prism[ObservingMode, Flamingos2Mos] =
    GenPrism[ObservingMode, Flamingos2Mos]

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

  val gnirsLongSlit: Prism[ObservingMode, GnirsLongSlit] =
    GenPrism[ObservingMode, GnirsLongSlit]

  val gnirsIfu: Prism[ObservingMode, GnirsIfu] =
    GenPrism[ObservingMode, GnirsIfu]

  val ghostIfu: Prism[ObservingMode, GhostIfu] =
    GenPrism[ObservingMode, GhostIfu]

  val visitor: Prism[ObservingMode, Visitor] =
    GenPrism[ObservingMode, Visitor]

  val keckExchange: Prism[ObservingMode, KeckExchange] =
    GenPrism[ObservingMode, KeckExchange]

  val subaruExchange: Prism[ObservingMode, SubaruExchange] =
    GenPrism[ObservingMode, SubaruExchange]

  // Present only for the modes that generate telluric calibrations.
  val telluricType: Optional[ObservingMode, TelluricType] =
    Optional[ObservingMode, TelluricType] {
      case m: Flamingos2LongSlit => m.telluricType.some
      case m: Flamingos2Mos      => m.telluricType.some
      case m: Igrins2LongSlit    => m.telluricType.some
      case m: GnirsLongSlit      => m.telluricType.some
      case m: GnirsIfu           => m.telluricType.some
      case _                     => none
    } { tt =>
      {
        case m: Flamingos2LongSlit => m.copy(telluricType = tt)
        case m: Flamingos2Mos      => m.copy(telluricType = tt)
        case m: Igrins2LongSlit    => m.copy(telluricType = tt)
        case m: GnirsLongSlit      => m.copy(telluricType = tt)
        case m: GnirsIfu           => m.copy(telluricType = tt)
        case other                 => other
      }
    }

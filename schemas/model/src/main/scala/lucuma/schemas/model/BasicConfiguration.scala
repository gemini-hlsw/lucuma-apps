// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.generic.semiauto.*
import io.circe.refined.given
import lucuma.core.enums.*
import lucuma.core.geom.visitors.*
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Target
import lucuma.core.model.probes
import lucuma.core.model.sequence.ghost.CentralWavelength as GhostCentralWavelength
import lucuma.core.model.sequence.igrins2.CentralWavelength as Igrins2CentralWavelength
import lucuma.core.model.sequence.visitors.AlopekeCentralWavelength
import lucuma.core.model.sequence.visitors.MaroonXCentralWavelength
import lucuma.core.model.sequence.visitors.ZorroCentralWavelength
import lucuma.itc.ItcGhostDetector
import lucuma.odb.json.angle.decoder.given
import lucuma.odb.json.wavelength.decoder.given
import lucuma.schemas.decoders.given
import monocle.Prism
import monocle.macros.GenPrism

// For when we don't need the whole observing mode, such as in the ObsSummary.
// For example, for the ags and visualization.
// This is also used to create the configuration.
// It can be created from either a ConfigSelection or an ObservingMode.
// We could almost just use an ItcInstrumentConfig directly instead, except that
// to create the configuration we need to allow muiltiple filters for imaging,
// but the ItcInstrumentConfig only allows one.
sealed abstract class BasicConfiguration(val instrument: Instrument)
    extends Product
    with Serializable derives Eq:
  def gmosFpuAlternative: Option[Either[GmosNorthFpu, GmosSouthFpu]] = this match
    case BasicConfiguration.GmosNorthLongSlit(_, _, fpu, _) => fpu.asLeft.some
    case BasicConfiguration.GmosSouthLongSlit(_, _, fpu, _) => fpu.asRight.some
    case _                                                  => none

  def f2Fpu: Option[Flamingos2Fpu] = this match
    case BasicConfiguration.Flamingos2LongSlit(fpu = fpu) => fpu.some
    case _                                                => none

  def siteFor: Site = this match
    case _: BasicConfiguration.Flamingos2Imaging  => Site.GS
    case _: BasicConfiguration.Flamingos2LongSlit => Site.GS
    case _: BasicConfiguration.GhostIfu           => Site.GS
    case _: BasicConfiguration.GmosNorthImaging   => Site.GN
    case _: BasicConfiguration.GmosNorthLongSlit  => Site.GN
    case _: BasicConfiguration.GmosSouthImaging   => Site.GS
    case _: BasicConfiguration.GmosSouthLongSlit  => Site.GS
    case _: BasicConfiguration.GnirsLongSlit      => Site.GN
    case BasicConfiguration.Igrins2LongSlit       => Site.GN
    case v: BasicConfiguration.Visitor            => v.site

  def obsModeType: ObservingModeType = this match
    case _: BasicConfiguration.Flamingos2Imaging  => ObservingModeType.Flamingos2Imaging
    case _: BasicConfiguration.Flamingos2LongSlit => ObservingModeType.Flamingos2LongSlit
    case _: BasicConfiguration.GhostIfu           => ObservingModeType.GhostIfu
    case _: BasicConfiguration.GmosNorthImaging   => ObservingModeType.GmosNorthImaging
    case _: BasicConfiguration.GmosNorthLongSlit  => ObservingModeType.GmosNorthLongSlit
    case _: BasicConfiguration.GmosSouthImaging   => ObservingModeType.GmosSouthImaging
    case _: BasicConfiguration.GmosSouthLongSlit  => ObservingModeType.GmosSouthLongSlit
    case _: BasicConfiguration.GnirsLongSlit      => ObservingModeType.GnirsLongSlit
    case BasicConfiguration.Igrins2LongSlit       => ObservingModeType.Igrins2LongSlit
    case v: BasicConfiguration.Visitor            => v.mode

  def centralWv: Option[CentralWavelength] = this match
    case BasicConfiguration.GmosNorthLongSlit(centralWavelength = cw) =>
      cw.some
    case BasicConfiguration.GmosSouthLongSlit(centralWavelength = cw) =>
      cw.some
    case BasicConfiguration.Flamingos2LongSlit(filter = filter)       =>
      CentralWavelength(filter.wavelength).some
    case BasicConfiguration.Igrins2LongSlit                           =>
      CentralWavelength(Igrins2CentralWavelength).some
    case BasicConfiguration.GhostIfu(_, _, _, _, _)                   =>
      CentralWavelength(GhostCentralWavelength).some
    case v: BasicConfiguration.Visitor                                =>
      v.centralWavelength.some
    case _                                                            =>
      none

  def agsWavelength: AGSWavelength = this match
    case BasicConfiguration.GmosNorthLongSlit(centralWavelength = cw) =>
      AGSWavelength(cw.value)
    case BasicConfiguration.GmosSouthLongSlit(centralWavelength = cw) =>
      AGSWavelength(cw.value)
    case BasicConfiguration.GmosNorthImaging(filters)                 =>
      AGSWavelength(filters.maximumBy(_.wavelength).wavelength)
    case BasicConfiguration.GmosSouthImaging(filters)                 =>
      AGSWavelength(filters.maximumBy(_.wavelength).wavelength)
    case BasicConfiguration.Flamingos2Imaging(filters)                =>
      AGSWavelength(filters.maximumBy(_.wavelength).wavelength)
    case BasicConfiguration.Flamingos2LongSlit(filter = filter)       =>
      AGSWavelength(filter.wavelength)
    case BasicConfiguration.Igrins2LongSlit                           =>
      AGSWavelength(Igrins2CentralWavelength)
    case BasicConfiguration.GhostIfu(_, _, _, _, _)                   =>
      AGSWavelength(GhostCentralWavelength)
    case gnirs: BasicConfiguration.GnirsLongSlit                      =>
      AGSWavelength(gnirs.centralWavelength)
    case v: BasicConfiguration.Visitor                                =>
      AGSWavelength(v.centralWavelength.value)

  def conditionsWavelength: Wavelength = this match
    case BasicConfiguration.GmosNorthLongSlit(centralWavelength = cw) =>
      cw.value
    case BasicConfiguration.GmosSouthLongSlit(centralWavelength = cw) =>
      cw.value
    case BasicConfiguration.GmosNorthImaging(filters)                 =>
      filters.minimumBy(_.wavelength).wavelength
    case BasicConfiguration.GmosSouthImaging(filters)                 =>
      filters.minimumBy(_.wavelength).wavelength
    case BasicConfiguration.Flamingos2Imaging(filters)                =>
      filters.minimumBy(_.wavelength).wavelength
    case BasicConfiguration.Flamingos2LongSlit(filter = filter)       =>
      filter.wavelength
    case BasicConfiguration.Igrins2LongSlit                           =>
      Igrins2CentralWavelength
    case BasicConfiguration.GhostIfu(_, _, _, _, _)                   =>
      GhostCentralWavelength
    case gnirs: BasicConfiguration.GnirsLongSlit                      =>
      gnirs.centralWavelength
    case v: BasicConfiguration.Visitor                                =>
      v.centralWavelength.value

  // Let's always return a fallback for viz
  def guideProbe(trackType: Option[TrackType]): GuideProbe =
    trackType.flatMap(probes.guideProbe(obsModeType, _)).getOrElse(fallBackGuideProbe)

  def targetLabelsMap(scienceTargets: List[TargetWithId]): Map[Target.Id, String] =
    this match
      case BasicConfiguration.GhostIfu(resolutionMode = GhostResolutionMode.Standard) =>
        scienceTargets.zip(List("SR-IFU1", "SR-IFU2")).map((t, l) => t.id -> l).toMap
      case BasicConfiguration.GhostIfu(resolutionMode = GhostResolutionMode.High)     =>
        scienceTargets.zip(List("HR-IFU1")).map((t, l) => t.id -> l).toMap
      case _                                                                          => Map.empty

  private def fallBackGuideProbe = this match
    case BasicConfiguration.GmosNorthLongSlit(_, _, _, _) |
        BasicConfiguration.GmosSouthLongSlit(_, _, _, _) | BasicConfiguration.GmosNorthImaging(_) |
        BasicConfiguration.GmosSouthImaging(_) =>
      GuideProbe.GmosOIWFS
    case BasicConfiguration.Flamingos2Imaging(_)         => GuideProbe.Flamingos2OIWFS
    case BasicConfiguration.Flamingos2LongSlit(_, _, _)  => GuideProbe.Flamingos2OIWFS
    case BasicConfiguration.Igrins2LongSlit              => GuideProbe.PWFS2
    case BasicConfiguration.GhostIfu(_, _, _, _, _)      => GuideProbe.PWFS2
    case BasicConfiguration.GnirsLongSlit(_, _, _, _, _) => GuideProbe.PWFS2
    case BasicConfiguration.Visitor(_, _, _)             => GuideProbe.PWFS2

object BasicConfiguration:
  given Decoder[BasicConfiguration] =
    Decoder
      .instance: c =>
        c.downField("gmosNorthLongSlit")
          .as[GmosNorthLongSlit]
          .orElse:
            c.downField("gmosSouthLongSlit")
              .as[GmosSouthLongSlit]
              .orElse:
                c.downField("gmosNorthImaging")
                  .as[GmosNorthImaging]
                  .orElse:
                    c.downField("gmosSouthImaging")
                      .as[GmosSouthImaging]
                      .orElse:
                        c.downField("flamingos2Imaging")
                          .as[Flamingos2Imaging]
                          .orElse:
                            c.downField("flamingos2LongSlit")
                              .as[Flamingos2LongSlit]
                              .orElse:
                                c.downField("igrins2LongSlit")
                                  .as[Igrins2LongSlit.type]
                                  .orElse:
                                    c.downField("gnirsLongSlit")
                                      .as[GnirsLongSlit]
                                      .orElse:
                                        c.downField("ghostIfu")
                                          .as[GhostIfu]
                                          .orElse:
                                            c.downField("visitor")
                                              .as[Visitor]
                                              .orElse:
                                                DecodingFailure(
                                                  "Could not decode BasicConfiguration",
                                                  c.history
                                                ).asLeft

  case class GmosNorthLongSlit(
    grating:           GmosNorthGrating,
    filter:            Option[GmosNorthFilter],
    fpu:               GmosNorthFpu,
    centralWavelength: CentralWavelength
  ) extends BasicConfiguration(Instrument.GmosNorth) derives Eq

  object GmosNorthLongSlit:
    given Decoder[GmosNorthLongSlit] = deriveDecoder

  case class GmosSouthLongSlit(
    grating:           GmosSouthGrating,
    filter:            Option[GmosSouthFilter],
    fpu:               GmosSouthFpu,
    centralWavelength: CentralWavelength
  ) extends BasicConfiguration(Instrument.GmosSouth) derives Eq

  object GmosSouthLongSlit:
    given Decoder[GmosSouthLongSlit] = deriveDecoder

  case class GmosNorthImaging(
    filters: NonEmptyList[GmosNorthFilter]
  ) extends BasicConfiguration(Instrument.GmosNorth) derives Eq

  object GmosNorthImaging:
    given Decoder[GmosNorthImaging] = deriveDecoder

  case class GmosSouthImaging(
    filters: NonEmptyList[GmosSouthFilter]
  ) extends BasicConfiguration(Instrument.GmosSouth) derives Eq

  object GmosSouthImaging:
    given Decoder[GmosSouthImaging] = deriveDecoder

  case class Flamingos2LongSlit(
    disperser: Flamingos2Disperser,
    filter:    Flamingos2Filter,
    fpu:       Flamingos2Fpu
  ) extends BasicConfiguration(Instrument.Flamingos2) derives Eq

  object Flamingos2LongSlit:
    given Decoder[Flamingos2LongSlit] = deriveDecoder

  case class Flamingos2Imaging(
    filters: NonEmptyList[Flamingos2Filter]
  ) extends BasicConfiguration(Instrument.Flamingos2) derives Eq

  object Flamingos2Imaging:
    given Decoder[Flamingos2Imaging] = deriveDecoder

  case object Igrins2LongSlit extends BasicConfiguration(Instrument.Igrins2) derives Eq:
    given Decoder[Igrins2LongSlit.type] = Decoder.const(Igrins2LongSlit)

  case class GnirsLongSlit(
    filter:  GnirsFilter,
    fpu:     GnirsFpuSlit,
    prism:   GnirsPrism,
    grating: GnirsGrating,
    camera:  GnirsCamera
  ) extends BasicConfiguration(Instrument.Gnirs) derives Eq:
    lazy val centralWavelength: Wavelength = filter.centralWavelength

  object GnirsLongSlit:
    given Decoder[GnirsLongSlit] = deriveDecoder

  case class GhostIfu(
    resolutionMode:  GhostResolutionMode,
    stepCount:       PosInt,
    signalToNoiseAt: Wavelength,
    red:             ItcGhostDetector,
    blue:            ItcGhostDetector
  ) extends BasicConfiguration(Instrument.Ghost) derives Eq

  object GhostIfu:
    given Decoder[ItcGhostDetector] = Decoder.instance:
      c =>
        for
          timeAndCount <-
            c.downField("exposureTimeMode")
              .as[ExposureTimeMode.TimeAndCountMode]
              .flatMap: etm =>
                ExposureTimeMode.timeAndCount
                  .getOption(etm)
                  .toRight(
                    DecodingFailure("Expected TimeAndCountMode for GHOST detector", c.history)
                  )
          readMode     <- c.downField("defaultReadMode").as[GhostReadMode]
          binning      <- c.downField("defaultBinning").as[GhostBinning]
        yield ItcGhostDetector(timeAndCount, readMode, binning)

        // TODO: When the ODB API has the signalToNoise value, we can switch to deriving the decoder
    given Decoder[GhostIfu]         = Decoder.instance: c =>
      for
        resolutionMode <- c.downField("resolutionMode").as[GhostResolutionMode]
        stepCount      <- c.downField("stepCount").as[PosInt]
        red            <- c.downField("red").as[ItcGhostDetector]
        blue           <- c.downField("blue").as[ItcGhostDetector]
      yield GhostIfu(resolutionMode, stepCount, red.timeAndCount.at, red = red, blue = blue)

  case class Visitor(
    mode:              VisitorObservingModeType,
    centralWavelength: CentralWavelength,
    agsDiameter:       Angle
  ) extends BasicConfiguration(mode.instrument) derives Eq:
    def site: Site = mode match
      case VisitorObservingModeType.AlopekeSpeckle | VisitorObservingModeType.AlopekeWideField |
          VisitorObservingModeType.MaroonX | VisitorObservingModeType.VisitorNorth =>
        Site.GN
      case VisitorObservingModeType.ZorroSpeckle | VisitorObservingModeType.ZorroWideField |
          VisitorObservingModeType.VisitorSouth =>
        Site.GS

  object Visitor:
    def fov(mode: VisitorObservingModeType): Angle =
      mode match
        case VisitorObservingModeType.AlopekeSpeckle | VisitorObservingModeType.ZorroSpeckle     =>
          AlopekeSpeckleScienceFov
        case VisitorObservingModeType.AlopekeWideField | VisitorObservingModeType.ZorroWideField =>
          AlopekeWideFieldScienceFov
        case VisitorObservingModeType.MaroonX                                                    =>
          MaroonXScienceFov
        case VisitorObservingModeType.VisitorNorth | VisitorObservingModeType.VisitorSouth       =>
          // Let's return something, the user will need to specify a value for an alien visitor.
          Angle.Angle0

    def defaultCentralWavelength(mode: VisitorObservingModeType): Wavelength =
      mode match
        case VisitorObservingModeType.AlopekeSpeckle | VisitorObservingModeType.AlopekeWideField =>
          AlopekeCentralWavelength
        case VisitorObservingModeType.ZorroSpeckle | VisitorObservingModeType.ZorroWideField     =>
          ZorroCentralWavelength
        case VisitorObservingModeType.MaroonX                                                    =>
          MaroonXCentralWavelength
        case VisitorObservingModeType.VisitorNorth | VisitorObservingModeType.VisitorSouth       =>
          // Let's return something, the user will need to specify a value for an alien visitor.
          Wavelength.unsafeFromIntPicometers(700000) // 700 nm

    given Decoder[Visitor] = Decoder.instance: c =>
      for
        mode <- c.downField("mode").as[VisitorObservingModeType]
        cw   <- c.downField("centralWavelength").as[Wavelength]
        gsms <- c.downField("agsDiameter").as[Angle]
      yield Visitor(mode, CentralWavelength(cw), gsms)

  val gmosNorthLongSlit: Prism[BasicConfiguration, GmosNorthLongSlit] =
    GenPrism[BasicConfiguration, GmosNorthLongSlit]

  val gmosSouthLongSlit: Prism[BasicConfiguration, GmosSouthLongSlit] =
    GenPrism[BasicConfiguration, GmosSouthLongSlit]

  val gmosNorthImaging: Prism[BasicConfiguration, GmosNorthImaging] =
    GenPrism[BasicConfiguration, GmosNorthImaging]

  val gmosSouthImaging: Prism[BasicConfiguration, GmosSouthImaging] =
    GenPrism[BasicConfiguration, GmosSouthImaging]

  val flamingos2LongSlit: Prism[BasicConfiguration, Flamingos2LongSlit] =
    GenPrism[BasicConfiguration, Flamingos2LongSlit]

  val flamingos2Imaging: Prism[BasicConfiguration, Flamingos2Imaging] =
    GenPrism[BasicConfiguration, Flamingos2Imaging]

  val igrins2LongSlit: Prism[BasicConfiguration, Igrins2LongSlit.type] =
    GenPrism[BasicConfiguration, Igrins2LongSlit.type]

  val ghostIfu: Prism[BasicConfiguration, GhostIfu] =
    GenPrism[BasicConfiguration, GhostIfu]

  val visitor: Prism[BasicConfiguration, Visitor] =
    GenPrism[BasicConfiguration, Visitor]

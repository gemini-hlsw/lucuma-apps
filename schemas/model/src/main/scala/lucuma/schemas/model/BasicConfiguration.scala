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
import io.circe.Json
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
import lucuma.core.model.sequence.ghost.GhostIfuMapping
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.core.model.sequence.igrins2.CentralWavelength as Igrins2CentralWavelength
import lucuma.core.model.sequence.visitors.AlopekeCentralWavelength
import lucuma.core.model.sequence.visitors.MaroonXCentralWavelength
import lucuma.core.model.sequence.visitors.ZorroCentralWavelength
import lucuma.core.util.TimeSpan
import lucuma.itc.ItcGhostDetector
import lucuma.odb.json.angle.decoder.given
import lucuma.odb.json.time.decoder.given
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
sealed trait BasicConfiguration extends Product with Serializable derives Eq:
  def gmosFpuAlternative: Option[Either[GmosNorthFpu, GmosSouthFpu]] = this match
    case BasicConfiguration.GmosNorthLongSlit(_, _, fpu, _) => fpu.asLeft.some
    case BasicConfiguration.GmosSouthLongSlit(_, _, fpu, _) => fpu.asRight.some
    case _                                                  => none

  def f2Fpu: Option[Flamingos2Fpu] = this match
    case BasicConfiguration.Flamingos2LongSlit(fpu = fpu) => fpu.some
    case _                                                => none

  def siteFor: Option[Site] = this match
    case _: BasicConfiguration.Flamingos2Imaging  => Site.GS.some
    case _: BasicConfiguration.Flamingos2LongSlit => Site.GS.some
    case _: BasicConfiguration.GhostIfu           => Site.GS.some
    case _: BasicConfiguration.GmosNorthImaging   => Site.GN.some
    case _: BasicConfiguration.GmosNorthLongSlit  => Site.GN.some
    case _: BasicConfiguration.GmosSouthImaging   => Site.GS.some
    case _: BasicConfiguration.GmosSouthLongSlit  => Site.GS.some
    case _: BasicConfiguration.GnirsImaging       => Site.GN.some
    case _: BasicConfiguration.GnirsSpectroscopy  => Site.GN.some
    case BasicConfiguration.Igrins2LongSlit       => Site.GN.some
    case v: BasicConfiguration.Visitor            => v.site.some
    case _: BasicConfiguration.KeckExchange       => none
    case _: BasicConfiguration.SubaruExchange     => none

  def obsModeType: ObservingModeType = this match
    case _: BasicConfiguration.Flamingos2Imaging  => ObservingModeType.Flamingos2Imaging
    case _: BasicConfiguration.Flamingos2LongSlit => ObservingModeType.Flamingos2LongSlit
    case _: BasicConfiguration.GhostIfu           => ObservingModeType.GhostIfu
    case _: BasicConfiguration.GmosNorthImaging   => ObservingModeType.GmosNorthImaging
    case _: BasicConfiguration.GmosNorthLongSlit  => ObservingModeType.GmosNorthLongSlit
    case _: BasicConfiguration.GmosSouthImaging   => ObservingModeType.GmosSouthImaging
    case _: BasicConfiguration.GmosSouthLongSlit  => ObservingModeType.GmosSouthLongSlit
    case _: BasicConfiguration.GnirsImaging       => ObservingModeType.GnirsImaging
    case g: BasicConfiguration.GnirsSpectroscopy  => g.gnirsObsModeType
    case BasicConfiguration.Igrins2LongSlit       => ObservingModeType.Igrins2LongSlit
    case v: BasicConfiguration.Visitor            => v.mode
    case _: BasicConfiguration.KeckExchange       => ObservingModeType.ExchangeKeck
    case _: BasicConfiguration.SubaruExchange     => ObservingModeType.ExchangeSubaru

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
    case BasicConfiguration.GnirsSpectroscopy(centralWavelength = cw) =>
      cw.some
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
    case BasicConfiguration.GnirsImaging(filters = filters)           =>
      AGSWavelength(filters.maximumBy(_.centralWavelength).centralWavelength)
    case BasicConfiguration.Flamingos2LongSlit(filter = filter)       =>
      AGSWavelength(filter.wavelength)
    case BasicConfiguration.Igrins2LongSlit                           =>
      AGSWavelength(Igrins2CentralWavelength)
    case BasicConfiguration.GhostIfu(_, _, _, _, _)                   =>
      AGSWavelength(GhostCentralWavelength)
    case gnirs: BasicConfiguration.GnirsSpectroscopy                  =>
      AGSWavelength(gnirs.centralWavelength.value)
    case v: BasicConfiguration.Visitor                                =>
      AGSWavelength(v.centralWavelength.value)
    case _: BasicConfiguration.KeckExchange                           =>
      AGSWavelength(Wavelength.Min)
    case _: BasicConfiguration.SubaruExchange                         =>
      AGSWavelength(Wavelength.Min)

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
    case BasicConfiguration.GnirsImaging(filters = filters)           =>
      filters.minimumBy(_.centralWavelength).centralWavelength
    case BasicConfiguration.Flamingos2LongSlit(filter = filter)       =>
      filter.wavelength
    case BasicConfiguration.Igrins2LongSlit                           =>
      Igrins2CentralWavelength
    case BasicConfiguration.GhostIfu(_, _, _, _, _)                   =>
      GhostCentralWavelength
    case gnirs: BasicConfiguration.GnirsSpectroscopy                  =>
      gnirs.centralWavelength.value
    case v: BasicConfiguration.Visitor                                =>
      v.centralWavelength.value
    case _: BasicConfiguration.KeckExchange                           =>
      Wavelength.Min
    case _: BasicConfiguration.SubaruExchange                         =>
      Wavelength.Min

  def guideProbe(trackType: Option[TrackType]): Option[GuideProbe] =
    trackType.flatMap(probes.guideProbe(obsModeType, _))

  def targetVisualization(
    scienceTargets: List[TargetWithId],
    ifuMapping:     Option[GhostIfuMapping]
  ): TargetVisualization =
    this match
      case BasicConfiguration.GhostIfu(resolutionMode = mode) =>
        val prefix                      = mode match
          case GhostResolutionMode.Standard => "SR"
          case GhostResolutionMode.High     => "HR"
        import SlotId.*
        val slots: List[InstrumentSlot] =
          ifuMapping match
            case Some(GhostIfuMapping.SingleTarget(ifu1))      =>
              List(InstrumentSlot.Science(ifu1, GhostIfu1))
            case Some(GhostIfuMapping.TargetPlusSky(ifu1, sk)) =>
              List(InstrumentSlot.Science(ifu1, GhostIfu1), InstrumentSlot.Sky(sk, GhostIfu2))
            case Some(GhostIfuMapping.SkyPlusTarget(sk, ifu2)) =>
              List(InstrumentSlot.Sky(sk, GhostIfu1), InstrumentSlot.Science(ifu2, GhostIfu2))
            case Some(GhostIfuMapping.DualTarget(ifu1, ifu2))  =>
              List(InstrumentSlot.Science(ifu1, GhostIfu1), InstrumentSlot.Science(ifu2, GhostIfu2))
            case None                                          =>
              val ids = mode match
                case GhostResolutionMode.Standard => List(GhostIfu1, GhostIfu2)
                case GhostResolutionMode.High     => List(GhostIfu1)
              scienceTargets.zip(ids).map((t, id) => InstrumentSlot.Science(t.id, id))
        TargetVisualization(slots, prefix.some)
      case _                                                  =>
        TargetVisualization.Empty

object BasicConfiguration:
  private def filtersDecoder[A: Decoder]: Decoder[NonEmptyList[A]] =
    Decoder.decodeNonEmptyList(using Decoder.instance(_.downField("filter").as[A]))

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
                                    c.downField("gnirsImaging")
                                      .as[GnirsImaging]
                                      .orElse:
                                        c.downField("gnirsSpectroscopy")
                                          .as[GnirsSpectroscopy]
                                          .orElse:
                                            c.downField("ghostIfu")
                                              .as[GhostIfu]
                                              .orElse:
                                                c.downField("visitor")
                                                  .as[Visitor]
                                                  .orElse:
                                                    c.downField("exchange")
                                                      .as[KeckExchange]
                                                      .orElse:
                                                        c.downField("exchange")
                                                          .as[SubaruExchange]
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
  ) extends BasicConfiguration derives Eq

  object GmosNorthLongSlit:
    given Decoder[GmosNorthLongSlit] = deriveDecoder

  case class GmosSouthLongSlit(
    grating:           GmosSouthGrating,
    filter:            Option[GmosSouthFilter],
    fpu:               GmosSouthFpu,
    centralWavelength: CentralWavelength
  ) extends BasicConfiguration derives Eq

  object GmosSouthLongSlit:
    given Decoder[GmosSouthLongSlit] = deriveDecoder

  case class GmosNorthImaging(
    filters: NonEmptyList[GmosNorthFilter]
  ) extends BasicConfiguration derives Eq

  object GmosNorthImaging:
    given Decoder[GmosNorthImaging] = Decoder.instance:
      _.downField("filters").as(using filtersDecoder[GmosNorthFilter]).map(GmosNorthImaging(_))

  case class GmosSouthImaging(
    filters: NonEmptyList[GmosSouthFilter]
  ) extends BasicConfiguration derives Eq

  object GmosSouthImaging:
    given Decoder[GmosSouthImaging] = Decoder.instance:
      _.downField("filters").as(using filtersDecoder[GmosSouthFilter]).map(GmosSouthImaging(_))

  case class Flamingos2LongSlit(
    disperser: Flamingos2Disperser,
    filter:    Flamingos2Filter,
    fpu:       Flamingos2Fpu
  ) extends BasicConfiguration derives Eq

  object Flamingos2LongSlit:
    given Decoder[Flamingos2LongSlit] = deriveDecoder

  case class Flamingos2Imaging(
    filters: NonEmptyList[Flamingos2Filter]
  ) extends BasicConfiguration derives Eq

  object Flamingos2Imaging:
    given Decoder[Flamingos2Imaging] = Decoder.instance:
      _.downField("filters").as(using filtersDecoder[Flamingos2Filter]).map(Flamingos2Imaging(_))

  case class GnirsImaging(
    filters: NonEmptyList[GnirsFilter],
    camera:  GnirsCamera
  ) extends BasicConfiguration derives Eq

  object GnirsImaging:
    given Decoder[GnirsImaging] = Decoder.instance: c =>
      for
        filters <- c.downField("filters").as(using filtersDecoder[GnirsFilter])
        camera  <- c.downField("camera").as[GnirsCamera]
      yield GnirsImaging(filters, camera)

  case object Igrins2LongSlit extends BasicConfiguration derives Eq:
    // Must reject a null field: in the union decoder this branch is tried before
    // ghostIfu/gnirs/visitor, and `Decoder.const` would otherwise swallow any of
    // those (whose `igrins2LongSlit` field is null) as Igrins2LongSlit.
    given Decoder[Igrins2LongSlit.type] = Decoder.instance: c =>
      if c.value.isNull then DecodingFailure("igrins2LongSlit is null", c.history).asLeft
      else Igrins2LongSlit.asRight

  case class GnirsSpectroscopy(
    filter:            GnirsFilter,
    fpu:               GnirsFpu.Spectroscopy,
    prism:             GnirsPrism,
    grating:           GnirsGrating,
    camera:            GnirsCamera,
    centralWavelength: CentralWavelength
  ) extends BasicConfiguration derives Eq:
    // The long slit and the IFU are the same observing mode with distinct types.
    def gnirsObsModeType: ObservingModeType =
      fpu match
        case GnirsFpu.Spectroscopy.Slit(_) => ObservingModeType.GnirsLongSlit
        case GnirsFpu.Spectroscopy.Ifu(_)  => ObservingModeType.GnirsIfu

  object GnirsSpectroscopy:
    given Decoder[GnirsSpectroscopy] = Decoder.instance: c =>
      for
        filter   <- c.downField("filter").as[GnirsFilter]
        slitJson <- c.downField("slit").as[Option[Json]]
        ifuJson  <- c.downField("ifu").as[Option[Json]]
        fpu      <- (slitJson, ifuJson) match
                      case (Some(s), None) =>
                        s.hcursor.downField("fpu").as[GnirsFpuSlit].map(GnirsFpu.Spectroscopy.Slit(_))
                      case (None, Some(i)) =>
                        i.hcursor.downField("fpu").as[GnirsFpuIfu].map(GnirsFpu.Spectroscopy.Ifu(_))
                      case _               =>
                        DecodingFailure(
                          "GNIRS spectroscopy: exactly one of slit / ifu expected",
                          c.history
                        ).asLeft
        prism    <- c.downField("prism").as[GnirsPrism]
        grating  <- c.downField("grating").as[GnirsGrating]
        camera   <- c.downField("camera").as[GnirsCamera]
        cw       <- c.downField("centralWavelength").as[CentralWavelength]
      yield GnirsSpectroscopy(filter, fpu, prism, grating, camera, cw)

  case class GhostIfu(
    resolutionMode:  GhostResolutionMode,
    stepCount:       PosInt,
    signalToNoiseAt: Wavelength,
    red:             ItcGhostDetector,
    blue:            ItcGhostDetector
  ) extends BasicConfiguration derives Eq

  object GhostIfu:
    given Decoder[ItcGhostDetector] = Decoder.instance:
      c =>
        for
          timeAndCount <-
            c.downField("exposureTimeMode")
              .as[ExposureTimeMode]
              .flatMap: etm =>
                ExposureTimeMode.timeAndCount
                  .getOption(etm)
                  .toRight(
                    DecodingFailure("Expected TimeAndCountMode for GHOST detector", c.history)
                  )
          // Effective values (explicit override or default), matching
          // ObservingMode.GhostIfu.toBasicConfiguration on `main`.
          readMode     <- c.downField("readMode").as[GhostReadMode]
          binning      <- c.downField("binning").as[GhostBinning]
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
  ) extends BasicConfiguration derives Eq:
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

  case class KeckExchange(
    keckInstrument:   KeckInstrument,
    totalRequestTime: TimeSpan
  ) extends BasicConfiguration derives Eq

  object KeckExchange:
    given Decoder[KeckExchange] = Decoder.instance: c =>
      for
        keckInstrument   <- c.downField("keckInstrument").as[KeckInstrument]
        totalRequestTime <- c.downField("totalRequestTime").as[TimeSpan]
      yield KeckExchange(keckInstrument, totalRequestTime)

  case class SubaruExchange(
    subaruInstrument: SubaruInstrument,
    totalRequestTime: TimeSpan
  ) extends BasicConfiguration derives Eq

  object SubaruExchange:
    given Decoder[SubaruExchange] = Decoder.instance: c =>
      for
        subaruInstrument <- c.downField("subaruInstrument").as[SubaruInstrument]
        totalRequestTime <- c.downField("totalRequestTime").as[TimeSpan]
      yield SubaruExchange(subaruInstrument, totalRequestTime)

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

  val gnirsImaging: Prism[BasicConfiguration, GnirsImaging] =
    GenPrism[BasicConfiguration, GnirsImaging]

  val igrins2LongSlit: Prism[BasicConfiguration, Igrins2LongSlit.type] =
    GenPrism[BasicConfiguration, Igrins2LongSlit.type]

  val ghostIfu: Prism[BasicConfiguration, GhostIfu] =
    GenPrism[BasicConfiguration, GhostIfu]

  val visitor: Prism[BasicConfiguration, Visitor] =
    GenPrism[BasicConfiguration, Visitor]

  val keckExchange: Prism[BasicConfiguration, KeckExchange] =
    GenPrism[BasicConfiguration, KeckExchange]

  val subaruExchange: Prism[BasicConfiguration, SubaruExchange] =
    GenPrism[BasicConfiguration, SubaruExchange]

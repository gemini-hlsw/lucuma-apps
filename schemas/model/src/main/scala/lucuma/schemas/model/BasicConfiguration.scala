// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.generic.semiauto.*
import lucuma.core.enums.*
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Target
import lucuma.core.model.probes
import lucuma.itc.ItcGhostDetector
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
    case _: BasicConfiguration.GmosNorthLongSlit  => Site.GN
    case _: BasicConfiguration.GmosSouthLongSlit  => Site.GS
    case _: BasicConfiguration.GmosNorthImaging   => Site.GN
    case _: BasicConfiguration.GmosSouthImaging   => Site.GS
    case _: BasicConfiguration.Flamingos2LongSlit => Site.GS
    case BasicConfiguration.Igrins2LongSlit       => Site.GN
    case _: BasicConfiguration.GhostIfu           => Site.GS

  def obsModeType: ObservingModeType = this match
    case n: BasicConfiguration.GmosNorthLongSlit  => ObservingModeType.GmosNorthLongSlit
    case s: BasicConfiguration.GmosSouthLongSlit  => ObservingModeType.GmosSouthLongSlit
    case n: BasicConfiguration.GmosNorthImaging   => ObservingModeType.GmosNorthImaging
    case s: BasicConfiguration.GmosSouthImaging   => ObservingModeType.GmosSouthImaging
    case s: BasicConfiguration.Flamingos2LongSlit => ObservingModeType.Flamingos2LongSlit
    case BasicConfiguration.Igrins2LongSlit       => ObservingModeType.Igrins2LongSlit
    case _: BasicConfiguration.GhostIfu           => ObservingModeType.GhostIfu

  def centralWv: Option[CentralWavelength] = this match
    case BasicConfiguration.GmosNorthLongSlit(centralWavelength = cw) =>
      cw.some
    case BasicConfiguration.GmosSouthLongSlit(centralWavelength = cw) =>
      cw.some
    case BasicConfiguration.Flamingos2LongSlit(filter = filter)       =>
      CentralWavelength(filter.wavelength).some
    case BasicConfiguration.Igrins2LongSlit                           =>
      // TODO: What is the right value?
      CentralWavelength(Wavelength.unsafeFromIntPicometers(1800000)).some
    case BasicConfiguration.GhostIfu(_, _, _, _)                      =>
      CentralWavelength(BasicConfiguration.GhostIfu.centralWavelength).some
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
    case BasicConfiguration.Flamingos2LongSlit(filter = filter)       =>
      AGSWavelength(filter.wavelength)
    case BasicConfiguration.Igrins2LongSlit                           =>
      // TODO: What is the right value?
      AGSWavelength(BasicConfiguration.Igrins2LongSlit.optimalWavelength)
    case BasicConfiguration.GhostIfu(_, _, _, _)                      =>
      AGSWavelength(BasicConfiguration.GhostIfu.centralWavelength)

  def conditionsWavelength: Wavelength = this match
    case BasicConfiguration.GmosNorthLongSlit(centralWavelength = cw) =>
      cw.value
    case BasicConfiguration.GmosSouthLongSlit(centralWavelength = cw) =>
      cw.value
    case BasicConfiguration.GmosNorthImaging(filters)                 =>
      filters.minimumBy(_.wavelength).wavelength
    case BasicConfiguration.GmosSouthImaging(filters)                 =>
      filters.minimumBy(_.wavelength).wavelength
    case BasicConfiguration.Flamingos2LongSlit(filter = filter)       =>
      filter.wavelength
    case BasicConfiguration.Igrins2LongSlit                           =>
      // TODO: What is the right value?
      BasicConfiguration.Igrins2LongSlit.optimalWavelength
    case BasicConfiguration.GhostIfu(_, _, _, _)                      =>
      BasicConfiguration.GhostIfu.centralWavelength

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
    case BasicConfiguration.Flamingos2LongSlit(_, _, _) => GuideProbe.Flamingos2OIWFS
    case BasicConfiguration.Igrins2LongSlit             => GuideProbe.PWFS2
    case BasicConfiguration.GhostIfu(_, _, _, _)        => GuideProbe.PWFS2

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
                        c.downField("flamingos2LongSlit")
                          .as[Flamingos2LongSlit]
                          .orElse:
                            c.downField("igrins2LongSlit")
                              .as[Igrins2LongSlit.type]
                              .orElse:
                                c.downField("ghostIfu")
                                  .as[GhostIfu]
                                  .orElse:
                                    DecodingFailure("Could not decode BasicConfiguration",
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

  case object Igrins2LongSlit extends BasicConfiguration(Instrument.Igrins2) derives Eq:
    // This is the optimal wavelength from the modes query, but we don't have access to that info
    // here so we'll hardcode it - for now at least.
    val optimalWavelength: Wavelength   = Wavelength.fromIntNanometers(1975).get
    given Decoder[Igrins2LongSlit.type] = Decoder.const(Igrins2LongSlit)

  case class GhostIfu(
    resolutionMode:  GhostResolutionMode,
    signalToNoiseAt: Wavelength,
    red:             ItcGhostDetector,
    blue:            ItcGhostDetector
  ) extends BasicConfiguration(Instrument.Ghost) derives Eq

  object GhostIfu:
    val centralWavelength: Wavelength = Wavelength.fromIntNanometers(530).get

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
        red            <- c.downField("red").as[ItcGhostDetector]
        blue           <- c.downField("blue").as[ItcGhostDetector]
      yield GhostIfu(resolutionMode, red.timeAndCount.at, red = red, blue = blue)

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

  val igrins2LongSlit: Prism[BasicConfiguration, Igrins2LongSlit.type] =
    GenPrism[BasicConfiguration, Igrins2LongSlit.type]

  val ghostIfu: Prism[BasicConfiguration, GhostIfu] =
    GenPrism[BasicConfiguration, GhostIfu]

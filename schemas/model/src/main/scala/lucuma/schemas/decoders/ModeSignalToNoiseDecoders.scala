// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.decoders

import io.circe.ACursor
import io.circe.Decoder
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GnirsFilter
import lucuma.itc.SignalToNoiseAt
import lucuma.itc.client.json.decoders.given
import lucuma.schemas.model.ItcResultValues
import lucuma.schemas.model.ModeSignalToNoise
import lucuma.schemas.model.PeakPixel

trait ModeSignalToNoiseDecoders:
  private given Decoder[PeakPixel] = Decoder.instance: c =>
    for
      flux <- c.downField("flux").as[Double]
      adu  <- c.downField("adu").as[Int]
    yield PeakPixel(flux, adu)

  private def itcResultValues(c: ACursor): Decoder.Result[ItcResultValues] =
    for
      sn   <- c.downField("signalToNoiseAt").as[Option[SignalToNoiseAt]]
      peak <- c.downField("peakPixel").as[Option[PeakPixel]]
    yield ItcResultValues(sn, peak)

  given Decoder[ModeSignalToNoise.Spectroscopy] = Decoder.instance: c =>
    for
      acquisition <- itcResultValues(c.downField("acquisition").downField("selected"))
      science     <- itcResultValues(c.downField("spectroscopyScience").downField("selected"))
    yield ModeSignalToNoise.Spectroscopy(acquisition, science)

  private def itcTupleDecoder[Filter: Decoder]: Decoder[(Filter, ItcResultValues)] =
    Decoder.instance: c =>
      for
        filter <- c.downField("filter").as[Filter]
        itc    <- itcResultValues(c.downField("results").downField("selected"))
      yield (filter, itc)

  private def itcTupleListDecoder[Filter: Decoder]: Decoder[List[(Filter, ItcResultValues)]] =
    summon[Decoder[List[(Filter, ItcResultValues)]]](using
      Decoder.decodeList(using itcTupleDecoder[Filter])
    )

  given Decoder[ModeSignalToNoise.GmosNorthImaging] = Decoder.instance:
    _.downField("gmosNorthImagingScience")
      .as[List[(GmosNorthFilter, ItcResultValues)]](using itcTupleListDecoder[GmosNorthFilter])
      .map(m => ModeSignalToNoise.GmosNorthImaging(m.toMap))

  given Decoder[ModeSignalToNoise.GmosSouthImaging] = Decoder.instance:
    _.downField("gmosSouthImagingScience")
      .as[List[(GmosSouthFilter, ItcResultValues)]](using itcTupleListDecoder[GmosSouthFilter])
      .map(m => ModeSignalToNoise.GmosSouthImaging(m.toMap))

  given Decoder[ModeSignalToNoise.Flamingos2Imaging] = Decoder.instance:
    _.downField("flamingos2ImagingScience")
      .as[List[(Flamingos2Filter, ItcResultValues)]](using itcTupleListDecoder[Flamingos2Filter])
      .map(m => ModeSignalToNoise.Flamingos2Imaging(m.toMap))

  given Decoder[ModeSignalToNoise.GnirsImaging] = Decoder.instance:
    _.downField("gnirsImagingScience")
      .as[List[(GnirsFilter, ItcResultValues)]](using itcTupleListDecoder[GnirsFilter])
      .map(m => ModeSignalToNoise.GnirsImaging(m.toMap))

  given Decoder[ModeSignalToNoise.GhostIfu] = Decoder.instance: c =>
    for
      red  <- itcResultValues(c.downField("red").downField("selected"))
      blue <- itcResultValues(c.downField("blue").downField("selected"))
    yield ModeSignalToNoise.GhostIfu(red, blue)

  given Decoder[ModeSignalToNoise] = Decoder.instance: c =>
    if c.value.isNull then Right(ModeSignalToNoise.Undefined)
    else
      c.downField("itcType")
        .as[String]
        .flatMap:
          case "SPECTROSCOPY" | "IGRINS_2_SPECTROSCOPY" | "GNIRS_SPECTROSCOPY" =>
            c.as[ModeSignalToNoise.Spectroscopy]
          case "GMOS_NORTH_IMAGING"                                            =>
            c.as[ModeSignalToNoise.GmosNorthImaging]
          case "GMOS_SOUTH_IMAGING"                                            =>
            c.as[ModeSignalToNoise.GmosSouthImaging]
          case "FLAMINGOS_2_IMAGING"                                           =>
            c.as[ModeSignalToNoise.Flamingos2Imaging]
          case "GNIRS_IMAGING"                                                 =>
            c.as[ModeSignalToNoise.GnirsImaging]
          case "GHOST_IFU"                                                     =>
            c.as[ModeSignalToNoise.GhostIfu]
          case other                                                           =>
            Left(io.circe.DecodingFailure(s"Unknown itcType: $other", c.history))

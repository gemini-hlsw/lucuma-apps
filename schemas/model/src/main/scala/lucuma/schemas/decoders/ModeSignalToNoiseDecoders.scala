// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.decoders

import io.circe.Decoder
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GnirsFilter
import lucuma.itc.SignalToNoiseAt
import lucuma.itc.client.json.decoders.given
import lucuma.odb.data.ItcScience
import lucuma.schemas.model.ModeSignalToNoise

trait ModeSignalToNoiseDecoders:
  given Decoder[ModeSignalToNoise.Spectroscopy] = Decoder.instance: c =>
    for
      acquisitionSn <- c.downField("acquisition")
                         .downField("selected")
                         .downField("signalToNoiseAt")
                         .as[Option[SignalToNoiseAt]]
      scienceSn     <- c.downField("spectroscopyScience")
                         .downField("selected")
                         .downField("signalToNoiseAt")
                         .as[Option[SignalToNoiseAt]]
    yield ModeSignalToNoise.Spectroscopy(acquisitionSn, scienceSn)

  private def snTupleDecoder[Filter: Decoder]: Decoder[(Filter, SignalToNoiseAt)] =
    Decoder.instance: c =>
      for
        filter <- c.downField("filter").as[Filter]
        sn     <- c.downField("results")
                    .downField("selected")
                    .downField("signalToNoiseAt")
                    .as[SignalToNoiseAt]
      yield (filter, sn)

  private def snTupleListDecoder[Filter: Decoder]: Decoder[List[(Filter, SignalToNoiseAt)]] =
    summon[Decoder[List[(Filter, SignalToNoiseAt)]]](using
      Decoder.decodeList(using snTupleDecoder[Filter])
    )

  given Decoder[ModeSignalToNoise.GmosNorthImaging] = Decoder.instance:
    _.downField("gmosNorthImagingScience")
      .as[List[(GmosNorthFilter, SignalToNoiseAt)]](using snTupleListDecoder[GmosNorthFilter])
      .map(m => ModeSignalToNoise.GmosNorthImaging(m.toMap))

  given Decoder[ModeSignalToNoise.GmosSouthImaging] = Decoder.instance:
    _.downField("gmosSouthImagingScience")
      .as[List[(GmosSouthFilter, SignalToNoiseAt)]](using snTupleListDecoder[GmosSouthFilter])
      .map(m => ModeSignalToNoise.GmosSouthImaging(m.toMap))

  given Decoder[ModeSignalToNoise.Flamingos2Imaging] = Decoder.instance:
    _.downField("flamingos2ImagingScience")
      .as[List[(Flamingos2Filter, SignalToNoiseAt)]](using snTupleListDecoder[Flamingos2Filter])
      .map(m => ModeSignalToNoise.Flamingos2Imaging(m.toMap))

  given Decoder[ModeSignalToNoise.GnirsImaging] = Decoder.instance:
    _.downField("gnirsImagingScience")
      .as[List[(GnirsFilter, SignalToNoiseAt)]](using snTupleListDecoder[GnirsFilter])
      .map(m => ModeSignalToNoise.GnirsImaging(m.toMap))

  given Decoder[ModeSignalToNoise.GhostIfu] = Decoder.instance: c =>
    for
      redSn  <- c.downField("red")
                  .downField("selected")
                  .downField("signalToNoiseAt")
                  .as[Option[SignalToNoiseAt]]
      blueSn <- c.downField("blue")
                  .downField("selected")
                  .downField("signalToNoiseAt")
                  .as[Option[SignalToNoiseAt]]
    yield ModeSignalToNoise.GhostIfu(redSn, blueSn)

  given Decoder[ModeSignalToNoise] = Decoder.instance: c =>
    if c.value.isNull then Right(ModeSignalToNoise.Undefined)
    else
      c.downField("itcType")
        .as[ItcScience.Type]
        .flatMap:
          case ItcScience.Type.Spectroscopy      => c.as[ModeSignalToNoise.Spectroscopy]
          case ItcScience.Type.GmosNorthImaging  => c.as[ModeSignalToNoise.GmosNorthImaging]
          case ItcScience.Type.GmosSouthImaging  => c.as[ModeSignalToNoise.GmosSouthImaging]
          case ItcScience.Type.Flamingos2Imaging => c.as[ModeSignalToNoise.Flamingos2Imaging]
          case ItcScience.Type.GnirsImaging      => c.as[ModeSignalToNoise.GnirsImaging]
          case ItcScience.Type.GhostIfu          => c.as[ModeSignalToNoise.GhostIfu]

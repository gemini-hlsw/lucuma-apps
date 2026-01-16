// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.decoders

import io.circe.Decoder
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.itc.SignalToNoiseAt
import lucuma.itc.client.json.decoders.given
import lucuma.odb.data.Itc
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

  given Decoder[ModeSignalToNoise] = Decoder.instance: c =>
    if c.value.isNull then Right(ModeSignalToNoise.Undefined)
    else
      c.downField("itcType")
        .as[Itc.Type]
        .flatMap:
          case Itc.Type.Spectroscopy     => c.as[ModeSignalToNoise.Spectroscopy]
          case Itc.Type.GmosNorthImaging => c.as[ModeSignalToNoise.GmosNorthImaging]
          case Itc.Type.GmosSouthImaging => c.as[ModeSignalToNoise.GmosSouthImaging]

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import cats.parse.Parser
import cats.parse.Parser0
import cats.parse.Rfc5234.char
import cats.syntax.all.*
import lucuma.core.enums.LightSinkName
import lucuma.core.enums.Site
import lucuma.core.enums.parser.EnumParsers
import lucuma.core.parser.MiscParsers.int
import mouse.boolean.given
import navigate.model.enums.LightSink
import navigate.model.enums.LightSource
import navigate.model.enums.LightSource.*
import navigate.server.acm.Decoder
import navigate.server.acm.Encoder
import navigate.server.acm.Encoder.*
import navigate.server.tcs.ScienceFold.Generic
import navigate.server.tcs.ScienceFold.Parked
import navigate.server.tcs.ScienceFold.Position

// Decoding and encoding the science fold position require some common definitions, therefore I
// put them inside an object
private[server] trait ScienceFoldPositionCodex:

  private val AO_PREFIX   = "ao2"
  private val GCAL_PREFIX = "gcal2"
  private val PARK_POS    = "park-pos"

  val lightSinkName: Parser[LightSinkName] = EnumParsers.enumBy[LightSinkName](_.name)

  def prefixed(p: String, s: LightSource): Parser0[ScienceFold] =
    (Parser.string0(p) *> lightSinkName ~ int.?)
      .map: (ls, port) =>
        Position(s, ls, port.getOrElse(1))
      .widen[ScienceFold]

  def calcLightSink(lightSinkName: LightSinkName, port: Int, site: Site): LightSink =
    (lightSinkName, port) match {
      case (LightSinkName.Gmos, _)                              =>
        (site === Site.GS).fold(LightSink.GmosSouth, LightSink.GmosNorth)
      case (LightSinkName.Niri_f6, _)                           => LightSink.NiriF6
      case (LightSinkName.Niri_f14, _)                          => LightSink.NiriF14
      case (LightSinkName.Niri_f32, _)                          => LightSink.NiriF32
      case (LightSinkName.Ac, _) | (LightSinkName.Hr, _)        =>
        (site === Site.GS).fold(LightSink.AcqCamSouth, LightSink.AcqCamNorth)
      case (LightSinkName.Gmos_Ifu, _)                          =>
        (site === Site.GS).fold(LightSink.GmosSouthIfu, LightSink.GmosNorthIfu)
      case (LightSinkName.Gnirs, _)                             => LightSink.Gnirs
      case (LightSinkName.Visitor, 1) if site === Site.GN       => LightSink.MaroonX
      case (LightSinkName.Visitor, 2)                           => (site === Site.GS).fold(LightSink.Zorro, LightSink.Alopeke)
      case (LightSinkName.Visitor, _)                           =>
        (site === Site.GS).fold(LightSink.VisitorSouth, LightSink.VisitorNorth)
      case (LightSinkName.Flamingos2, _)                        => LightSink.Flamingos2
      case (LightSinkName.Gsaoi, _)                             => LightSink.Gsaoi
      case (LightSinkName.Gpi, _)                               => LightSink.Gpi
      case (LightSinkName.Ghost, _)                             => LightSink.Ghost
      case (LightSinkName.Igrins2, _)                           => LightSink.Igrins2
      case (LightSinkName.Scorpio, _)                           => LightSink.Scorpio
      case (LightSinkName.Nifs, _) | (LightSinkName.Phoenix, _) =>
        sys.error(s"Unsupported science fold position name ${lightSinkName.name}")
    }

  val park: Parser[ScienceFold] =
    (Parser.string(PARK_POS) <* char.rep.?).as(Parked)

  given Decoder[String, ScienceFold] = (t: String) =>
    (park | prefixed(AO_PREFIX, AO) | prefixed(GCAL_PREFIX, GCAL) | prefixed("", Sky))
      .parseAll(t)
      .getOrElse(Generic(t))

  given Encoder[LightSinkName, String] = _.name

  extension (x: LightSink) {
    def toLightSinkName: LightSinkName = x match {
      case LightSink.AcqCamNorth | LightSink.AcqCamSouth   => LightSinkName.Ac
      case LightSink.Alopeke                               => LightSinkName.Visitor
      case LightSink.Flamingos2                            => LightSinkName.Flamingos2
      case LightSink.Ghost                                 => LightSinkName.Ghost
      case LightSink.GmosNorth | LightSink.GmosSouth       => LightSinkName.Gmos
      case LightSink.GmosNorthIfu | LightSink.GmosSouthIfu => LightSinkName.Gmos_Ifu
      case LightSink.Gnirs                                 => LightSinkName.Gnirs
      case LightSink.Gpi                                   => LightSinkName.Gpi
      case LightSink.Gsaoi                                 => LightSinkName.Gsaoi
      case LightSink.Igrins2                               => LightSinkName.Igrins2
      case LightSink.MaroonX                               => LightSinkName.Visitor
      case LightSink.NiriF6                                => LightSinkName.Niri_f6
      case LightSink.NiriF14                               => LightSinkName.Niri_f14
      case LightSink.NiriF32                               => LightSinkName.Niri_f32
      case LightSink.Scorpio                               => LightSinkName.Scorpio
      case LightSink.VisitorNorth                          => LightSinkName.Visitor
      case LightSink.VisitorSouth                          => LightSinkName.Visitor
      case LightSink.Zorro                                 => LightSinkName.Visitor
    }
  }

  given Encoder[Position, String] = (a: Position) =>
    // Position name in agSeq doesn't have the port number for AC
    val instAGName =
      if (a.sink === LightSinkName.Ac) a.sink.encode else s"${a.sink.encode}${a.port}"

    a.source match {
      case Sky  => instAGName
      case AO   => AO_PREFIX + instAGName
      case GCAL => GCAL_PREFIX + instAGName
    }

object ScienceFoldPositionCodex extends ScienceFoldPositionCodex

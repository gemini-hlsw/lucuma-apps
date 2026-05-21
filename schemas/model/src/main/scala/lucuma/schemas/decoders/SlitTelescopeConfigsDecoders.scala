// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.decoders

import cats.data.NonEmptyList
import cats.syntax.either.*
import io.circe.Decoder
import io.circe.DecodingFailure
import lucuma.core.enums.SlitOffsetMode
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.TelescopeConfigAlongSlit
import lucuma.odb.json.stepconfig.given
import lucuma.odb.json.telescopeConfigAlongSlit.decoder.given

trait SlitTelescopeConfigsDecoders:
  given Decoder[SlitTelescopeConfigs] = Decoder.instance: c =>
    for
      offsetMode <- c.downField("offsetMode").as[SlitOffsetMode]
      alongSlit  <- c.downField("alongSlit").as[Option[NonEmptyList[TelescopeConfigAlongSlit]]]
      toSky      <- c.downField("toSky").as[Option[NonEmptyList[TelescopeConfig]]]
      configs    <- (offsetMode, alongSlit, toSky) match
                      case (SlitOffsetMode.NodAlongSlit, Some(as), None) =>
                        SlitTelescopeConfigs.AlongSlit(as).asRight
                      case (SlitOffsetMode.NodToSky, None, Some(os))     =>
                        SlitTelescopeConfigs.ToSky(os).asRight
                      case _                                             =>
                        DecodingFailure(
                          s"Invalid SlitTelescopeConfigs: offsetMode $offsetMode does not match telescopeConfig data",
                          c.history
                        ).asLeft
    yield configs

object SlitTelescopeConfigsDecoders extends SlitTelescopeConfigsDecoders

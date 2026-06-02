// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.decoders

import cats.syntax.either.*
import io.circe.Decoder
import io.circe.DecodingFailure
import lucuma.core.math.Offset
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMode
import lucuma.odb.json.offset.decoder.given

trait GnirsDecoders:
  given Decoder[GnirsAcquisitionMode] = Decoder.instance: c =>
    for
      enabled   <- c.downField("enabled").as[Boolean]
      offset    <- c.downField("offset").as[Option[Offset]]
      skyOffset <- enabled match
                     case false => GnirsAcquisitionSkyOffset.Disabled.asRight
                     case true  =>
                       offset.fold(
                         DecodingFailure(
                           "Missing offset for enabled GnirsAcquisitionSkyOffset",
                           c.history
                         ).asLeft
                       )(GnirsAcquisitionSkyOffset.Enabled(_).asRight)
    yield skyOffset

object GnirsDecoders extends GnirsDecoders

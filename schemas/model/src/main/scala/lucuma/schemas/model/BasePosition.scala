// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model

import cats.Eq
import cats.derived.*
import io.circe.Decoder
import lucuma.core.math.Coordinates
import lucuma.odb.json.coordinates.query.given
import lucuma.schemas.model.enums.BasePositionType

case class BasePosition(
  baseType:    BasePositionType,
  coordinates: Option[Coordinates]
) derives Eq

object BasePosition:
  given Decoder[BasePosition] = Decoder.instance: c =>
    for
      t  <- c.get[BasePositionType]("type")
      cs <- c.get[Option[Coordinates]]("coordinates")
    yield BasePosition(t, cs)

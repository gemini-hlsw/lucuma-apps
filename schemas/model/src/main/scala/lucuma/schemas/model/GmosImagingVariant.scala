// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model

import cats.Eq
import cats.derived.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.NonNegInt
import io.circe.Decoder
import lucuma.core.enums.WavelengthOrder
import lucuma.core.math.Offset

enum GmosImagingVariant derives Eq:
  case Grouped(
    order:      WavelengthOrder,
    offsets:    Option[TelescopeConfigGenerator],
    skyCount:   NonNegInt,
    skyOffsets: Option[TelescopeConfigGenerator]
  ) extends GmosImagingVariant
  case Interleaved(
    offsets:    Option[TelescopeConfigGenerator],
    skyCount:   NonNegInt,
    skyOffsets: Option[TelescopeConfigGenerator]
  ) extends GmosImagingVariant
  case PreImaging(offset1: Offset, offset2: Offset, offset3: Offset, offset4: Offset)
      extends GmosImagingVariant

object GmosImagingVariant:
  given Decoder[GmosImagingVariant] = ???

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import lucuma.schemas.ObservationDB
import lucuma.schemas.decoders.given
import lucuma.schemas.model.ImagingVariant
import lucuma.schemas.odb.*

@GraphQLType("ImagingVariant")
object ImagingVariantSubquery
    extends GraphQLSubquery.Typed[ObservationDB, ImagingVariant]:
  override val subquery: String = s"""
    {
      variantType
      grouped {
        order
        offsets $TelescopeConfigGeneratorSubquery
        skyCount
        skyOffsets $TelescopeConfigGeneratorSubquery
      }
      interleaved {
        offsets $TelescopeConfigGeneratorSubquery
        skyCount
        skyOffsets $TelescopeConfigGeneratorSubquery
      }
      preImaging {
        offset1 $OffsetSubquery
        offset2 $OffsetSubquery
        offset3 $OffsetSubquery
        offset4 $OffsetSubquery
      }
    }
  """

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import lucuma.schemas.ObservationDB
import lucuma.schemas.decoders.given
import lucuma.schemas.model.GmosImagingVariant
import lucuma.schemas.odb.*

object GmosImagingVariantSubquery
    extends GraphQLSubquery.Typed[ObservationDB, GmosImagingVariant]("GmosImagingVariant") {
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
}

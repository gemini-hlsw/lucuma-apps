// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import lucuma.schemas.model.TelescopeConfigGenerator
import lucuma.schemas.ObservationDB
import lucuma.schemas.decoders.given
import lucuma.schemas.odb.*

object TelescopeConfigGeneratorSubquery
    extends GraphQLSubquery.Typed[ObservationDB, Option[TelescopeConfigGenerator]](
      "TelescopeConfigGenerator"
    ):
  override val subquery: String = s"""
    {
      generatorType
      enumerated {
        values {
          offset $OffsetSubquery
          guiding
        }
      }
      random {
        size $AngleSubquery
        center $OffsetSubquery
      }
      spiral {
        size $AngleSubquery
        center $OffsetSubquery      
      }
      uniform {
        cornerA $OffsetSubquery
        cornerB $OffsetSubquery
      }
    }
  """

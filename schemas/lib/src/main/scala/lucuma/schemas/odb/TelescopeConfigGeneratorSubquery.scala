// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import lucuma.schemas.ObservationDB
import lucuma.schemas.decoders.given
import lucuma.schemas.model.TelescopeConfigGenerator
import lucuma.schemas.odb.*

@GraphQLType("TelescopeConfigGenerator")
object TelescopeConfigGeneratorSubquery
    extends GraphQLSubquery.Typed[ObservationDB, Option[TelescopeConfigGenerator]]:
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

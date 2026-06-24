// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.schemas.ObservationDB
import lucuma.schemas.decoders.SlitTelescopeConfigsDecoders.given

@GraphQLType("SlitTelescopeConfigs")
object SlitTelescopeConfigsSubquery
    extends GraphQLSubquery.Typed[ObservationDB, SlitTelescopeConfigs]:
  override val subquery: String = s"""
        {
          offsetMode
          alongSlit {
           q $AngleSubquery
           guiding
          }
          toSky {
            offset $OffsetSubquery
            guiding
          }
        }
      """

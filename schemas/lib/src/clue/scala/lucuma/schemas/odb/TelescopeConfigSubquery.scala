// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.annotation.GraphQL
import clue.GraphQLSubquery
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.schemas.ObservationDB
import lucuma.odb.json.stepconfig.given

@GraphQL
object TelescopeConfigSubquery
    extends GraphQLSubquery.Typed[ObservationDB, TelescopeConfig]("TelescopeConfig"):
  override val subquery: String = s"""
        {
          offset $OffsetSubquery
          guiding
        }
      """

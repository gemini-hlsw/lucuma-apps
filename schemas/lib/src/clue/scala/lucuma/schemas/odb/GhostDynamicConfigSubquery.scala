// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.odb.json.ghost.given
import lucuma.schemas.ObservationDB

@GraphQL
object GhostDynamicConfigSubquery
    extends GraphQLSubquery.Typed[ObservationDB, GhostDynamicConfig]("GhostDynamic"):
  override val subquery: String = s"""
    {
      red {
        exposureTime $TimeSpanSubquery
        exposureCount
        binning
        readMode
      }
      blue {
        exposureTime $TimeSpanSubquery
        exposureCount
        binning
        readMode
      }
      ifu1FiberAgitator
      ifu2FiberAgitator
    }
  """

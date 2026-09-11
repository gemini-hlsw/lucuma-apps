// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import clue.annotation.GraphQLType
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.odb.json.sequence.given
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.*

// Same selection as the GhostExecutionConfig branch of ExecutionConfigSubquery, for the per-instrument reads.
@GraphQL
@GraphQLType("GhostExecutionConfig")
object GhostExecutionConfigSubquery
    extends GraphQLSubquery.Typed[ObservationDB, InstrumentExecutionConfig.Ghost]:
  override val subquery = gql"""
    {
      static {
        resolutionMode
        ifuMapping $GhostIfuMappingSubquery
        slitViewingCameraExposureTime $TimeSpanSubquery
      }
      science {
        nextAtom $GhostAtomSubquery
        possibleFuture $GhostAtomSubquery
        hasMore
      }
    }
  """

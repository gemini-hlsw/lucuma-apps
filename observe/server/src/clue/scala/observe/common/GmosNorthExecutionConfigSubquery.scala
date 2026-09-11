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

// Same selection as the GmosNorthExecutionConfig branch of ExecutionConfigSubquery, for the per-instrument reads.
@GraphQL
@GraphQLType("GmosNorthExecutionConfig")
object GmosNorthExecutionConfigSubquery
    extends GraphQLSubquery.Typed[ObservationDB, InstrumentExecutionConfig.GmosNorth]:
  override val subquery = gql"""
    {
      static {
        stageMode
        detector
        mosPreImaging
        nodAndShuffle {
          posA $OffsetSubquery
          posB $OffsetSubquery
          eOffset
          shuffleOffset
          shuffleCycles
        }
      }
      acquisition {
        nextAtom $GmosNorthAtomSubquery
        possibleFuture $GmosNorthAtomSubquery
        hasMore
      }
      science {
        nextAtom $GmosNorthAtomSubquery
        possibleFuture $GmosNorthAtomSubquery
        hasMore
      }
    }
  """

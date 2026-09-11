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

// Same selection as the Igrins2ExecutionConfig branch of ExecutionConfigSubquery, for the per-instrument reads.
@GraphQL
@GraphQLType("Igrins2ExecutionConfig")
object Igrins2ExecutionConfigSubquery
    extends GraphQLSubquery.Typed[ObservationDB, InstrumentExecutionConfig.Igrins2]:
  override val subquery = gql"""
    {
      static {
        saveSVCImages
        offsetMode
      }
      acquisition {
        nextAtom $Igrins2AtomSubquery
        possibleFuture $Igrins2AtomSubquery
        hasMore
      }
      science {
        nextAtom $Igrins2AtomSubquery
        possibleFuture $Igrins2AtomSubquery
        hasMore
      }
    }
  """

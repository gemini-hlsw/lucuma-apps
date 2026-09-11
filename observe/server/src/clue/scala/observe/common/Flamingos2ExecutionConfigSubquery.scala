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

// Same selection as the Flamingos2ExecutionConfig branch of ExecutionConfigSubquery, for the per-instrument reads.
@GraphQL
@GraphQLType("Flamingos2ExecutionConfig")
object Flamingos2ExecutionConfigSubquery
    extends GraphQLSubquery.Typed[ObservationDB, InstrumentExecutionConfig.Flamingos2]:
  override val subquery = gql"""
    {
      static {
        mosPreImaging
        useElectronicOffsetting
      }
      acquisition {
        nextAtom $Flamingos2AtomSubquery
        possibleFuture $Flamingos2AtomSubquery
        hasMore
      }
      science {
        nextAtom $Flamingos2AtomSubquery
        possibleFuture $Flamingos2AtomSubquery
        hasMore
      }
    }
  """

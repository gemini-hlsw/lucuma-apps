// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import clue.annotation.GraphQLType
import lucuma.schemas.ObservationDB

// The steps left in an execution sequence, the same selection for every instrument.
@GraphQL
@GraphQLType("GmosNorthExecutionSequence")
abstract class RemainingSequenceSubquery extends GraphQLSubquery[ObservationDB]:
  override val subquery = gql"""
    {
      nextAtom { steps $RemainingStepSubquery }
      possibleFuture { steps $RemainingStepSubquery }
    }
  """

@clue.annotation.GraphQLStub
object RemainingSequenceSubquery

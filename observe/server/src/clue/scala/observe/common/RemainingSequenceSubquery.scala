// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import clue.annotation.GraphQLStub
import clue.annotation.GraphQLType
import lucuma.schemas.ObservationDB

// The steps left in an execution sequence, validated against the sequence type of every instrument.
@GraphQL
@GraphQLType(
  "GmosNorthExecutionSequence",
  "GmosSouthExecutionSequence",
  "Flamingos2ExecutionSequence",
  "Igrins2ExecutionSequence",
  "GnirsExecutionSequence",
  "GhostExecutionSequence"
)
abstract class RemainingSequenceSubquery extends GraphQLSubquery[ObservationDB]:
  override val subquery = gql"""
    {
      nextAtom { steps $RemainingStepSubquery }
      possibleFuture { steps $RemainingStepSubquery }
    }
  """

@GraphQLStub
object RemainingSequenceSubquery

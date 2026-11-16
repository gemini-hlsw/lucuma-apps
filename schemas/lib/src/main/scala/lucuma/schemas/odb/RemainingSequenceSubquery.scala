// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import io.circe.Decoder
import lucuma.schemas.ObservationDB

// The steps left in an execution sequence, validated against the sequence type of every instrument.
@GraphQLType(
  "GmosNorthExecutionSequence",
  "GmosSouthExecutionSequence",
  "Flamingos2ExecutionSequence",
  "Igrins2ExecutionSequence",
  "GnirsExecutionSequence",
  "GhostExecutionSequence"
)
object RemainingSequenceSubquery extends GraphQLSubquery[ObservationDB]:
  case class Atom(steps: List[RemainingStepSubquery.Data]) derives Decoder
  case class Data(nextAtom: Atom, possibleFuture: List[Atom]) derives Decoder

  override val dataDecoder: Decoder[Data] = Decoder[Data]

  override val subquery = gql"""
    {
      nextAtom { steps $RemainingStepSubquery }
      possibleFuture { steps $RemainingStepSubquery }
    }
  """

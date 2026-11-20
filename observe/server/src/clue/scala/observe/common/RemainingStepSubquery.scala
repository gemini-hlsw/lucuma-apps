// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import clue.annotation.GraphQLStub
import clue.annotation.GraphQLType
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.*
// gql: import lucuma.odb.json.all.query.given

// What a step's kind and time need, validated against the step type of every instrument.
@GraphQL
@GraphQLType(
  "GmosNorthStep",
  "GmosSouthStep",
  "Flamingos2Step",
  "Igrins2Step",
  "GnirsStep",
  "GhostStep"
)
abstract class RemainingStepSubquery extends GraphQLSubquery[ObservationDB]:
  override val subquery = gql"""
    {
      stepConfig $StepConfigSubquery
      observeClass
      estimate {
        total $TimeSpanSubquery
      }
    }
  """

@GraphQLStub
object RemainingStepSubquery

// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import clue.annotation.GraphQLType
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.*
// gql: import lucuma.odb.json.stepconfig.given

// What a step's kind needs. Every instrument's step has these fields, so all decode to this Data.
@GraphQL
@GraphQLType("GmosNorthStep")
abstract class RemainingStepSubquery extends GraphQLSubquery[ObservationDB]:
  override val subquery = gql"""
    {
      stepConfig $StepConfigSubquery
      observeClass
    }
  """

@clue.annotation.GraphQLStub
object RemainingStepSubquery

// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import clue.annotation.GraphQL
import explore.model.Execution
import lucuma.schemas.ObservationDB

@GraphQL
@GraphQLType("Execution")
object ExecutionSubquery extends GraphQLSubquery.Typed[ObservationDB, Execution] {
  override val subquery = gql"""
    {
      digest $CalculatedDigestSubquery
      timeCharge $ProgramTimeSubquery
      acquisitionSequenceIsMaterialized
      scienceSequenceIsMaterialized
    }
  """
}

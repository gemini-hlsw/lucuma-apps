// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import clue.annotation.GraphQL
import explore.model.ProgramTimeRange
import lucuma.schemas.ObservationDB

@GraphQL
@GraphQLType("CategorizedTimeRange")
object ProgramTimeRangeSubquery extends GraphQLSubquery.Typed[ObservationDB, ProgramTimeRange]:
  override val subquery = gql"""
    {
      minimum $ProgramTimeSubquery
      maximum $ProgramTimeSubquery
    }
  """

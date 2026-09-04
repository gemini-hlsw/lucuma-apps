// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import clue.annotation.GraphQL
import explore.model.BandedProgramTime
import lucuma.schemas.ObservationDB

@GraphQL
@GraphQLType("BandedTime")
object BandedProgramTimeSubquery extends GraphQLSubquery.Typed[ObservationDB, BandedProgramTime]:
  override val subquery = gql"""
        {
          band
          time $ProgramTimeSubquery
        }
      """

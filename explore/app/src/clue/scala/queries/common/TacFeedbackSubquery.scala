// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import clue.annotation.GraphQL
import explore.model.TacFeedback
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.TimeSpanSubquery

@GraphQL
@GraphQLType("TacFeedback")
object TacFeedbackSubquery extends GraphQLSubquery.Typed[ObservationDB, TacFeedback]:
  override val subquery = gql"""
    {
      timeAccountingCategory
      decision
      ranking
      recommendedTimes {
        band
        time $TimeSpanSubquery
      }
      comments
    }
  """

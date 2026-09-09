// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import clue.annotation.GraphQLType
import explore.model.ProposalSummaryGeneration
import lucuma.schemas.ObservationDB

@GraphQL
@GraphQLType("ProposalSummaryGeneration")
object ProposalSummaryGenerationSubquery
    extends GraphQLSubquery.Typed[ObservationDB, ProposalSummaryGeneration]:
  override val subquery = gql"""
    {
      state
      requestedAt
      failures {
        partner
        message
      }
    }
  """

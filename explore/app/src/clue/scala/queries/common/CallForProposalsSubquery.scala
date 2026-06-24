// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import clue.annotation.GraphQL
import explore.model.CallForProposal
import lucuma.schemas.ObservationDB

@GraphQL
@GraphQLType("CallForProposals")
object CallForProposalsSubquery
    extends GraphQLSubquery.Typed[ObservationDB, CallForProposal]:
  override val subquery: String = s"""
    {
      id
      semester
      title
      observatory
      gemini {
        cfpType: type
        coordinateLimits {
          north $SiteCoordinatesLimitsSubquery
          south $SiteCoordinatesLimitsSubquery
        }
        instruments
        proprietaryMonths
        allowsNonPartnerPi
        nonPartnerDeadline
        exchangePartners
      }
      keck {
        instruments
        coordinateLimits $SiteCoordinatesLimitsSubquery
      }
      subaru {
        cfpType: type
        instruments
        coordinateLimits $SiteCoordinatesLimitsSubquery
      }
      active {
        start
        end
      }
      partners {
        partner: geminiPartner
        submissionDeadline
      }
    }
  """

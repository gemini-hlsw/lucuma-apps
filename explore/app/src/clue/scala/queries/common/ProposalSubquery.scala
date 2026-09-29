// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import clue.annotation.GraphQL
import explore.model.Proposal
import explore.model.PartnerSplit
import lucuma.schemas.ObservationDB

@GraphQL
@GraphQLType("Proposal")
object ProposalSubquery extends GraphQLSubquery.Typed[ObservationDB, Proposal]:
  override val subquery = gql"""
    {
      call $CallForProposalsSubquery
      category
      reference {
        label
      }
      gemini {
        scienceSubtype
        __typename
        ... on Classical {
          minPercentTime
          partnerSplits $PartnerSplitSubquery
          exchangePartner
          aeonMultiFacility {
            requiredInstruments
          }
          jwstSynergy
          usLongTerm
        }
        ... on DemoScience {
          minPercentTime
        }
        ... on DirectorsTime {
          minPercentTime
        }
        ... on FastTurnaround {
          minPercentTime
          reviewer { id }
          mentor { id }
        }
        ... on LargeProgram {
          minPercentTime
          minPercentTotalTime
          totalTime {
            hours
            minutes
          }
          aeonMultiFacility {
            requiredInstruments
          }
          jwstSynergy
        }
        ... on Queue {
          minPercentTime
          partnerSplits $PartnerSplitSubquery
          exchangePartner
          aeonMultiFacility {
            requiredInstruments
          }
          jwstSynergy
          usLongTerm
          considerForBand3
        }
        ... on SystemVerification {
          minPercentTime
        }
      }
      keck {
        minPercentTime
        partnerSplits $PartnerSplitSubquery
      }
      subaru {
        minPercentTime
        partnerSplits $PartnerSplitSubquery
      }
    }
  """

@GraphQL
@GraphQLType("PartnerSplit")
object PartnerSplitSubquery extends GraphQLSubquery.Typed[ObservationDB, PartnerSplit]:
  override val subquery = gql"""
    {
      partner
      percent
    }
  """

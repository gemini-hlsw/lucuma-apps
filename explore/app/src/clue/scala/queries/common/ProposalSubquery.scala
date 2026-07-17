// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
  override val subquery: String = s"""
    {
      call $CallForProposalsSubquery
      category
      reference {
        label
      }
      gemini {
        scienceSubtype
        ... on Classical {
          minPercentTime
          partnerSplits $PartnerSplitSubquery
          aeonMultiFacility
          jwstSynergy
          usLongTerm
        }
        ... on DirectorsTime {
          toOActivation
          minPercentTime
        }
        ... on FastTurnaround {
          toOActivation
          minPercentTime
          reviewer { id }
          mentor { id }
        }
        ... on LargeProgram {
          toOActivation
          minPercentTime
          minPercentTotalTime
          totalTime {
            hours
            minutes
          }
          aeonMultiFacility
          jwstSynergy
        }
        ... on Queue {
          toOActivation
          minPercentTime
          partnerSplits $PartnerSplitSubquery
          aeonMultiFacility
          jwstSynergy
          usLongTerm
          considerForBand3
        }
        ... on SystemVerification {
          toOActivation
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
  override val subquery: String = """
    {
      partner
      percent
    }
  """

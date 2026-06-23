// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import clue.annotation.GraphQL
import explore.model.ProgramUser
import lucuma.schemas.ObservationDB

@GraphQL
@GraphQLType("ProgramUser")
object ProgramUserSubquery extends GraphQLSubquery.Typed[ObservationDB, ProgramUser]:
  override val subquery: String = s"""
    {
      id
      user $UserSubquery
      partnerLink {
        linkType
        ... on HasGeminiPartner {
          geminiPartner
        }
        ... on HasExchangePartner {
          exchangePartner
        }
      }
      role
      educationalStatus
      thesis
      gender
      affiliation
      preferredProfile $UserProfileSubquery
      invitations $UserInvitationSubquery
      hasDataAccess
      classicalVisitor
    }
  """

// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.schemas

import clue.annotation.GraphQLSchema

// gql: import io.circe.refined.*

@GraphQLSchema
trait SSO:
  object Scalars:
    // Ids
    type UserId         = lucuma.core.model.User.Id
    type RoleId         = lucuma.core.model.StandardRole.Id
    type ApiKeyId       = String
    // Refined
    type NonEmptyString = eu.timepit.refined.types.string.NonEmptyString
    type NonNegInt      = eu.timepit.refined.types.numeric.NonNegInt

  object Enums:
    type RoleType = lucuma.core.enums.RoleType
    type Partner  = lucuma.core.enums.Partner

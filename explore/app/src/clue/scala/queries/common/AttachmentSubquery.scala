// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import clue.annotation.GraphQL
import explore.model.Attachment
import lucuma.schemas.ObservationDB

@GraphQL
@GraphQLType("Attachment")
object AttachmentSubquery extends GraphQLSubquery.Typed[ObservationDB, Attachment]:
  override val subquery = gql"""
    {
      id
      attachmentType
      fileName
      mask { name instrument }
      description
      checked
      fileSize
      updatedAt
    }
  """

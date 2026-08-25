// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import clue.annotation.GraphQLType
import explore.model.ArchiveDuplication
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.*

// The Archive Duplication header only: `matches` are fetched per observation when its row is
// expanded. See docs/adr/0007-archive-duplication-is-pulled-not-pushed.md.
@GraphQL
@GraphQLType("ArchiveDuplication")
object ArchiveDuplicationSubquery
    extends GraphQLSubquery.Typed[ObservationDB, ArchiveDuplication]:

  override val subquery = gql"""
    {
      state
      matchCount
      saturated
      lastCheckedAt
      error
      attemptedAt
      stale
      searchCoordinates $CoordinatesSubquery
      searchTargetName
      searchRadius $AngleSubquery
    }
  """

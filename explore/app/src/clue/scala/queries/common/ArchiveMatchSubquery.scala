// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import clue.annotation.GraphQLType
import explore.model.ArchiveMatch
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.*

@GraphQL
@GraphQLType("ArchiveMatch")
object ArchiveMatchSubquery extends GraphQLSubquery.Typed[ObservationDB, ArchiveMatch]:

  override val subquery = gql"""
    {
      name
      dataLabel
      coordinates $CoordinatesSubquery
      instrumentString
      instrument
      qaStateString
      utDateTime
      releaseDate
      programReference
      observationReference
      objectName
      exposure $TimeSpanSubquery
      disperser
      filter
      wavelength $WavelengthSubquery
      distance $AngleSubquery
    }
  """

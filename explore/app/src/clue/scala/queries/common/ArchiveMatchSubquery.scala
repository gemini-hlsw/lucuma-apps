// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import clue.annotation.GraphQLType
import explore.model.ArchiveMatch
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.*

// `observationType` is being removed from the ODB schema and is deliberately not selected. The
// focal plane mask is not carried by `ArchiveMatch` at all — see
// .scratch/archive-duplication-tile/issues/01-odb-archive-match-missing-fpu.md.
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

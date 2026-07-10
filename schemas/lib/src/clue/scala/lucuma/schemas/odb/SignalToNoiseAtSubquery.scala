// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import clue.annotation.GraphQL
import lucuma.itc.SignalToNoiseAt
import lucuma.itc.client.json.decoders.given
import lucuma.schemas.ObservationDB

@GraphQL
@GraphQLType("SignalToNoiseAt")
object SignalToNoiseAtSubquery extends GraphQLSubquery.Typed[ObservationDB, SignalToNoiseAt]:
  override val subquery: String = s"""
    {
      wavelength $WavelengthSubquery
      single
      total
    }
  """

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB
import lucuma.itc.SignalToNoiseAt
import lucuma.itc.client.json.decoders.given

@GraphQL
object SignalToNoiseAtSubquery
    extends GraphQLSubquery.Typed[ObservationDB, SignalToNoiseAt]("SignalToNoiseAt"):
  override val subquery: String = s"""
    {
      wavelength $WavelengthSubquery
      single
      total
    }
  """

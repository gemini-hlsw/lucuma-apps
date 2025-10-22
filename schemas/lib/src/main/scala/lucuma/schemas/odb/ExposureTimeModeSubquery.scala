// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import lucuma.core.model.ExposureTimeMode
import lucuma.schemas.ObservationDB
import lucuma.schemas.decoders.given
import lucuma.schemas.odb.*

@GraphQL
object ExposureTimeModeSubquery
    extends GraphQLSubquery.Typed[ObservationDB, ExposureTimeMode]("ExposureTimeMode"):
  override val subquery: String = s"""
    {
      signalToNoise {
        value
        at $WavelengthSubquery
      }
      timeAndCount {
        time $TimeSpanSubquery
        count
        at $WavelengthSubquery
      }
    }
  """

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import lucuma.core.math.WavelengthDither
import lucuma.schemas.ObservationDB
import lucuma.schemas.decoders.given

@GraphQLType("WavelengthDither")
object WavelengthDitherSubquery
    extends GraphQLSubquery.Typed[ObservationDB, WavelengthDither]:
  override val subquery: String = """
        {
          picometers
        }
      """

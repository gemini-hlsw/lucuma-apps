// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import lucuma.core.math.Declination
import lucuma.odb.json.declination.decoder.given
import lucuma.schemas.ObservationDB

@GraphQLType("Declination")
object DecSubquery extends GraphQLSubquery.Typed[ObservationDB, Declination]:
  override val subquery: String = """
        {
          microarcseconds
        }
      """

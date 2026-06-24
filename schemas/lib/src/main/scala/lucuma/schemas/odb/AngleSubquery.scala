// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import lucuma.core.math.Angle
import lucuma.odb.json.angle.decoder.given
import lucuma.schemas.ObservationDB

@GraphQLType("Angle")
object AngleSubquery extends GraphQLSubquery.Typed[ObservationDB, Angle]:
  override val subquery: String = """
        {
          microarcseconds
        }
      """

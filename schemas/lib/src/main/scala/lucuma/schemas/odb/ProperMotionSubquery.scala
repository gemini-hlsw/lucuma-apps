// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import lucuma.core.math.ProperMotion
import lucuma.odb.json.propermotion.decoder.given
import lucuma.schemas.ObservationDB

@GraphQLType("ProperMotion")
object ProperMotionSubquery extends GraphQLSubquery.Typed[ObservationDB, ProperMotion]:
  override val subquery: String = """
        {
          ra {
            microarcsecondsPerYear
          }
          dec {
            microarcsecondsPerYear
          }
        }
      """

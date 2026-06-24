// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import lucuma.core.math.RightAscension
import lucuma.odb.json.rightascension.decoder.given
import lucuma.schemas.ObservationDB

@GraphQLType("RightAscension")
object RASubquery extends GraphQLSubquery.Typed[ObservationDB, RightAscension]:
  override val subquery: String = """
        {
          microseconds
        }
      """

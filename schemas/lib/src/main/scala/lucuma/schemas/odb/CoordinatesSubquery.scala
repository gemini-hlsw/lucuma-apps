// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import lucuma.core.math.Coordinates
import lucuma.odb.json.coordinates.query.given
import lucuma.schemas.ObservationDB

object CoordinatesSubquery
    extends GraphQLSubquery.Typed[ObservationDB, Coordinates]("Coordinates") {
  override val subquery: String = s"""
    {
      ra $RASubquery
      dec $DecSubquery
    }     
  """.stripMargin
}

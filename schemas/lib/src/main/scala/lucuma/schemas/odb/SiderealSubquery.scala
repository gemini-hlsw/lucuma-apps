// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import lucuma.core.model.SiderealTracking
import lucuma.odb.json.target.decoder.given
import lucuma.schemas.ObservationDB

@GraphQLType("Sidereal")
object SiderealSubquery extends GraphQLSubquery.Typed[ObservationDB, SiderealTracking] {
  override val subquery: String = s"""
    {
      ra $RASubquery
      dec $DecSubquery
      epoch
      properMotion $ProperMotionSubquery
      radialVelocity $RadialVelocitySubquery
      parallax $AngleSubquery
      catalogInfo {
        name
        id
        objectType
      }
    }
  """
}

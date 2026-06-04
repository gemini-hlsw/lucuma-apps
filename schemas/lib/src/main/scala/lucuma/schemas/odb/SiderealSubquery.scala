// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import lucuma.schemas.ObservationDB
import lucuma.core.model.Target.Sidereal
import lucuma.odb.json.target.decoder.given 

object SiderealSubquery extends GraphQLSubquery.Typed[ObservationDB, Sidereal]("Sidereal") {
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

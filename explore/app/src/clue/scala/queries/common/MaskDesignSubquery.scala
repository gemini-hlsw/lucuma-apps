// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import clue.annotation.GraphQLType
import explore.model.MaskDesign
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.AngleSubquery
import lucuma.schemas.odb.CoordinatesSubquery

@GraphQL
@GraphQLType("MaskDefinition")
object MaskDesignSubquery extends GraphQLSubquery.Typed[ObservationDB, MaskDesign]:
  override val subquery = gql"""
    {
      name
      instrument
      pointing $CoordinatesSubquery
      positionAngle $AngleSubquery
      slits {
        id
        coordinates $CoordinatesSubquery
        x
        y
        width $AngleSubquery
        length $AngleSubquery
        offsetAlongSlit $AngleSubquery
        offsetAcrossSlit $AngleSubquery
        tilt $AngleSubquery
        priority
      }
    }
  """

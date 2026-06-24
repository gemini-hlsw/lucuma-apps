// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB
// gql: import io.circe.refined.*

@GraphQL
@GraphQLType("FluxDensityEntry")
abstract class FluxDensitySubquery extends GraphQLSubquery[ObservationDB]:
  // FIXME Replace wavelength when subqueries can contain subqueries
  override val subquery: String = """
        {
          wavelength {
            picometers
          }
          density
        }
      """

@clue.annotation.GraphQLStub
object FluxDensitiesSubquery

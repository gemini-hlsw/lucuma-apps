// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import clue.annotation.GraphQL
import lucuma.core.model.sequence.StepConfig
import lucuma.odb.json.stepconfig.given
import lucuma.schemas.ObservationDB

@GraphQL
@GraphQLType("StepConfig")
object StepConfigSubquery extends GraphQLSubquery.Typed[ObservationDB, StepConfig]:
  override val subquery: String = """
        {
          stepType
          ... on Gcal {
            continuum
            arcs
            filter
            diffuser
            shutter
          }
          ... on SmartGcal {
            smartGcalType
          }
        }
      """

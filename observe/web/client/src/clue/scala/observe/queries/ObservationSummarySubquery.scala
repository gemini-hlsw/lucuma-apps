// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.queries

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import observe.ui.model.ObsSummary
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.*
import clue.annotation.GraphQL

@GraphQL
@GraphQLType("Observation")
object ObservationSummarySubquery extends GraphQLSubquery.Typed[ObservationDB, ObsSummary]:

  override val subquery: String = s"""
        {
          id
          program { id }
          title
          subtitle
          instrument
          observationTime
          calibrationRole
          posAngleConstraint $PosAngleConstraintSubquery
          constraintSet $ConstraintSetSubquery
          attachments { id }
          observingMode $ObservingModeSubquery
          reference { label }
          workflow {
            value {
              state
            }
          }
        }
      """

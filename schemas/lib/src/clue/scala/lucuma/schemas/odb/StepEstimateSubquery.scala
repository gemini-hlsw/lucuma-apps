// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.annotation.GraphQL
import clue.GraphQLSubquery
import lucuma.core.model.sequence.StepEstimate
import lucuma.schemas.ObservationDB
import lucuma.odb.json.timeaccounting.given

@GraphQL
object StepEstimateSubquery
    extends GraphQLSubquery.Typed[ObservationDB, StepEstimate]("StepEstimate"):
  override val subquery: String = """
        {
          configChange {
            all {
              name
              description
              estimate { microseconds }
            }
            index
          }
          detector {
            all {
              name
              description
              dataset {
                exposure { microseconds }
                readout { microseconds }
                write { microseconds }
              }
              count
            }
            index
          }
        }
      """

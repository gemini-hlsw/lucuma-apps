// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import clue.annotation.GraphQLType
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.util.CalculatedValue
import lucuma.odb.json.calculatedValue.given
import lucuma.odb.json.sequence.given
import lucuma.schemas.ObservationDB

@GraphQL
@GraphQLType("CalculatedExecutionDigest")
object CalculatedExecutionDigestSubquery
    extends GraphQLSubquery.Typed[ObservationDB, CalculatedValue[Option[ExecutionDigest]]]:
  override val subquery = gql"""
    {
      calculationState
      value {
        estimate {
          setup {
            full $TimeSpanSubquery
            reacquisition $TimeSpanSubquery
          }
          setupCount
          reacquisitionCount
          calibrations {
            existing {
              count
              time {
                program $TimeSpanSubquery
                nonCharged $TimeSpanSubquery
                total $TimeSpanSubquery
              }
            }
            expected {
              count
              time {
                program $TimeSpanSubquery
                nonCharged $TimeSpanSubquery
                total $TimeSpanSubquery
              }
            }
          }
        }
        acquisition $SequenceDigestSubquery
        science $SequenceDigestSubquery
      }
    }
  """

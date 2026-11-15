// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import clue.annotation.GraphQL
import explore.model.ObservationDigest
import lucuma.core.util.CalculatedValue
import lucuma.schemas.ObservationDB
import lucuma.odb.json.calculatedValue.given
import lucuma.schemas.odb.SequenceDigestSubquery
import lucuma.schemas.odb.TimeSpanSubquery

@GraphQL
@GraphQLType("CalculatedExecutionDigest")
object CalculatedDigestSubquery
    extends GraphQLSubquery.Typed[ObservationDB, CalculatedValue[Option[ObservationDigest]]]:
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
          total {
            program $TimeSpanSubquery
          }
        }
        acquisition $SequenceDigestSubquery
        science $SequenceDigestSubquery
      }
    }
  """

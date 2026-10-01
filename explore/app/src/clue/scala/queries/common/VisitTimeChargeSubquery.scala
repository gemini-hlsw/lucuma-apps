// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import clue.annotation.GraphQLType
import explore.model.VisitTimeCharge
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.*

@GraphQL
@GraphQLType("Visit")
object VisitTimeChargeSubquery extends GraphQLSubquery.Typed[ObservationDB, VisitTimeCharge]:
  override val subquery = gql"""
    {
      id
      site
      interval $TimestampIntervalSubquery
      timeChargeInvoice {
        executionTime {
          program $TimeSpanSubquery
          nonCharged $TimeSpanSubquery
        }
        discounts {
          __typename
          interval $TimestampIntervalSubquery
          amount $TimeSpanSubquery
          comment
        }
        corrections {
          chargeClass
          op
          amount $TimeSpanSubquery
          comment
        }
        finalCharge {
          program $TimeSpanSubquery
          nonCharged $TimeSpanSubquery
        }
      }
    }
  """

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import clue.annotation.GraphQL
import lucuma.core.util.TimestampInterval
import lucuma.schemas.ObservationDB
import lucuma.schemas.decoders.given

@GraphQL
@GraphQLType("TimestampInterval")
object TimestampIntervalSubquery extends GraphQLSubquery.Typed[ObservationDB, TimestampInterval]:
  override val subquery: String = """
        {
          start
          end
        }
      """

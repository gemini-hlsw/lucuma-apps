// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.odb.json.igrins2.given
import lucuma.schemas.ObservationDB

@GraphQL
object Igrins2DynamicConfigSubquery
    extends GraphQLSubquery.Typed[ObservationDB, Igrins2DynamicConfig]("Igrins2Dynamic"):
  override val subquery: String = s"""
    {
      exposure $TimeSpanSubquery
    }
  """

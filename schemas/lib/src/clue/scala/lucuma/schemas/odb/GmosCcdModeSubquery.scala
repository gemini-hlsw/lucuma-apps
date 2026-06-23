// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB

@GraphQL
@GraphQLType("GmosCcdMode")
abstract class GmosCcdModeSubquery extends GraphQLSubquery[ObservationDB]:
  override val subquery: String = """
        {
          xBin
          yBin
          ampCount
          ampGain
          ampReadMode
        }
      """

@clue.annotation.GraphQLStub
object GmosCcdModeSubquery

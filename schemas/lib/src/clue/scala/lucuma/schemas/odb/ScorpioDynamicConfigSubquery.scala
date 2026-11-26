// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import clue.annotation.GraphQL
import lucuma.core.model.sequence.scorpio.ScorpioDynamicConfig
import lucuma.odb.json.scorpio.given
import lucuma.schemas.ObservationDB

@GraphQL
@GraphQLType("ScorpioDynamic")
object ScorpioDynamicConfigSubquery
    extends GraphQLSubquery.Typed[ObservationDB, ScorpioDynamicConfig]:
  override val subquery = gql"""
    {
      g { exposureTime $TimeSpanSubquery exposureCount readoutMode }
      r { exposureTime $TimeSpanSubquery exposureCount readoutMode }
      i { exposureTime $TimeSpanSubquery exposureCount readoutMode }
      z { exposureTime $TimeSpanSubquery exposureCount readoutMode }
      fpu
      disperser
    }
  """

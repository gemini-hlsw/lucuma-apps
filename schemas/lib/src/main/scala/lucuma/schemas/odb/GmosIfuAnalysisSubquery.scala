// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import lucuma.core.model.GmosIfuAnalysis
import lucuma.schemas.ObservationDB
import lucuma.schemas.decoders.given

@GraphQL
object GmosIfuAnalysisSubquery extends GraphQLSubquery.Typed[ObservationDB, GmosIfuAnalysis]:
  override val subquery = gql"""
        {
          sumRadius $AngleSubquery
          singleOffset $AngleSubquery
        }
      """

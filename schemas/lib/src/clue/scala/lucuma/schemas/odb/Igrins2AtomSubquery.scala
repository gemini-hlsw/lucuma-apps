// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import clue.annotation.GraphQL
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.odb.json.igrins2.given
import lucuma.odb.json.sequence.given
import lucuma.schemas.ObservationDB

@GraphQL
@GraphQLType("Igrins2Atom")
object Igrins2AtomSubquery
    extends GraphQLSubquery.Typed[ObservationDB, Atom[Igrins2DynamicConfig]]:
  override val subquery: String = s"""
        {
          id
          description
          steps {
            id
            instrumentConfig $Igrins2DynamicConfigSubquery
            stepConfig $StepConfigSubquery
            telescopeConfig $TelescopeConfigSubquery
            estimate $StepEstimateSubquery
            observeClass
            breakpoint
          }
        }
      """

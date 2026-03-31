// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.annotation.GraphQL
import clue.GraphQLSubquery
import lucuma.schemas.ObservationDB
import lucuma.odb.json.sequence.given
import lucuma.odb.json.igrins2.given
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig

@GraphQL
object Igrins2AtomSubquery
    extends GraphQLSubquery.Typed[ObservationDB, Atom[Igrins2DynamicConfig]](
      "Igrins2Atom"
    ):
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

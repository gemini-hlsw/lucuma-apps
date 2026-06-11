// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.odb.json.ghost.decoder.given
import lucuma.odb.json.sequence.given
import lucuma.schemas.ObservationDB

@GraphQL
object GhostAtomSubquery
    extends GraphQLSubquery.Typed[ObservationDB, Atom[GhostDynamicConfig]]("GhostAtom"):

  override val subquery: String =
    s"""
        {
          id
          description
          steps {
            id
            instrumentConfig $GhostDynamicConfigSubquery
            stepConfig $StepConfigSubquery
            telescopeConfig $TelescopeConfigSubquery
            estimate $StepEstimateSubquery
            observeClass
            breakpoint
          }
        }
      """

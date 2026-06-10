// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.odb.json.gnirs.given
import lucuma.odb.json.sequence.given
import lucuma.schemas.ObservationDB

@GraphQL
object GnirsAtomSubquery
    extends GraphQLSubquery.Typed[ObservationDB, Atom[GnirsDynamicConfig]](
      "GnirsAtom"
    ):
  override val subquery: String = s"""
        {
          id
          description
          steps {
            id
            instrumentConfig $GnirsDynamicConfigSubquery
            stepConfig $StepConfigSubquery
            telescopeConfig $TelescopeConfigSubquery
            estimate $StepEstimateSubquery
            observeClass
            breakpoint
          }
        }
      """

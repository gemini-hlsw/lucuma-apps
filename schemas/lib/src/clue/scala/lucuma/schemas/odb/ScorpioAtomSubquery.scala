// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import clue.annotation.GraphQL
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.scorpio.ScorpioDynamicConfig
import lucuma.odb.json.scorpio.given
import lucuma.odb.json.sequence.given
import lucuma.schemas.ObservationDB

@GraphQL
@GraphQLType("ScorpioAtom")
object ScorpioAtomSubquery extends GraphQLSubquery.Typed[ObservationDB, Atom[ScorpioDynamicConfig]]:
  override val subquery = gql"""
        {
          id
          description
          steps {
            id
            instrumentConfig $ScorpioDynamicConfigSubquery
            stepConfig $StepConfigSubquery
            telescopeConfig $TelescopeConfigSubquery
            estimate $StepEstimateSubquery
            observeClass
            breakpoint
          }
        }
      """

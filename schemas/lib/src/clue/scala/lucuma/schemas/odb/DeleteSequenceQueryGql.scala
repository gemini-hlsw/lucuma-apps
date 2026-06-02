// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB

object DeleteSequenceQueryGql:
  @GraphQL
  trait DeleteSequence extends GraphQLOperation[ObservationDB]:
    val document = s"""
      mutation($$obsId: ObservationId!) {
        deleteSequence(input: { observationId: $$obsId }) {
          observation {
            id
          }
        }
      }
    """

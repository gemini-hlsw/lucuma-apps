// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB

// gql: import io.circe.refined.given

// Masks of every program, for daytime calibrations. Staff only: others get an empty list.
object MosMaskQueriesGQL:
  @GraphQL
  trait MosMasks extends GraphQLOperation[ObservationDB]:
    val document = gql"""
      query($$instrument: Instrument!, $$OFFSET_AFTER: AttachmentId) {
        mosMasks(instruments: [$$instrument], OFFSET_AFTER: $$OFFSET_AFTER) {
          matches {
            id
            mask {
              name
            }
          }
          hasMore
        }
      }
    """

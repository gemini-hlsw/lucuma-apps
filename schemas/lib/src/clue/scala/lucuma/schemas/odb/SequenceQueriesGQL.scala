// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB
// gql: import lucuma.odb.json.sequence.given
// gql: import lucuma.odb.json.timeaccounting.given
// gql: import lucuma.schemas.decoders.given

object SequenceQueriesGql:
  @GraphQL
  trait SequenceQuery extends GraphQLOperation[ObservationDB]:
    val document = s"""
        query($$obsId: ObservationId!, $$includeItc: Boolean!) {
          observation(observationId: $$obsId) @include(if: $$includeItc) {
            signalToNoise:itc $ModeSignalToNoiseSubquery
          }

          executionConfig(observationId: $$obsId, futureLimit: 100) $ExecutionConfigSubquery
        }
      """

  // query for the per-detector time estimate of a GHOST science step. Just one atom
  @GraphQL
  trait GhostScienceStepEstimateQuery extends GraphQLOperation[ObservationDB]:
    val document = s"""
        query($$obsId: ObservationId!) {
          executionConfig(observationId: $$obsId, futureLimit: 0) {
            ghost {
              science {
                nextAtom {
                  steps {
                    estimate $StepEstimateSubquery
                  }
                }
              }
            }
          }
        }
      """

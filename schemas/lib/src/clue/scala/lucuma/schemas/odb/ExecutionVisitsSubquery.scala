// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB
import lucuma.schemas.decoders.given
import lucuma.schemas.model.ExecutionVisits

@GraphQL
object ExecutionVisitsSubquery
    extends GraphQLSubquery.Typed[ObservationDB, Option[ExecutionVisits]]("Execution"):
  override val subquery: String = s"""
    {
      visits(OFFSET: $$visitIdOffset) {
        matches {
          id
          instrument
          created
          interval $TimestampIntervalSubquery
          atomRecords {
            matches {
              id
              executionState
              interval $TimestampIntervalSubquery
              sequenceType
              steps {
                matches {
                  id
                  executionState
                  interval $TimestampIntervalSubquery
                  stepConfig {
                    stepType
                    ... on Gcal {
                      continuum
                      arcs
                      filter
                      diffuser
                      shutter
                    }
                  }
                  telescopeConfig {
                    offset $OffsetSubquery
                    guiding
                  }
                  observeClass
                  qaState
                  datasets {
                    matches {
                      id
                      index
                      filename
                      qaState
                      comment
                      interval $TimestampIntervalSubquery
                      isWritten
                    }
                  }
                  gmosNorth $GmosNorthDynamicConfigSubquery
                  gmosSouth $GmosSouthDynamicConfigSubquery
                  flamingos2 $Flamingos2DynamicConfigSubquery
                  igrins2 $Igrins2DynamicConfigSubquery
                  ghost $GhostDynamicConfigSubquery
                }
              }
            }
          }
        }
      }
    }
  """

  // At some point we hope to support properly having fragments in subQueries, but currently these
  // fragments need to be redeclared where this subquery is used...
  val Fragments = s"""
    fragment nodAndShuffleFields on GmosNodAndShuffle {
      posA $OffsetSubquery
      posB $OffsetSubquery
      eOffset
      shuffleOffset
      shuffleCycles
    }
  """

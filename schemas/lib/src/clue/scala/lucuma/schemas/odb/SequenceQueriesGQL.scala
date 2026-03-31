// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.core.model.sequence.*
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.OffsetSubquery
// gql: import lucuma.odb.json.sequence.given
// gql: import lucuma.schemas.decoders.given

object SequenceQueriesGql:
  @GraphQL
  trait SequenceQuery extends GraphQLOperation[ObservationDB]:
    val document = s"""
        query($$obsId: ObservationId!, $$includeItc: Boolean = true) {
          observation(observationId: $$obsId) @include(if: $$includeItc) {
            signalToNoise:itc $ModeSignalToNoiseSubquery
          }

          executionConfig(observationId: $$obsId, futureLimit: 100) {
            instrument
            gmosNorth {
              static {
                stageMode
                detector
                mosPreImaging
                nodAndShuffle { ...nodAndShuffleFields }
              }
              acquisition { ...gmosNorthSequenceFields }
              science { ...gmosNorthSequenceFields }
            }
            gmosSouth {
              static {
                stageMode
                detector
                mosPreImaging
                nodAndShuffle { ...nodAndShuffleFields }
              }
              acquisition { ...gmosSouthSequenceFields }
              science { ...gmosSouthSequenceFields }
            }
            flamingos2 {
              static {
                mosPreImaging
                useElectronicOffsetting
              }
              acquisition { ...flamingos2SequenceFields }
              science { ...flamingos2SequenceFields }
            }
            igrins2 {
              static {
                saveSVCImages
                offsetMode
              }
              science { ...igrins2SequenceFields }
            }
          }
        }

        fragment nodAndShuffleFields on GmosNodAndShuffle {
          posA $OffsetSubquery
          posB $OffsetSubquery
          eOffset
          shuffleOffset
          shuffleCycles
        }

        fragment gmosNorthSequenceFields on GmosNorthExecutionSequence {
          nextAtom $GmosNorthAtomSubquery
          possibleFuture $GmosNorthAtomSubquery
          hasMore
        }

        fragment gmosSouthSequenceFields on GmosSouthExecutionSequence {
          nextAtom $GmosSouthAtomSubquery
          possibleFuture $GmosSouthAtomSubquery
          hasMore
        }

        fragment flamingos2SequenceFields on Flamingos2ExecutionSequence {
          nextAtom $Flamingos2AtomSubquery
          possibleFuture $Flamingos2AtomSubquery
          hasMore
        }

        fragment igrins2SequenceFields on Igrins2ExecutionSequence {
          nextAtom $Igrins2AtomSubquery
          possibleFuture $Igrins2AtomSubquery
          hasMore
        }
      """

    object Data:
      type ExecutionConfig = InstrumentExecutionConfig

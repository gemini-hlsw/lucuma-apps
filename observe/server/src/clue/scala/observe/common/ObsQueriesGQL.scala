// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB
import lucuma.core.model
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.schemas.odb.*
// gql: import io.circe.refined.*
// gql: import lucuma.schemas.decoders.given
// gql: import lucuma.odb.json.all.query.given

object ObsQueriesGql:

  @GraphQL
  trait ObsQuery extends GraphQLOperation[ObservationDB]:
    val document = s"""
      query($$obsId: ObservationId!) {
        observation(observationId: $$obsId) {
          id
          title
          observationTime
          program {
            id
            name
            goa { proprietaryMonths }
          }
          targetEnvironment {
            asterism $TargetWithIdSubquery
            firstScienceTarget {
              targetId: id
              targetName: name
            }
            basePosition {
              type
              name
              sidereal $SiderealSubquery
              nonsidereal {
                des
                keyType
                key
              }
              coordinates $CoordinatesSubquery
            }
            guideEnvironment {
              guideTargets { probe }
            }
            explicitBase $CoordinatesSubquery
          }
          constraintSet $ConstraintSetSubquery
          timingWindows $TimingWindowSubquery
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
          gnirs {
            static {
              wellDepth
            }
            acquisition { ...gnirsSequenceFields }
            science { ...gnirsSequenceFields }
          }
          ghost {
            static {
              resolutionMode
              ifuMapping $GhostIfuMappingSubquery
              slitViewingCameraExposureTime $TimeSpanSubquery
            }
            science { ...ghostSequenceFields }
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

      fragment gnirsSequenceFields on GnirsExecutionSequence {
        nextAtom $GnirsAtomSubquery
        possibleFuture $GnirsAtomSubquery
        hasMore
      }

      fragment ghostSequenceFields on GhostExecutionSequence {
        nextAtom $GhostAtomSubquery
        possibleFuture $GhostAtomSubquery
        hasMore
      }
    """

    object Data:
      object Observation:
        type ConstraintSet = model.ConstraintSet
        type TimingWindows = model.TimingWindow
        object TargetEnvironment {
          type ExplicitBase = lucuma.core.math.Coordinates
        }
      type ExecutionConfig = InstrumentExecutionConfig

  @GraphQL
  trait ResetAcquisitionMutation extends GraphQLOperation[ObservationDB]:
    val document = """
      mutation($obsId: ObservationId!) {
        resetAcquisition(input: { observationId: $obsId } ) {
          observation { id }
        }
      }
      """

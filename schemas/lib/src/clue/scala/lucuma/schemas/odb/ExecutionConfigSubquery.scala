// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.odb.json.sequence.given
import lucuma.schemas.ObservationDB

// Clue does not support fragment definitions inside subqueries, so the per-sequence
// fields (`nextAtom`/`possibleFuture`/`hasMore`) and the nod & shuffle fields are inlined.
@GraphQL
object ExecutionConfigSubquery
    extends GraphQLSubquery.Typed[ObservationDB, InstrumentExecutionConfig]("ExecutionConfig"):
  override val subquery: String = s"""
    {
      instrument
      gmosNorth {
        static {
          stageMode
          detector
          mosPreImaging
          nodAndShuffle {
            posA $OffsetSubquery
            posB $OffsetSubquery
            eOffset
            shuffleOffset
            shuffleCycles
          }
        }
        acquisition {
          nextAtom $GmosNorthAtomSubquery
          possibleFuture $GmosNorthAtomSubquery
          hasMore
        }
        science {
          nextAtom $GmosNorthAtomSubquery
          possibleFuture $GmosNorthAtomSubquery
          hasMore
        }
      }
      gmosSouth {
        static {
          stageMode
          detector
          mosPreImaging
          nodAndShuffle {
            posA $OffsetSubquery
            posB $OffsetSubquery
            eOffset
            shuffleOffset
            shuffleCycles
          }
        }
        acquisition {
          nextAtom $GmosSouthAtomSubquery
          possibleFuture $GmosSouthAtomSubquery
          hasMore
        }
        science {
          nextAtom $GmosSouthAtomSubquery
          possibleFuture $GmosSouthAtomSubquery
          hasMore
        }
      }
      flamingos2 {
        static {
          mosPreImaging
          useElectronicOffsetting
        }
        acquisition {
          nextAtom $Flamingos2AtomSubquery
          possibleFuture $Flamingos2AtomSubquery
          hasMore
        }
        science {
          nextAtom $Flamingos2AtomSubquery
          possibleFuture $Flamingos2AtomSubquery
          hasMore
        }
      }
      igrins2 {
        static {
          saveSVCImages
          offsetMode
        }
        science {
          nextAtom $Igrins2AtomSubquery
          possibleFuture $Igrins2AtomSubquery
          hasMore
        }
      }
      gnirs {
        static {
          wellDepth
        }
        acquisition {
          nextAtom $GnirsAtomSubquery
          possibleFuture $GnirsAtomSubquery
          hasMore
        }
        science {
          nextAtom $GnirsAtomSubquery
          possibleFuture $GnirsAtomSubquery
          hasMore
        }
      }
      ghost {
        static {
          resolutionMode
          ifuMapping $GhostIfuMappingSubquery
          slitViewingCameraExposureTime $TimeSpanSubquery
        }
        science {
          nextAtom $GhostAtomSubquery
          possibleFuture $GhostAtomSubquery
          hasMore
        }
      }
    }
  """

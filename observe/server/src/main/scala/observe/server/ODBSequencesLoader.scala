// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import cats.Endo
import cats.syntax.all.*
import lucuma.core.enums.SequenceType
import lucuma.core.model.sequence.ExecutionConfig
import lucuma.core.model.sequence.InstrumentExecutionConfig
import monocle.Lens
import observe.model.Observer
import observe.model.SystemOverrides
import observe.server.engine.Breakpoints
import observe.server.engine.SequenceState
import observe.server.odb.OdbObservationData

object ODBSequencesLoader {
  private def initialSequenceType[S, D](ec: ExecutionConfig[S, D]): SequenceType =
    ec.acquisition.fold(SequenceType.Science)(_ => SequenceType.Acquisition)

  /**
   * Create a new SequenceData for a freshly loaded observation. Constructs the appropriate
   * instrument-specific subclass based on the execution config.
   */
  private[server] def loadSequenceMod[F[_]](
    observer:               Option[Observer],
    odbData:                OdbObservationData,
    instrumentSequenceLens: Lens[EngineState[F], Option[SequenceData[F]]]
  ): Endo[EngineState[F]] = st =>
    val (initialBreakpoints, seqType): (Breakpoints, SequenceType) =
      odbData.executionConfig match
        case InstrumentExecutionConfig.GmosNorth(ec)  =>
          (Breakpoints.fromExecutionConfig(ec), initialSequenceType(ec))
        case InstrumentExecutionConfig.GmosSouth(ec)  =>
          (Breakpoints.fromExecutionConfig(ec), initialSequenceType(ec))
        case InstrumentExecutionConfig.Flamingos2(ec) =>
          (Breakpoints.fromExecutionConfig(ec), initialSequenceType(ec))
        case InstrumentExecutionConfig.Igrins2(ec)    =>
          (Breakpoints.fromExecutionConfig(ec), initialSequenceType(ec))
        case InstrumentExecutionConfig.Ghost(ec)      =>
          (Breakpoints.fromExecutionConfig(ec), initialSequenceType(ec))

    val seqState: SequenceState[F] =
      SequenceState.init(odbData.observation.id, seqType, initialBreakpoints)

    val seqData: SequenceData[F] = odbData.executionConfig match
      case InstrumentExecutionConfig.GmosNorth(ec)  =>
        SequenceData.GmosNorth(
          observer = observer,
          overrides = SystemOverrides.AllEnabled,
          targetEnvironment = odbData.observation.targetEnvironment,
          constraintSet = odbData.observation.constraintSet,
          staticCfg = ec.static,
          seq = seqState,
          pendingObsCmd = none,
          visitStartDone = false
        )
      case InstrumentExecutionConfig.GmosSouth(ec)  =>
        SequenceData.GmosSouth(
          observer = observer,
          overrides = SystemOverrides.AllEnabled,
          targetEnvironment = odbData.observation.targetEnvironment,
          constraintSet = odbData.observation.constraintSet,
          staticCfg = ec.static,
          seq = seqState,
          pendingObsCmd = none,
          visitStartDone = false
        )
      case InstrumentExecutionConfig.Flamingos2(ec) =>
        SequenceData.Flamingos2(
          observer = observer,
          overrides = SystemOverrides.AllEnabled,
          targetEnvironment = odbData.observation.targetEnvironment,
          constraintSet = odbData.observation.constraintSet,
          staticCfg = ec.static,
          seq = seqState,
          pendingObsCmd = none,
          visitStartDone = false
        )
      case InstrumentExecutionConfig.Igrins2(ec)    =>
        SequenceData.Igrins2(
          observer = observer,
          overrides = SystemOverrides.AllEnabled,
          targetEnvironment = odbData.observation.targetEnvironment,
          constraintSet = odbData.observation.constraintSet,
          staticCfg = ec.static,
          seq = seqState,
          pendingObsCmd = none,
          visitStartDone = false
        )
      case InstrumentExecutionConfig.Ghost(_)       => ???

    instrumentSequenceLens.replace(seqData.some)(st)

  /**
   * Reload the step definition for an existing sequence. Preserves observer, overrides, etc.
   */
  private[server] def reloadSequenceMod[F[_]](
    stepGen: Option[StepGen[F]],
    l:       Lens[EngineState[F], Option[SequenceData[F]]]
  ): Endo[EngineState[F]] = st =>
    l.some
      .modify { sd =>
        val headerExtra = HeaderExtraData(st.conditions, st.operator, sd.observer)
        val newSeqState = SequenceState.reload(stepGen, sd.overrides, headerExtra, sd.seq)
        SequenceData.seq.replace(newSeqState)(sd)
      }(st)

}

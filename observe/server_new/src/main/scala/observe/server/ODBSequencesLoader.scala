// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import cats.Endo
import cats.syntax.all.*
import lucuma.core.model.sequence.InstrumentExecutionConfig
import monocle.Lens
import observe.model.Observer
import observe.model.SystemOverrides
import observe.server.engine.Breakpoints
import observe.server.engine.Engine
import observe.server.engine.Sequence
import observe.server.engine.SequenceState
import observe.server.odb.OdbObservationData

object ODBSequencesLoader {

  /**
   * Build the engine step from a StepGen, generating the breakpoint delta.
   */
  // private def buildEngineStep[F[_]](
  //   stepGen:              StepGen[F],
  //   overrides:            SystemOverrides,
  //   headerExtra:          HeaderExtraData,
  //   preservedBreakpoints: Breakpoints
  // ): (engine.EngineStep[F], Breakpoints) =
  //   val (engineStep, breakpoint) = generateStep(stepGen, overrides, headerExtra)
  //   val breakpoints              =
  //     preservedBreakpoints.merge(
  //       BreakpointsDelta.fromStepsWithBreakpoints(List((engineStep, breakpoint)))
  //     )
  //   (engineStep, breakpoints)

  /**
   * Create a new SequenceData for a freshly loaded observation. Constructs the appropriate
   * instrument-specific subclass based on the execution config.
   */
  private[server] def loadSequenceMod[F[_]](
    observer:               Option[Observer],
    odbData:                OdbObservationData,
    instrumentSequenceLens: Lens[EngineState[F], Option[SequenceData[F]]],
    cleanup:                F[Unit]
  ): Endo[EngineState[F]] = st =>
    val initialBreakpoints: Breakpoints =
      odbData.executionConfig match
        case InstrumentExecutionConfig.GmosNorth(ec)  => Breakpoints.fromExecutionConfig(ec)
        case InstrumentExecutionConfig.GmosSouth(ec)  => Breakpoints.fromExecutionConfig(ec)
        case InstrumentExecutionConfig.Flamingos2(ec) => Breakpoints.fromExecutionConfig(ec)
        case InstrumentExecutionConfig.Igrins2(_)     =>
          sys.error("Igrins2 is not supported")

    val seqState: SequenceState[F] =
      Engine.initialSequenceState:
        Sequence(odbData.observation.id, none, initialBreakpoints)

    val seqData: SequenceData[F] = odbData.executionConfig match
      case InstrumentExecutionConfig.GmosNorth(ec)  =>
        SequenceData.GmosNorth(
          observer = observer,
          overrides = SystemOverrides.AllEnabled,
          obsData = odbData.observation,
          staticCfg = ec.static,
          // currentStep = stepGen.collect { case StepGen.gmosNorth(gn) => gn },
          currentStep = none,
          seq = seqState,
          pendingObsCmd = none,
          visitStartDone = false,
          cleanup = cleanup
        )
      case InstrumentExecutionConfig.GmosSouth(ec)  =>
        SequenceData.GmosSouth(
          observer = observer,
          overrides = SystemOverrides.AllEnabled,
          obsData = odbData.observation,
          staticCfg = ec.static,
          // currentStep = stepGen.collect { case StepGen.gmosSouth(gs) => gs },
          currentStep = none,
          seq = seqState,
          pendingObsCmd = none,
          visitStartDone = false,
          cleanup = cleanup
        )
      case InstrumentExecutionConfig.Flamingos2(ec) =>
        SequenceData.Flamingos2(
          observer = observer,
          overrides = SystemOverrides.AllEnabled,
          obsData = odbData.observation,
          staticCfg = ec.static,
          // currentStep = stepGen.collect { case StepGen.flamingos2(f2) => f2 },
          currentStep = none,
          seq = seqState,
          pendingObsCmd = none,
          visitStartDone = false,
          cleanup = cleanup
        )
      case InstrumentExecutionConfig.Igrins2(_)     =>
        sys.error("Igrins2 is not supported")

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
        val engineStep  = stepGen.map: sg =>
          generateStep(sg, sd.overrides, headerExtra)._1
        val newSeqState = Engine.reload(sd.seq, engineStep)
        SequenceData.seq.replace(newSeqState)(sd)
      }(st)

}

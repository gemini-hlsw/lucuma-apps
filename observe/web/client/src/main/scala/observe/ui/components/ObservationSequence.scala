// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.components

import crystal.react.*
import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.*
import lucuma.core.enums.Breakpoint
import lucuma.core.model.Attachment
import lucuma.core.model.Observation
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.model.sequence.Step
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Message
import lucuma.schemas.model.ExecutionVisits
import lucuma.schemas.model.ItcResultValues
import lucuma.schemas.model.ModeSignalToNoise
import lucuma.ui.sequence.SelectedRowId
import lucuma.ui.sequence.SequenceData
import observe.model.ExecutionState
import observe.model.StepProgress
import observe.model.odb.RecordedVisit
import observe.ui.components.sequence.byInstrument.*
import observe.ui.model.AppContext
import observe.ui.model.ObservationRequests
import observe.ui.model.enums.ClientMode
import observe.ui.services.SequenceApi
import observe.ui.utils.*

case class ObservationSequence(
  obsId:                Observation.Id,
  sequenceData:         SequenceData,
  visits:               View[Option[ExecutionVisits]],
  executionState:       View[ExecutionState],
  currentRecordedVisit: Option[RecordedVisit],
  progress:             Option[StepProgress],
  requests:             ObservationRequests,
  selectedRowId:        Option[SelectedRowId],
  setSelectedRowId:     SelectedRowId => Callback,
  clientMode:           ClientMode,
  getMaskName:          Attachment.Id => Option[NonEmptyString]
) extends ReactFnProps(ObservationSequence)

object ObservationSequence
    extends ReactFnComponent[ObservationSequence](props =>
      for
        ctx         <- useContext(AppContext.ctx)
        sequenceApi <- useContext(SequenceApi.ctx)
      yield
        import ctx.given

        val breakpoints: View[Set[Step.Id]] =
          props.executionState.zoom(ExecutionState.breakpoints)

        // TODO Does it make sense to move this to SequenceBuilder? Then we don't need the hooks here.
        val onBreakpointFlip: (Observation.Id, Step.Id) => Callback =
          (obsId, stepId) =>
            breakpoints
              .modCB(
                _.toggle(stepId),
                newBreakpoints =>
                  sequenceApi
                    .setBreakpoint(
                      obsId,
                      stepId,
                      if newBreakpoints.contains(stepId) then Breakpoint.Enabled
                      else Breakpoint.Disabled
                    )
                    .runAsync
              )

        val mismatchError = Message(
          text = "ERROR: Sequence and S/N are inconsistent.",
          severity = Message.Severity.Error
        )

        props.sequenceData match // TODO Show visits even if sequence data is not available
          case SequenceData(InstrumentExecutionConfig.GmosNorth(config), signalToNoise)  =>
            signalToNoise match
              case ModeSignalToNoise.Spectroscopy(acquisitionItc, scienceItc) =>
                GmosNorthSpectroscopySequenceTable(
                  props.clientMode,
                  props.obsId,
                  config,
                  acquisitionItc,
                  scienceItc,
                  props.visits,
                  props.executionState.get,
                  props.currentRecordedVisit,
                  props.progress,
                  props.selectedRowId,
                  props.setSelectedRowId,
                  props.requests,
                  isPreview = false,
                  onBreakpointFlip,
                  props.getMaskName
                )
              case ModeSignalToNoise.GmosNorthImaging(snByFilter)             =>
                GmosNorthImagingSequenceTable(
                  props.clientMode,
                  props.obsId,
                  config,
                  snByFilter,
                  props.visits,
                  props.executionState.get,
                  props.currentRecordedVisit,
                  props.progress,
                  props.selectedRowId,
                  props.setSelectedRowId,
                  props.requests,
                  isPreview = false,
                  onBreakpointFlip
                )
              // Twilight calibrations have no signal to noise
              case ModeSignalToNoise.Undefined                                =>
                GmosNorthSpectroscopySequenceTable(
                  props.clientMode,
                  props.obsId,
                  config,
                  ItcResultValues.Empty,
                  ItcResultValues.Empty,
                  props.visits,
                  props.executionState.get,
                  props.currentRecordedVisit,
                  props.progress,
                  props.selectedRowId,
                  props.setSelectedRowId,
                  props.requests,
                  isPreview = false,
                  onBreakpointFlip,
                  props.getMaskName
                )
              case _                                                          => mismatchError
          case SequenceData(InstrumentExecutionConfig.GmosSouth(config), signalToNoise)  =>
            signalToNoise match
              case ModeSignalToNoise.Spectroscopy(acquisitionItc, scienceItc) =>
                GmosSouthSpectroscopySequenceTable(
                  props.clientMode,
                  props.obsId,
                  config,
                  acquisitionItc,
                  scienceItc,
                  props.visits,
                  props.executionState.get,
                  props.currentRecordedVisit,
                  props.progress,
                  props.selectedRowId,
                  props.setSelectedRowId,
                  props.requests,
                  isPreview = false,
                  onBreakpointFlip,
                  props.getMaskName
                )
              case ModeSignalToNoise.GmosSouthImaging(snByFilter)             =>
                GmosSouthImagingSequenceTable(
                  props.clientMode,
                  props.obsId,
                  config,
                  snByFilter,
                  props.visits,
                  props.executionState.get,
                  props.currentRecordedVisit,
                  props.progress,
                  props.selectedRowId,
                  props.setSelectedRowId,
                  props.requests,
                  isPreview = false,
                  onBreakpointFlip
                )
              // Twilight calibrations have no signal to noise
              case ModeSignalToNoise.Undefined                                =>
                GmosSouthSpectroscopySequenceTable(
                  props.clientMode,
                  props.obsId,
                  config,
                  ItcResultValues.Empty,
                  ItcResultValues.Empty,
                  props.visits,
                  props.executionState.get,
                  props.currentRecordedVisit,
                  props.progress,
                  props.selectedRowId,
                  props.setSelectedRowId,
                  props.requests,
                  isPreview = false,
                  onBreakpointFlip,
                  props.getMaskName
                )
              case _                                                          => mismatchError
          case SequenceData(InstrumentExecutionConfig.Flamingos2(config), signalToNoise) =>
            signalToNoise match
              case ModeSignalToNoise.Spectroscopy(acquisitionItc, scienceItc) =>
                Flamingos2SequenceTable(
                  props.clientMode,
                  props.obsId,
                  config,
                  acquisitionItc,
                  scienceItc,
                  props.visits,
                  props.executionState.get,
                  props.currentRecordedVisit,
                  props.progress,
                  props.selectedRowId,
                  props.setSelectedRowId,
                  props.requests,
                  isPreview = false,
                  onBreakpointFlip
                )
              case ModeSignalToNoise.Flamingos2Imaging(snByFilter)            =>
                Flamingos2ImagingSequenceTable(
                  props.clientMode,
                  props.obsId,
                  config,
                  snByFilter,
                  props.visits,
                  props.executionState.get,
                  props.currentRecordedVisit,
                  props.progress,
                  props.selectedRowId,
                  props.setSelectedRowId,
                  props.requests,
                  isPreview = false,
                  onBreakpointFlip
                )
              case _                                                          => mismatchError
          case SequenceData(
                InstrumentExecutionConfig.Igrins2(config),
                ModeSignalToNoise.Spectroscopy(acquisitionItc, scienceItc)
              ) =>
            Igrins2SequenceTable(
              props.clientMode,
              props.obsId,
              config,
              acquisitionItc,
              scienceItc,
              props.visits,
              props.executionState.get,
              props.currentRecordedVisit,
              props.progress,
              props.selectedRowId,
              props.setSelectedRowId,
              props.requests,
              isPreview = false,
              onBreakpointFlip
            )
          case SequenceData(InstrumentExecutionConfig.Ghost(config), _)                  =>
            GhostSequenceTable(
              props.clientMode,
              props.obsId,
              config,
              ItcResultValues.Empty,
              ItcResultValues.Empty,
              props.visits,
              props.executionState.get,
              props.currentRecordedVisit,
              props.progress,
              props.selectedRowId,
              props.setSelectedRowId,
              props.requests,
              isPreview = false,
              onBreakpointFlip
            )
          case SequenceData(InstrumentExecutionConfig.Gnirs(config), signalToNoise)      =>
            signalToNoise match
              case ModeSignalToNoise.Spectroscopy(acquisitionItc, scienceItc) =>
                GnirsSequenceTable(
                  props.clientMode,
                  props.obsId,
                  config,
                  acquisitionItc,
                  scienceItc,
                  props.visits,
                  props.executionState.get,
                  props.currentRecordedVisit,
                  props.progress,
                  props.selectedRowId,
                  props.setSelectedRowId,
                  props.requests,
                  isPreview = false,
                  onBreakpointFlip
                )
              case ModeSignalToNoise.GnirsImaging(snByFilter)                 =>
                GnirsImagingSequenceTable(
                  props.clientMode,
                  props.obsId,
                  config,
                  snByFilter,
                  props.visits,
                  props.executionState.get,
                  props.currentRecordedVisit,
                  props.progress,
                  props.selectedRowId,
                  props.setSelectedRowId,
                  props.requests,
                  isPreview = false,
                  onBreakpointFlip
                )
              // Twilight calibrations have no signal to noise
              case ModeSignalToNoise.Undefined                                =>
                GnirsSequenceTable(
                  props.clientMode,
                  props.obsId,
                  config,
                  ItcResultValues.Empty,
                  ItcResultValues.Empty,
                  props.visits,
                  props.executionState.get,
                  props.currentRecordedVisit,
                  props.progress,
                  props.selectedRowId,
                  props.setSelectedRowId,
                  props.requests,
                  isPreview = false,
                  onBreakpointFlip
                )
              case _                                                          => mismatchError
          case _                                                                         => mismatchError
    )

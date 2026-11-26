// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.components

import cats.effect.IO
import crystal.Pot
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.VdomNode
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.SequenceType
import lucuma.core.model.Attachment
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.ExecutionConfig
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.model.sequence.Step
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Message
import lucuma.schemas.model.ExecutionVisits
import lucuma.schemas.model.ItcResultValues
import lucuma.schemas.model.ModeSignalToNoise
import lucuma.ui.primereact.ToastCtx
import lucuma.ui.reusability.given
import lucuma.ui.sequence.IsEditEnabled
import lucuma.ui.sequence.IsEditing
import lucuma.ui.sequence.SelectedRowId
import lucuma.ui.sequence.SequenceColumnProfile
import lucuma.ui.sequence.SequenceData
import lucuma.ui.sequence.SequenceEditApi
import lucuma.ui.sequence.SequenceEditApiImpl
import monocle.Optional
import observe.model.ExecutionState
import observe.model.ObserveStep.*
import observe.model.StepProgress
import observe.model.odb.RecordedVisit
import observe.ui.components.sequence.SeqControlButtons
import observe.ui.components.sequence.byInstrument.*
import observe.ui.model.AppContext
import observe.ui.model.ObservationRequests
import observe.ui.model.enums.ClientMode
import observe.ui.services.SequenceApi
import observe.ui.utils.*

case class ObservationSequence(
  obsId:                Observation.Id,
  sequenceData:         View[SequenceData],
  visits:               View[Option[ExecutionVisits]],
  executionState:       View[ExecutionState],
  refreshing:           Pot[View[Boolean]],
  currentRecordedVisit: Option[RecordedVisit],
  progress:             Option[StepProgress],
  requests:             ObservationRequests,
  selectedRowId:        Option[SelectedRowId],
  setSelectedRowId:     SelectedRowId => Callback,
  clientMode:           ClientMode,
  getMaskName:          Attachment.Id => Option[NonEmptyString],
  columnProfile:        SequenceColumnProfile
) extends ReactFnProps(ObservationSequence)

object ObservationSequence
    extends ReactFnComponent[ObservationSequence](props =>
      for
        ctx                  <- useContext(AppContext.ctx)
        sequenceApi          <- useContext(SequenceApi.ctx)
        // Here, so that both the table and the sequence controls know about the edit sessions.
        isEditingAcquisition <- useStateView(IsEditing.False)
        isEditingScience     <- useStateView(IsEditing.False)
        _                    <- useEffectWithDeps(props.obsId): _ =>
                                  isEditingAcquisition.set(IsEditing.False) >>
                                    isEditingScience.set(IsEditing.False)
      yield
        import ctx.given

        val sequenceData: SequenceData = props.sequenceData.get

        val hasAcquisition: Boolean =
          sequenceData.config match
            case InstrumentExecutionConfig.GmosNorth(config)  => config.acquisition.isDefined
            case InstrumentExecutionConfig.GmosSouth(config)  => config.acquisition.isDefined
            case InstrumentExecutionConfig.Flamingos2(config) => config.acquisition.isDefined
            case InstrumentExecutionConfig.Igrins2(config)    => config.acquisition.isDefined
            case InstrumentExecutionConfig.Ghost(config)      => config.acquisition.isDefined
            case InstrumentExecutionConfig.Gnirs(config)      => config.acquisition.isDefined
            case _                                            => false

        val hasPendingScience: Boolean =
          sequenceData.config match
            case InstrumentExecutionConfig.GmosNorth(config)  => config.science.isDefined
            case InstrumentExecutionConfig.GmosSouth(config)  => config.science.isDefined
            case InstrumentExecutionConfig.Flamingos2(config) => config.science.isDefined
            case InstrumentExecutionConfig.Igrins2(config)    => config.science.isDefined
            case InstrumentExecutionConfig.Ghost(config)      => config.science.isDefined
            case InstrumentExecutionConfig.Gnirs(config)      => config.science.isDefined
            case _                                            => false

        val sequenceControls: VdomNode =
          SeqControlButtons(
            props.obsId,
            props.refreshing,
            props.executionState.get.sequenceStatus,
            props.executionState.get.loadedStep.exists(_.isObserveStarted),
            props.executionState.get.sequenceType,
            hasAcquisition,
            hasPendingScience,
            props.requests,
            isSequenceEditing = isEditingAcquisition.get || isEditingScience.get
          )

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

        val isEditEnabled: IsEditEnabled = IsEditEnabled(props.clientMode.canOperate)

        val editApi: SequenceEditApi[IO] = SequenceEditApiImpl[IO]

        // Observe's breakpoints prevail over the ODB's. Stamped now, as step ids change on replace.
        def withObserveBreakpoints[D](atoms: List[Atom[D]]): List[Atom[D]] =
          val breakpoints: Set[Step.Id] = props.executionState.get.breakpoints
          atoms.map:
            Atom
              .steps[D]
              .modify:
                _.map: step =>
                  Step
                    .breakpoint[D]
                    .replace(
                      if breakpoints.contains(step.id) then Breakpoint.Enabled
                      else Breakpoint.Disabled
                    )(step)

        // After the ODB replaces the sequence, the server reloads that sequence's breakpoints.
        def remoteReplace[D](
          replace: (Observation.Id, SequenceType, List[Atom[D]]) => IO[List[Atom[D]]]
        ): SequenceType => List[Atom[D]] => IO[List[Atom[D]]] =
          sequenceType =>
            atoms =>
              replace(props.obsId, sequenceType, withObserveBreakpoints(atoms)) <*
                sequenceApi
                  .reloadAfterEdit(props.obsId, sequenceType)
                  .handleErrorWith: e =>
                    ToastCtx[IO].showToast(
                      s"The sequence was saved, but Observe could not reload its breakpoints: ${e.getMessage}",
                      Message.Severity.Warning,
                      sticky = true
                    )

        def configView[S, D](
          optional: Optional[InstrumentExecutionConfig, ExecutionConfig[S, D]]
        ): Option[View[ExecutionConfig[S, D]]] =
          props.sequenceData.zoom(SequenceData.config.andThen(optional)).toOptionView

        val mismatchError = Message(
          text = "ERROR: Sequence and S/N are inconsistent.",
          severity = Message.Severity.Error
        )

        sequenceData match // TODO Show visits even if sequence data is not available
          case SequenceData(InstrumentExecutionConfig.GmosNorth(_), signalToNoise)  =>
            configView(
              InstrumentExecutionConfig.gmosNorth.andThen(
                InstrumentExecutionConfig.GmosNorth.executionConfig
              )
            ).fold[VdomNode](mismatchError): config =>
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
                    columnProfile = props.columnProfile,
                    onBreakpointFlip,
                    props.getMaskName,
                    sequenceControls,
                    isEditEnabled,
                    isEditingAcquisition,
                    isEditingScience,
                    remoteReplace(editApi.replaceGmosNorthSequence)
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
                    columnProfile = props.columnProfile,
                    onBreakpointFlip,
                    sequenceControls,
                    isEditEnabled,
                    isEditingAcquisition,
                    isEditingScience,
                    remoteReplace(editApi.replaceGmosNorthSequence)
                  )
                // Twilight calibrations have no signal to noise
                case ModeSignalToNoise.Undefined                                =>
                  GmosNorthSpectroscopySequenceTable(
                    props.clientMode,
                    props.obsId,
                    config,
                    None,
                    ItcResultValues.Empty,
                    props.visits,
                    props.executionState.get,
                    props.currentRecordedVisit,
                    props.progress,
                    props.selectedRowId,
                    props.setSelectedRowId,
                    props.requests,
                    isPreview = false,
                    columnProfile = props.columnProfile,
                    onBreakpointFlip,
                    props.getMaskName,
                    sequenceControls,
                    isEditEnabled,
                    isEditingAcquisition,
                    isEditingScience,
                    remoteReplace(editApi.replaceGmosNorthSequence)
                  )
                case _                                                          => mismatchError
          case SequenceData(InstrumentExecutionConfig.GmosSouth(_), signalToNoise)  =>
            configView(
              InstrumentExecutionConfig.gmosSouth.andThen(
                InstrumentExecutionConfig.GmosSouth.executionConfig
              )
            ).fold[VdomNode](mismatchError): config =>
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
                    columnProfile = props.columnProfile,
                    onBreakpointFlip,
                    props.getMaskName,
                    sequenceControls,
                    isEditEnabled,
                    isEditingAcquisition,
                    isEditingScience,
                    remoteReplace(editApi.replaceGmosSouthSequence)
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
                    columnProfile = props.columnProfile,
                    onBreakpointFlip,
                    sequenceControls,
                    isEditEnabled,
                    isEditingAcquisition,
                    isEditingScience,
                    remoteReplace(editApi.replaceGmosSouthSequence)
                  )
                // Twilight calibrations have no signal to noise
                case ModeSignalToNoise.Undefined                                =>
                  GmosSouthSpectroscopySequenceTable(
                    props.clientMode,
                    props.obsId,
                    config,
                    None,
                    ItcResultValues.Empty,
                    props.visits,
                    props.executionState.get,
                    props.currentRecordedVisit,
                    props.progress,
                    props.selectedRowId,
                    props.setSelectedRowId,
                    props.requests,
                    isPreview = false,
                    columnProfile = props.columnProfile,
                    onBreakpointFlip,
                    props.getMaskName,
                    sequenceControls,
                    isEditEnabled,
                    isEditingAcquisition,
                    isEditingScience,
                    remoteReplace(editApi.replaceGmosSouthSequence)
                  )
                case _                                                          => mismatchError
          case SequenceData(InstrumentExecutionConfig.Flamingos2(_), signalToNoise) =>
            configView(
              InstrumentExecutionConfig.flamingos2.andThen(
                InstrumentExecutionConfig.Flamingos2.executionConfig
              )
            ).fold[VdomNode](mismatchError): config =>
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
                    columnProfile = props.columnProfile,
                    onBreakpointFlip,
                    props.getMaskName,
                    sequenceControls,
                    isEditEnabled,
                    isEditingAcquisition,
                    isEditingScience,
                    remoteReplace(editApi.replaceFlamingos2Sequence)
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
                    columnProfile = props.columnProfile,
                    onBreakpointFlip,
                    sequenceControls,
                    isEditEnabled,
                    isEditingAcquisition,
                    isEditingScience,
                    remoteReplace(editApi.replaceFlamingos2Sequence)
                  )
                // Daytime calibrations have no signal to noise
                case ModeSignalToNoise.Undefined                                =>
                  Flamingos2SequenceTable(
                    props.clientMode,
                    props.obsId,
                    config,
                    None,
                    ItcResultValues.Empty,
                    props.visits,
                    props.executionState.get,
                    props.currentRecordedVisit,
                    props.progress,
                    props.selectedRowId,
                    props.setSelectedRowId,
                    props.requests,
                    isPreview = false,
                    columnProfile = props.columnProfile,
                    onBreakpointFlip,
                    props.getMaskName,
                    sequenceControls,
                    isEditEnabled,
                    isEditingAcquisition,
                    isEditingScience,
                    remoteReplace(editApi.replaceFlamingos2Sequence)
                  )
                case _                                                          => mismatchError
          case SequenceData(InstrumentExecutionConfig.Igrins2(_), signalToNoise)    =>
            configView(
              InstrumentExecutionConfig.igrins2.andThen(
                InstrumentExecutionConfig.Igrins2.executionConfig
              )
            ).fold[VdomNode](mismatchError): config =>
              def table(
                acquisitionItc: Option[ItcResultValues],
                scienceItc:     ItcResultValues
              ): VdomNode =
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
                  columnProfile = props.columnProfile,
                  onBreakpointFlip,
                  sequenceControls,
                  isEditEnabled,
                  isEditingAcquisition,
                  isEditingScience,
                  remoteReplace(editApi.replaceIgrins2Sequence)
                )
              signalToNoise match
                case ModeSignalToNoise.Spectroscopy(acquisitionItc, scienceItc) =>
                  table(acquisitionItc, scienceItc)
                // Daytime calibrations have no signal to noise
                case ModeSignalToNoise.Undefined                                =>
                  table(None, ItcResultValues.Empty)
                case _                                                          => mismatchError
          case SequenceData(InstrumentExecutionConfig.Ghost(_), _)                  =>
            configView(
              InstrumentExecutionConfig.ghost.andThen(
                InstrumentExecutionConfig.Ghost.executionConfig
              )
            ).fold[VdomNode](mismatchError): config =>
              GhostSequenceTable(
                props.clientMode,
                props.obsId,
                config,
                None,
                ItcResultValues.Empty,
                props.visits,
                props.executionState.get,
                props.currentRecordedVisit,
                props.progress,
                props.selectedRowId,
                props.setSelectedRowId,
                props.requests,
                isPreview = false,
                columnProfile = props.columnProfile,
                onBreakpointFlip,
                sequenceControls,
                isEditEnabled,
                isEditingAcquisition,
                isEditingScience,
                remoteReplace(editApi.replaceGhostSequence)
              )
          case SequenceData(InstrumentExecutionConfig.Gnirs(_), signalToNoise)      =>
            configView(
              InstrumentExecutionConfig.gnirs.andThen(
                InstrumentExecutionConfig.Gnirs.executionConfig
              )
            ).fold[VdomNode](mismatchError): config =>
              signalToNoise match
                case ModeSignalToNoise.GnirsSpectroscopy(acquisitionItc, scienceItc) =>
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
                    columnProfile = props.columnProfile,
                    onBreakpointFlip,
                    sequenceControls,
                    isEditEnabled,
                    isEditingAcquisition,
                    isEditingScience,
                    remoteReplace(editApi.replaceGnirsSequence)
                  )
                case ModeSignalToNoise.GnirsImaging(snByFilter)                      =>
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
                    columnProfile = props.columnProfile,
                    onBreakpointFlip,
                    sequenceControls,
                    isEditEnabled,
                    isEditingAcquisition,
                    isEditingScience,
                    remoteReplace(editApi.replaceGnirsSequence)
                  )
                // Twilight calibrations have no signal to noise
                case ModeSignalToNoise.Undefined                                     =>
                  GnirsSequenceTable(
                    props.clientMode,
                    props.obsId,
                    config,
                    ItcResultValues.Empty,
                    Nil,
                    props.visits,
                    props.executionState.get,
                    props.currentRecordedVisit,
                    props.progress,
                    props.selectedRowId,
                    props.setSelectedRowId,
                    props.requests,
                    isPreview = false,
                    columnProfile = props.columnProfile,
                    onBreakpointFlip,
                    sequenceControls,
                    isEditEnabled,
                    isEditingAcquisition,
                    isEditingScience,
                    remoteReplace(editApi.replaceGnirsSequence)
                  )
                case _                                                               => mismatchError
          case _                                                                    => mismatchError
    )

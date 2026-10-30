// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import cats.effect.IO
import cats.syntax.all.*
import crystal.Pot
import crystal.react.*
import crystal.react.hooks.*
import explore.*
import explore.components.*
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.config.sequence.byInstrument.*
import explore.model.AppContext
import explore.model.AttachmentList
import explore.model.DismissedWarnings
import explore.model.Execution
import explore.model.IsActive
import explore.model.ObsTabTileIds
import explore.model.Observation
import explore.model.enums.ObsValidationSeverity
import explore.model.enums.WavelengthUnits
import explore.model.syntax.all.*
import explore.render.*
import explore.syntax.ui.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.TagOf
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.SequenceType
import lucuma.core.model.Target
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.ExecutionConfig
import lucuma.core.model.sequence.ExecutionSequence
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.react.primereact.Button
import lucuma.react.primereact.Dialog
import lucuma.react.primereact.DialogPosition
import lucuma.react.primereact.Message
import lucuma.react.primereact.TooltipOptions
import lucuma.react.primereact.tooltip.*
import lucuma.react.table.ColumnId
import lucuma.refined.*
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.ItcResultValues
import lucuma.schemas.model.ModeSignalToNoise
import lucuma.ui.primereact.*
import lucuma.ui.sequence.*
import lucuma.ui.sequence.IsEditing
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import org.scalajs.dom.HTMLElement
import org.typelevel.log4cats.Logger

import scala.collection.immutable.SortedSet

final case class SequenceTile(
  obsId:                   Observation.Id,
  obsExecution:            Execution,
  asterismIds:             SortedSet[Target.Id],
  customSedTimestamps:     List[Timestamp],
  calibrationRole:         Option[CalibrationRole],
  basicConfiguration:      Option[BasicConfiguration],
  hasMaterializedSequence: Boolean,
  sequenceChanged:         View[Pot[Unit]],
  isEditingAcquisition:    View[IsEditing],
  isEditingScience:        View[IsEditing],
  isUserStaffOrAdmin:      Boolean,
  wavelengthUnits:         WavelengthUnits,
  dismissedWarnings:       DismissedWarnings,
  obsIsExecuted:           Boolean,
  readonly:                Boolean,
  attachments:             AttachmentList
) extends Tile[SequenceTile](
      ObsTabTileIds.SequenceId.id,
      <.span(
        "Sequence",
        obsExecution.stepValidations
          .map(v => ObsValidationSeverity.of(v.code, dismissedWarnings))
          .maximumOption
          .map: severity =>
            <.span(ExploreStyles.SequenceTileStepViolations)(validationSeverityIcon(severity))
              .withTooltip(content =
                validationsTooltip(obsExecution.stepValidations, dismissedWarnings)
              )
      ),
      canMinimize = !isEditingAcquisition.get && !isEditingScience.get
    )(
      SequenceTile // TODO Move isEditing state here, but we need to be able to change tile state from within tile
    )

object SequenceTile
    extends TileComponent[SequenceTile]((props, sizeState) =>
      import SequenceTileHelper.*

      for
        ctx               <- useContext(AppContext.ctx)
        liveSequence      <- useLiveSequence(
                               props.obsId,
                               props.asterismIds.toList,
                               props.customSedTimestamps,
                               props.calibrationRole
                             )
        _                 <-
          useEffectWithDeps(liveSequence): newLiveSequence =>
            props.sequenceChanged.set: // Notify caller of change
              (newLiveSequence.visits, newLiveSequence.sequence.map(_.get)).tupled.void
        isRegenerating    <- useStateView(IsActive(false))
        confirmRegenerate <- useStateView(false)
      yield
        given Logger[IO] = ctx.logger

        val execution: Execution           = props.obsExecution
        val staleCss: TagMod               = execution.digest.staleClass
        val staleTooltip: Option[VdomNode] = execution.digest.staleTooltip
        val programTimeCharge: TimeSpan    = execution.programTimeCharge.value
        val executed: TagOf[HTMLElement]   = timeDisplay("Executed", programTimeCharge)
        val hiddenColumns: Set[ColumnId]   =
          SequenceColumns.hiddenColumns(
            SequenceColumnProfile.fromObservation(props.calibrationRole, props.basicConfiguration)
          )

        // The ODB only lets staff replace the sequence once execution has started.
        val isEditEnabled: IsEditEnabled =
          IsEditEnabled(
            sizeState.isMaximized && liveSequence.isReady && !props.readonly &&
              (props.isUserStaffOrAdmin || !props.obsIsExecuted)
          )

        val customizedSequences: List[SequenceType] = execution.customizedSequences

        val customizedBadge: Option[VdomNode] =
          Option.when(customizedSequences.nonEmpty):
            <.span(ExploreStyles.SequenceTileCustomized)(
              Icons.ExclamationDiamond.withClass(ExploreStyles.WarningIcon),
              "Customized",
              testId := "explore-sequence-customized"
            ).withTooltip(content = SequenceTile.customizedTooltip(customizedSequences))

        val canRegenerateAcquisition: Boolean =
          execution.acquisitionSequenceIsCustomized && !props.readonly &&
            !props.isEditingAcquisition.get && !props.isEditingScience.get &&
            (props.isUserStaffOrAdmin || !props.obsIsExecuted)

        val regenerateAcquisition: IO[Unit] =
          ctx.odbApi
            .resetAcquisition(props.obsId, regenerate = true)
            .handleErrorWith: e =>
              ctx.toastCtx.showToast(
                s"Failed to regenerate the acquisition: ${e.getMessage}",
                Message.Severity.Error,
                sticky = true
              )
            .switching(isRegenerating.async, IsActive(_)) >> confirmRegenerate.async.set(false)

        val regenerateAcquisitionButton: Option[VdomNode] =
          Option.when(canRegenerateAcquisition):
            React.Fragment(
              Button(
                onClick = confirmRegenerate.set(true),
                label = "Regenerate acquisition",
                icon = Icons.ArrowsRotate,
                tooltip = "Discard the acquisition sequence customizations and regenerate it",
                tooltipOptions = TooltipOptions.Top
              ).mini.compact,
              Dialog(
                visible = confirmRegenerate.get,
                onHide = confirmRegenerate.set(false),
                header = "Regenerate acquisition",
                footer = <.div(
                  Button(
                    label = "Cancel",
                    icon = Icons.Close,
                    severity = Button.Severity.Danger,
                    onClick = confirmRegenerate.set(false)
                  ).small.compact,
                  Button(
                    label = "Proceed",
                    icon = Icons.Checkmark,
                    onClick = regenerateAcquisition.runAsync,
                    loading = isRegenerating.get.value
                  ).small.compact
                ),
                position = DialogPosition.Top,
                modal = true,
                resizable = false
              )(
                <.div("Acquisition sequence customizations will be lost. Proceed?")
              )
            )

        // A materialized sequence isn't regenerated when the configuration changes, and with no
        // sequence yet the body already shows a spinner, so this only marks an in-place update.
        val isUpdating: Boolean =
          liveSequence.isRefreshing &&
            !props.hasMaterializedSequence &&
            liveSequence.sequence.toOption.exists(_.get.isDefined)

        val updatingIndicator: Option[VdomNode] =
          Option.when(isUpdating):
            <.span(Icons.Spinner.withSpin(true))
              .withTooltip(content = "Updating sequence...")

        val summary: VdomNode =
          execution.digest.programTimeEstimate.value
            .map: plannedTime =>
              val total: TimeSpan             = programTimeCharge +| plannedTime
              val pending: TagOf[HTMLElement] =
                timeDisplay(
                  "Pending",
                  plannedTime,
                  timeClass = staleCss,
                  timeTooltip = staleTooltip
                )
              val planned: TagOf[HTMLElement] =
                timeDisplay("Planned", total, timeClass = staleCss, timeTooltip = staleTooltip)(
                  testId := "explore-sequence-planned"
                )

              <.span(ExploreStyles.SequenceTileTitleSummary)(
                HelpIcon("target/main/sequence-times.md".refined),
                planned,
                executed,
                pending,
                updatingIndicator
              )
            .getOrElse(executed)

        val title =
          <.span(ExploreStyles.SequenceTileTitle)(
            summary,
            <.span(ExploreStyles.SequenceTileCustomization)(
              customizedBadge,
              regenerateAcquisitionButton
            ).when(customizedSequences.nonEmpty)
          )

        val mismatchError = Message(
          text = "ERROR: Sequence and S/N are inconsistent.",
          severity = Message.Severity.Error
        )

        // The sequence takes a few seconds to be regenerated after an edit, mark that state.
        val waitingForSequence =
          Message(
            text = "Waiting for the sequence to be generated...",
            severity = Message.Severity.Info,
            icon = Icons.Spinner.withSpin(true)
          )

        extension [D](execution: View[Option[ExecutionSequence[D]]])
          def flatExecutionSequence: View[List[Atom[D]]] =
            execution.zoom(_.foldMap(s => s.nextAtom +: s.possibleFuture))(modList =>
              optExSeq =>
                val list: List[Atom[D]]    = optExSeq.foldMap(s => s.nextAtom +: s.possibleFuture)
                val newList: List[Atom[D]] = modList(list)
                if (newList.isEmpty) none
                else ExecutionSequence(newList.head, newList.tail, optExSeq.exists(_.hasMore)).some
            )

        extension [S, D](execution: View[ExecutionConfig[S, D]])
          def flatAcquisition: View[List[Atom[D]]] =
            execution.zoom(ExecutionConfig.acquisition).flatExecutionSequence
          def flatScience: View[List[Atom[D]]]     =
            execution.zoom(ExecutionConfig.science).flatExecutionSequence

        val body =
          props.sequenceChanged.get
            .flatMap: _ =>
              (liveSequence.visits, liveSequence.sequence).tupled
            .renderPot(
              (visitsViewOpt, sequenceViewOpt) =>
                // TODO Show visits even if sequence data is not available
                sequenceViewOpt.toOptionView
                  .flatMap { sequnceView =>
                    sequnceView.get match
                      case SequenceData(
                            InstrumentExecutionConfig.GmosNorth(config),
                            signalToNoise
                          ) =>
                        sequnceView
                          .zoom(
                            SequenceData.config
                              .andThen(InstrumentExecutionConfig.gmosNorth)
                              .andThen(InstrumentExecutionConfig.GmosNorth.executionConfig)
                          )
                          .toOptionView
                          .map: gmosNorthExecutionView =>
                            signalToNoise match
                              case ModeSignalToNoise.Spectroscopy(acquisitionSn, scienceSn) =>
                                GmosNorthSpectroscopySequenceTable(
                                  visitsViewOpt,
                                  config.static,
                                  gmosNorthExecutionView.flatAcquisition,
                                  gmosNorthExecutionView.flatScience,
                                  acquisitionSn,
                                  scienceSn,
                                  isEditEnabled,
                                  props.isEditingAcquisition,
                                  props.isEditingScience,
                                  props.isUserStaffOrAdmin,
                                  props.wavelengthUnits,
                                  seqType =>
                                    atoms =>
                                      ctx.odbApi
                                        .replaceGmosNorthSequence(props.obsId, seqType, atoms),
                                  props.attachments,
                                  hiddenColumns
                                )
                              case ModeSignalToNoise.GmosNorthImaging(itcPerFilter)         =>
                                GmosNorthImagingSequenceTable(
                                  visitsViewOpt,
                                  config.static,
                                  gmosNorthExecutionView.flatAcquisition,
                                  gmosNorthExecutionView.flatScience,
                                  itcPerFilter,
                                  isEditEnabled,
                                  props.isEditingAcquisition,
                                  props.isEditingScience,
                                  props.isUserStaffOrAdmin,
                                  props.wavelengthUnits,
                                  seqType =>
                                    atoms =>
                                      ctx.odbApi
                                        .replaceGmosNorthSequence(props.obsId, seqType, atoms),
                                  hiddenColumns
                                )
                              // Twilights have no signal to noise
                              case ModeSignalToNoise.Undefined                              =>
                                GmosNorthSpectroscopySequenceTable(
                                  visitsViewOpt,
                                  config.static,
                                  gmosNorthExecutionView.flatAcquisition,
                                  gmosNorthExecutionView.flatScience,
                                  ItcResultValues.Empty,
                                  ItcResultValues.Empty,
                                  isEditEnabled,
                                  props.isEditingAcquisition,
                                  props.isEditingScience,
                                  props.isUserStaffOrAdmin,
                                  props.wavelengthUnits,
                                  seqType =>
                                    atoms =>
                                      ctx.odbApi
                                        .replaceGmosNorthSequence(props.obsId, seqType, atoms),
                                  props.attachments,
                                  hiddenColumns
                                )
                              case _                                                        => mismatchError
                      case SequenceData(
                            InstrumentExecutionConfig.GmosSouth(config),
                            signalToNoise
                          ) =>
                        sequnceView
                          .zoom(
                            SequenceData.config
                              .andThen(InstrumentExecutionConfig.gmosSouth)
                              .andThen(InstrumentExecutionConfig.GmosSouth.executionConfig)
                          )
                          .toOptionView
                          .map: gmosSouthExecutionView =>
                            signalToNoise match
                              case ModeSignalToNoise.Spectroscopy(acquisitionSn, scienceSn) =>
                                GmosSouthSpectroscopySequenceTable(
                                  visitsViewOpt,
                                  config.static,
                                  gmosSouthExecutionView.flatAcquisition,
                                  gmosSouthExecutionView.flatScience,
                                  acquisitionSn,
                                  scienceSn,
                                  isEditEnabled,
                                  props.isEditingAcquisition,
                                  props.isEditingScience,
                                  props.isUserStaffOrAdmin,
                                  props.wavelengthUnits,
                                  seqType =>
                                    atoms =>
                                      ctx.odbApi
                                        .replaceGmosSouthSequence(props.obsId, seqType, atoms),
                                  props.attachments,
                                  hiddenColumns
                                )
                              case ModeSignalToNoise.GmosSouthImaging(itcPerFilter)         =>
                                GmosSouthImagingSequenceTable(
                                  visitsViewOpt,
                                  config.static,
                                  gmosSouthExecutionView.flatAcquisition,
                                  gmosSouthExecutionView.flatScience,
                                  itcPerFilter,
                                  isEditEnabled,
                                  props.isEditingAcquisition,
                                  props.isEditingScience,
                                  props.isUserStaffOrAdmin,
                                  props.wavelengthUnits,
                                  seqType =>
                                    atoms =>
                                      ctx.odbApi
                                        .replaceGmosSouthSequence(props.obsId, seqType, atoms),
                                  hiddenColumns
                                )
                              // Twilights have no signal to noise
                              case ModeSignalToNoise.Undefined                              =>
                                GmosSouthSpectroscopySequenceTable(
                                  visitsViewOpt,
                                  config.static,
                                  gmosSouthExecutionView.flatAcquisition,
                                  gmosSouthExecutionView.flatScience,
                                  ItcResultValues.Empty,
                                  ItcResultValues.Empty,
                                  isEditEnabled,
                                  props.isEditingAcquisition,
                                  props.isEditingScience,
                                  props.isUserStaffOrAdmin,
                                  props.wavelengthUnits,
                                  seqType =>
                                    atoms =>
                                      ctx.odbApi
                                        .replaceGmosSouthSequence(props.obsId, seqType, atoms),
                                  props.attachments,
                                  hiddenColumns
                                )
                              case _                                                        => mismatchError
                      case SequenceData(
                            InstrumentExecutionConfig.Flamingos2(config),
                            signalToNoise
                          ) =>
                        sequnceView
                          .zoom(
                            SequenceData.config
                              .andThen(InstrumentExecutionConfig.flamingos2)
                              .andThen(InstrumentExecutionConfig.Flamingos2.executionConfig)
                          )
                          .toOptionView
                          .map: flamingos2ExecutionView =>
                            signalToNoise match
                              case ModeSignalToNoise.Spectroscopy(acquisitionSn, scienceSn) =>
                                Flamingos2SequenceTable(
                                  visitsViewOpt,
                                  config.static,
                                  flamingos2ExecutionView.flatAcquisition,
                                  flamingos2ExecutionView.flatScience,
                                  acquisitionSn,
                                  scienceSn,
                                  isEditEnabled,
                                  props.isEditingAcquisition,
                                  props.isEditingScience,
                                  props.isUserStaffOrAdmin,
                                  props.wavelengthUnits,
                                  seqType =>
                                    atoms =>
                                      ctx.odbApi
                                        .replaceFlamingos2Sequence(props.obsId, seqType, atoms),
                                  props.attachments,
                                  hiddenColumns
                                )
                              case ModeSignalToNoise.Flamingos2Imaging(itcPerFilter)        =>
                                Flamingos2ImagingSequenceTable(
                                  visitsViewOpt,
                                  config.static,
                                  flamingos2ExecutionView.flatAcquisition,
                                  flamingos2ExecutionView.flatScience,
                                  itcPerFilter,
                                  isEditEnabled,
                                  props.isEditingAcquisition,
                                  props.isEditingScience,
                                  props.isUserStaffOrAdmin,
                                  props.wavelengthUnits,
                                  seqType =>
                                    atoms =>
                                      ctx.odbApi
                                        .replaceFlamingos2Sequence(props.obsId, seqType, atoms),
                                  hiddenColumns
                                )
                              case ModeSignalToNoise.Undefined                              =>
                                Flamingos2SequenceTable(
                                  visitsViewOpt,
                                  config.static,
                                  flamingos2ExecutionView.flatAcquisition,
                                  flamingos2ExecutionView.flatScience,
                                  ItcResultValues.Empty,
                                  ItcResultValues.Empty,
                                  isEditEnabled,
                                  props.isEditingAcquisition,
                                  props.isEditingScience,
                                  props.isUserStaffOrAdmin,
                                  props.wavelengthUnits,
                                  seqType =>
                                    atoms =>
                                      ctx.odbApi
                                        .replaceFlamingos2Sequence(props.obsId, seqType, atoms),
                                  props.attachments,
                                  hiddenColumns
                                )
                              case _                                                        => mismatchError
                      case SequenceData(
                            InstrumentExecutionConfig.Igrins2(config),
                            ModeSignalToNoise.Spectroscopy(acquisitionSn, scienceSn)
                          ) =>
                        sequnceView
                          .zoom(
                            SequenceData.config
                              .andThen(InstrumentExecutionConfig.igrins2)
                              .andThen(InstrumentExecutionConfig.Igrins2.executionConfig)
                          )
                          .toOptionView
                          .map: igrins2ExecutionView =>
                            Igrins2SequenceTable(
                              visitsViewOpt,
                              config.static,
                              igrins2ExecutionView.flatAcquisition,
                              igrins2ExecutionView.flatScience,
                              acquisitionSn,
                              scienceSn,
                              isEditEnabled,
                              props.isEditingAcquisition,
                              props.isEditingScience,
                              props.isUserStaffOrAdmin,
                              props.wavelengthUnits,
                              seqType => ctx.odbApi.replaceIgrins2Sequence(props.obsId, seqType, _),
                              hiddenColumns
                            )
                      case SequenceData(InstrumentExecutionConfig.Ghost(config), _)             =>
                        sequnceView
                          .zoom(
                            SequenceData.config
                              .andThen(InstrumentExecutionConfig.ghost)
                              .andThen(InstrumentExecutionConfig.Ghost.executionConfig)
                          )
                          .toOptionView
                          .map: ghostExecutionView =>
                            GhostSequenceTable(
                              visitsViewOpt,
                              config.static,
                              ghostExecutionView.flatScience,
                              isEditEnabled,
                              props.isEditingAcquisition,
                              props.isEditingScience,
                              props.isUserStaffOrAdmin,
                              props.wavelengthUnits,
                              seqType => ctx.odbApi.replaceGhostSequence(props.obsId, seqType, _),
                              hiddenColumns
                            )
                      case SequenceData(InstrumentExecutionConfig.Gnirs(config), signalToNoise) =>
                        sequnceView
                          .zoom(
                            SequenceData.config
                              .andThen(InstrumentExecutionConfig.gnirs)
                              .andThen(InstrumentExecutionConfig.Gnirs.executionConfig)
                          )
                          .toOptionView
                          .map: gnirsExecutionView =>
                            signalToNoise match
                              case ModeSignalToNoise.GnirsImaging(itcPerFilter)                  =>
                                GnirsImagingSequenceTable(
                                  visitsViewOpt,
                                  config.static,
                                  gnirsExecutionView.flatAcquisition,
                                  gnirsExecutionView.flatScience,
                                  itcPerFilter,
                                  isEditEnabled,
                                  props.isEditingAcquisition,
                                  props.isEditingScience,
                                  props.isUserStaffOrAdmin,
                                  props.wavelengthUnits,
                                  seqType =>
                                    ctx.odbApi.replaceGnirsSequence(props.obsId, seqType, _),
                                  hiddenColumns
                                )
                              case ModeSignalToNoise.GnirsSpectroscopy(acquisitionSn, scienceSn) =>
                                GnirsSequenceTable(
                                  visitsViewOpt,
                                  config.static,
                                  gnirsExecutionView.flatAcquisition,
                                  gnirsExecutionView.flatScience,
                                  acquisitionSn,
                                  scienceSn,
                                  isEditEnabled,
                                  props.isEditingAcquisition,
                                  props.isEditingScience,
                                  props.isUserStaffOrAdmin,
                                  props.wavelengthUnits,
                                  seqType =>
                                    ctx.odbApi.replaceGnirsSequence(props.obsId, seqType, _),
                                  hiddenColumns
                                )
                              // Daytime Pinholes have no signal to noise
                              case ModeSignalToNoise.Undefined                                   =>
                                GnirsSequenceTable(
                                  visitsViewOpt,
                                  config.static,
                                  gnirsExecutionView.flatAcquisition,
                                  gnirsExecutionView.flatScience,
                                  ItcResultValues.Empty,
                                  Nil,
                                  isEditEnabled,
                                  props.isEditingAcquisition,
                                  props.isEditingScience,
                                  props.isUserStaffOrAdmin,
                                  props.wavelengthUnits,
                                  seqType =>
                                    ctx.odbApi.replaceGnirsSequence(props.obsId, seqType, _),
                                  hiddenColumns
                                )
                              case _                                                             => mismatchError
                      case _                                                                    => mismatchError.some
                  }
                  .getOrElse:
                    Message(
                      text = "Empty or incomplete sequence data returned by server",
                      severity = Message.Severity.Error
                    )
              ,
              errorRender = m =>
                <.div(ExploreStyles.SequencesPanelError)(
                  if liveSequence.isRefreshing then waitingForSequence
                  else
                    Message(
                      text = m.getMessage,
                      severity = Message.Severity.Warning,
                      icon = Icons.ExclamationTriangle
                    )
                )
            )

        TileContents(title, body)
    ):

  private def customizedTooltip(sequences: List[SequenceType]): String =
    sequences match
      case List(sequenceType) => s"The ${sequenceType.tag} sequence has been customized"
      case _                  => "The acquisition and science sequences have been customized"

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import cats.Endo
import cats.effect.IO
import cats.syntax.all.*
import crystal.Pot
import crystal.react.View
import crystal.react.hooks.*
import crystal.react.syntax.effect.*
import explore.*
import explore.components.*
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.config.sequence.byInstrument.*
import explore.givens.given
import explore.model.AppContext
import explore.model.Execution
import explore.model.ObsTabTileIds
import explore.model.Observation
import explore.model.syntax.all.*
import explore.syntax.ui.*
import explore.undo.UndoContext
import explore.undo.UndoStacks
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.TagOf
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.model.Target
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.ExecutionConfig
import lucuma.core.model.sequence.ExecutionSequence
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.react.primereact.Button
import lucuma.react.primereact.Message
import lucuma.react.primereact.TooltipOptions
import lucuma.refined.*
import lucuma.schemas.model.ModeSignalToNoise
import lucuma.ui.primereact.*
import lucuma.ui.sequence.IsEditing
import lucuma.ui.sequence.SequenceData
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import monocle.Iso
import monocle.Optional
import org.scalajs.dom.HTMLElement

import scala.collection.immutable.SortedSet

final case class SequenceTile(
  obsId:               Observation.Id,
  obsExecution:        Execution,
  asterismIds:         SortedSet[Target.Id],
  customSedTimestamps: List[Timestamp],
  calibrationRole:     Option[CalibrationRole],
  sequenceChanged:     View[Pot[Unit]],
  isEditing:           View[IsEditing],
  isUserStaffOrAdmin:  Boolean,
  isEditable:          Boolean
) extends Tile[SequenceTile](ObsTabTileIds.SequenceId.id, "Sequence", canMinimize = !isEditing.get)(
      SequenceTile // TODO Move isEditing state here, but we need to be able to change tile state from within tile
    )

object SequenceTile
    extends TileComponent[SequenceTile]((props, sizeState) =>
      import SequenceTileHelper.*

      for
        ctx                      <- useContext(AppContext.ctx)
        liveSequence             <- useLiveSequence(
                                      props.obsId,
                                      props.obsExecution,
                                      props.asterismIds.toList,
                                      props.customSedTimestamps,
                                      props.calibrationRole
                                    )
        editableSequence         <- useStateView(EditableSequence.fromLiveSequence(liveSequence))
        resetEditableSequenceFrom = // LiveSequence => Callback
          (newLiveSequence: LiveSequence) =>
            editableSequence.set(EditableSequence.fromLiveSequence(newLiveSequence))
        _                        <-
          useEffectWithDeps(liveSequence): newLiveSequence =>
            props.sequenceChanged.set: // Notify caller of change
              (newLiveSequence.visits.value, newLiveSequence.sequence.value.map(_.get)).tupled.void
            >> ( // Invalidate edit sequence if we were editing.
              ctx.toastCtx
                .showToast(
                  "The sequence was modified remotely. The edit session has been canceled.",
                  Message.Severity.Warning,
                  sticky = true
                )
                .runAsyncAndForget >> props.isEditing.set(IsEditing.False)
            ).when_(props.isEditing.get) >>
              // Keep editable sequence in sync with live sequence
              resetEditableSequenceFrom(newLiveSequence)
        undoStacks               <- useStateView(UndoStacks.empty[IO, Option[EditableSequence]])
        _                        <- useEffectWithDeps(props.isEditing.get.value): _ =>
                                      undoStacks.set(UndoStacks.empty[IO, Option[EditableSequence]])
      yield
        import ctx.given

        val execution: Execution           = props.obsExecution
        val staleCss: TagMod               = execution.digest.staleClass
        val staleTooltip: Option[VdomNode] = execution.digest.staleTooltip
        val programTimeCharge: TimeSpan    = execution.programTimeCharge.value
        val executed: TagOf[HTMLElement]   = timeDisplay("Executed", programTimeCharge)

        val undoCtx: UndoContext[Option[EditableSequence]] =
          UndoContext(undoStacks, editableSequence)

        def replaceAcquisition[S, D](
          editableOptional:        Optional[EditableSequence, Option[Atom[D]]],
          executionConfigOptional: Optional[InstrumentExecutionConfig, ExecutionConfig[S, D]]
        ): Endo[InstrumentExecutionConfig] =
          editableSequence.get
            .flatMap(editableOptional.getOption)
            .foldMap:
              case Some(newAcq) => // TODO Should we also erase possibleFuture?
                executionConfigOptional
                  .andThen(ExecutionConfig.acquisition.some)
                  .andThen(ExecutionSequence.nextAtom)
                  .replace(newAcq)
              case None         =>
                executionConfigOptional
                  .andThen(ExecutionConfig.acquisition)
                  .replace(none)

        def replaceScience[S, D](
          editableOptional:        Optional[EditableSequence, List[Atom[D]]],
          executionConfigOptional: Optional[InstrumentExecutionConfig, ExecutionConfig[S, D]]
        ): Endo[InstrumentExecutionConfig] =
          editableSequence.get
            .flatMap(editableOptional.getOption)
            .foldMap:
              case head :: tail =>
                executionConfigOptional
                  .andThen(ExecutionConfig.science.some)
                  .andThen(ExecutionSequence.nextAtom)
                  .replace(head) >>>
                  executionConfigOptional
                    .andThen(ExecutionConfig.science.some)
                    .andThen(ExecutionSequence.possibleFuture)
                    .replace(tail)
              case Nil          =>
                executionConfigOptional
                  .andThen(ExecutionConfig.science)
                  .replace(none)

        def replaceRemoteAcquisition[D](
          editableOptional: Optional[EditableSequence, Option[Atom[D]]],
          modifyRemote:     List[Atom[D]] => IO[Unit]
        ): IO[Unit] =
          editableSequence.get
            .flatMap(editableOptional.getOption)
            .flatten
            .foldMap: atom =>
              modifyRemote(List(atom))

        def replaceRemoteScience[D](
          editableOptional: Optional[EditableSequence, List[Atom[D]]],
          modifyRemote:     List[Atom[D]] => IO[Unit]
        ): IO[Unit] =
          editableSequence.get
            .flatMap(editableOptional.getOption)
            .foldMap: atoms =>
              modifyRemote(atoms)

        def replaceRemoteSequence[D](
          acquisitionOptional: Optional[EditableSequence, Option[Atom[D]]],
          scienceOptional:     Optional[EditableSequence, List[Atom[D]]],
          modifyRemote:        SequenceType => List[Atom[D]] => IO[Unit]
        ): Callback =
          (replaceRemoteAcquisition(acquisitionOptional, modifyRemote(SequenceType.Acquisition)),
           replaceRemoteScience(scienceOptional, modifyRemote(SequenceType.Science))
          ).parTupled.void.runAsync

        def replaceLocalSequence[S, D](
          acquisitionOptional:     Optional[EditableSequence, Option[Atom[D]]],
          scienceOptional:         Optional[EditableSequence, List[Atom[D]]],
          executionConfigOptional: Optional[InstrumentExecutionConfig, ExecutionConfig[S, D]]
        ): Callback =
          liveSequence.sequence.toOption
            .flatMap(_.toOptionView)
            .foldMap:
              _.zoom(SequenceData.config).mod:
                replaceAcquisition(acquisitionOptional, executionConfigOptional) >>>
                  replaceScience(scienceOptional, executionConfigOptional)

        def replaceSequence[S, D](
          acquisitionOptional:     Optional[EditableSequence, Option[Atom[D]]],
          scienceOptional:         Optional[EditableSequence, List[Atom[D]]],
          executionConfigOptional: Optional[InstrumentExecutionConfig, ExecutionConfig[S, D]],
          modifyRemote:            SequenceType => List[Atom[D]] => IO[Unit]
        ): Callback =
          replaceRemoteSequence(acquisitionOptional, scienceOptional, modifyRemote) >>
            replaceLocalSequence(acquisitionOptional, scienceOptional, executionConfigOptional)

        val commitEdits: Callback =
          liveSequence.sequenceInstrument.foldMap:
            case Instrument.GmosNorth  =>
              replaceSequence(
                EditableSequence.gmosNorthAcquisition,
                EditableSequence.gmosNorthScience,
                InstrumentExecutionConfig.gmosNorth
                  .andThen(InstrumentExecutionConfig.GmosNorth.executionConfig),
                seqType => atoms => ctx.odbApi.replaceGmosNorthSequence(props.obsId, seqType, atoms)
              )
            case Instrument.GmosSouth  =>
              replaceSequence(
                EditableSequence.gmosSouthAcquisition,
                EditableSequence.gmosSouthScience,
                InstrumentExecutionConfig.gmosSouth
                  .andThen(InstrumentExecutionConfig.GmosSouth.executionConfig),
                seqType => atoms => ctx.odbApi.replaceGmosSouthSequence(props.obsId, seqType, atoms)
              )
            case Instrument.Flamingos2 =>
              replaceSequence(
                EditableSequence.flamingos2Acquisition,
                EditableSequence.flamingos2Science,
                InstrumentExecutionConfig.flamingos2
                  .andThen(InstrumentExecutionConfig.Flamingos2.executionConfig),
                seqType =>
                  atoms => ctx.odbApi.replaceFlamingos2Sequence(props.obsId, seqType, atoms)
              )
            case Instrument.Igrins2    => Callback.empty
            case _                     => Callback.empty

        def resolveAcquisition[S, D](
          config:           ExecutionConfig[S, D],
          editableOptional: Optional[EditableSequence, Option[Atom[D]]]
        ): Option[Atom[D]] = // For acquisition, we ignore possibleFuture
          if props.isEditing.get then
            editableSequence.get.flatMap(editableOptional.getOption).flatten
          else config.acquisition.map(_.nextAtom)

        def resolveScience[S, D](
          config:           ExecutionConfig[S, D],
          editableOptional: Optional[EditableSequence, List[Atom[D]]]
        ): Option[List[Atom[D]]] =
          if props.isEditing.get then editableSequence.get.flatMap(editableOptional.getOption)
          else config.science.map(a => a.nextAtom +: a.possibleFuture)

        def modSequence[D](
          editableOptional: Optional[EditableSequence, D]
        ): Endo[D] => Callback =
          undoCtx
            .zoom(Iso.id.some.andThen(editableOptional))
            .foldMap(_.mod)

        val title =
          <.span(
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
                  timeDisplay("Planned", total, timeClass = staleCss, timeTooltip = staleTooltip)

                <.span(ExploreStyles.SequenceTileTitle)(
                  <.span(ExploreStyles.SequenceTileTitleSide, ExploreStyles.SequenceTileTitleUndo)(
                    UndoButtons(undoCtx, size = PlSize.Mini).when(props.isEditing.get)
                  ),
                  <.span(ExploreStyles.SequenceTileTitleSummary)(
                    HelpIcon("target/main/sequence-times.md".refined),
                    planned,
                    executed,
                    pending
                  ),
                  <.span(
                    ExploreStyles.SequenceTileTitleSide,
                    ExploreStyles.SequenceTileTitleEdit
                  )(
                    Button(
                      onClick = props.isEditing.set(IsEditing.True),
                      label = "Edit",
                      icon = Icons.Pencil,
                      tooltip = "Enter sequence editing mode",
                      tooltipOptions = TooltipOptions.Top
                    ).mini.compact
                      .when(
                        props.isEditable && !props.isEditing.get && sizeState.isMaximized && liveSequence.isReady
                      ),
                    React
                      .Fragment(
                        Button(
                          onClick = props.isEditing.set(IsEditing.False) >>
                            resetEditableSequenceFrom(liveSequence),
                          label = "Cancel",
                          icon = Icons.Close,
                          tooltip = "Cancel sequence editing",
                          tooltipOptions = TooltipOptions.Top,
                          severity = Button.Severity.Danger
                        ).mini.compact,
                        Button(
                          onClick = props.isEditing.set(IsEditing.False) >> commitEdits,
                          label = "Accept",
                          icon = Icons.Checkmark,
                          tooltip = "Accept sequence modifications",
                          tooltipOptions = TooltipOptions.Top,
                          severity = Button.Severity.Success
                        ).mini.compact
                      )
                      .when(props.isEditing.get)
                  )
                )
              .getOrElse(executed)
          )

        val mismatchError = Message(
          text = "ERROR: Sequence and S/N are inconsistent.",
          severity = Message.Severity.Error
        )

        val body =
          props.sequenceChanged.get
            .flatMap: _ =>
              (liveSequence.visits.value, liveSequence.sequence.value.map(_.get)).tupled
            .renderPot(
              (visitsViewOpt, sequenceDataOpt) =>
                // TODO Show visits even if sequence data is not available
                sequenceDataOpt
                  .fold[VdomNode](
                    Message(
                      text = "Empty or incomplete sequence data returned by server",
                      severity = Message.Severity.Error
                    )
                  ) {
                    case SequenceData(InstrumentExecutionConfig.GmosNorth(config), signalToNoise) =>
                      signalToNoise match
                        case ModeSignalToNoise.Spectroscopy(acquisitionSn, scienceSn) =>
                          GmosNorthSpectroscopySequenceTable(
                            visitsViewOpt,
                            config.static,
                            resolveAcquisition(config, EditableSequence.gmosNorthAcquisition),
                            resolveScience(config, EditableSequence.gmosNorthScience),
                            acquisitionSn,
                            scienceSn,
                            props.isEditing.get,
                            modSequence(EditableSequence.gmosNorthAcquisition),
                            modSequence(EditableSequence.gmosNorthScience),
                            props.isUserStaffOrAdmin
                          )
                        case ModeSignalToNoise.GmosNorthImaging(snPerFilter)          =>
                          GmosNorthImagingSequenceTable(
                            visitsViewOpt,
                            config.static,
                            resolveAcquisition(config, EditableSequence.gmosNorthAcquisition),
                            resolveScience(config, EditableSequence.gmosNorthScience),
                            snPerFilter,
                            props.isEditing.get,
                            modSequence(EditableSequence.gmosNorthAcquisition),
                            modSequence(EditableSequence.gmosNorthScience),
                            props.isUserStaffOrAdmin
                          )
                        case _                                                        => mismatchError
                    case SequenceData(InstrumentExecutionConfig.GmosSouth(config), signalToNoise) =>
                      signalToNoise match
                        case ModeSignalToNoise.Spectroscopy(acquisitionSn, scienceSn) =>
                          GmosSouthSpectroscopySequenceTable(
                            visitsViewOpt,
                            config.static,
                            resolveAcquisition(config, EditableSequence.gmosSouthAcquisition),
                            resolveScience(config, EditableSequence.gmosSouthScience),
                            acquisitionSn,
                            scienceSn,
                            props.isEditing.get,
                            modSequence(EditableSequence.gmosSouthAcquisition),
                            modSequence(EditableSequence.gmosSouthScience),
                            props.isUserStaffOrAdmin
                          )
                        case ModeSignalToNoise.GmosSouthImaging(snPerFilter)          =>
                          GmosSouthImagingSequenceTable(
                            visitsViewOpt,
                            config.static,
                            resolveAcquisition(config, EditableSequence.gmosSouthAcquisition),
                            resolveScience(config, EditableSequence.gmosSouthScience),
                            snPerFilter,
                            props.isEditing.get,
                            modSequence(EditableSequence.gmosSouthAcquisition),
                            modSequence(EditableSequence.gmosSouthScience),
                            props.isUserStaffOrAdmin
                          )
                        case _                                                        => mismatchError
                    case SequenceData(
                          InstrumentExecutionConfig.Flamingos2(config),
                          ModeSignalToNoise.Spectroscopy(acquisitionSn, scienceSn)
                        ) =>
                      Flamingos2SequenceTable(
                        visitsViewOpt,
                        config.static,
                        resolveAcquisition(config, EditableSequence.flamingos2Acquisition),
                        resolveScience(config, EditableSequence.flamingos2Science),
                        acquisitionSn,
                        scienceSn,
                        props.isEditing.get,
                        modSequence(EditableSequence.flamingos2Acquisition),
                        modSequence(EditableSequence.flamingos2Science),
                        props.isUserStaffOrAdmin
                      )
                    case SequenceData(
                          InstrumentExecutionConfig.Igrins2(config),
                          ModeSignalToNoise.Spectroscopy(acquisitionSn, scienceSn)
                        ) =>
                      Igrins2SequenceTable(
                        visitsViewOpt,
                        config.static,
                        config.science.map(a => a.nextAtom +: a.possibleFuture),
                        scienceSn,
                        IsEditing.False,
                        props.isUserStaffOrAdmin
                      )
                    case _                                                                        => mismatchError
                  },
              errorRender = m =>
                <.div(ExploreStyles.SequencesPanelError)(
                  Message(
                    text = m.getMessage,
                    severity = Message.Severity.Warning,
                    icon = Icons.ExclamationTriangle
                  )
                )
            )

        TileContents(title, body)
    )

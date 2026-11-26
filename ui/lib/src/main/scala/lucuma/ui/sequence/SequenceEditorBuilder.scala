// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import cats.Eq
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.SequenceType
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.react.primereact.Message
import lucuma.react.table.*
import lucuma.ui.primereact.ToastCtx
import lucuma.ui.reusability.given
import lucuma.ui.sequence.*
import lucuma.ui.syntax.effect.*
import lucuma.ui.table.*
import lucuma.ui.table.hooks.*
import lucuma.ui.undo.UndoContext
import lucuma.ui.undo.UndoStacks
import org.scalajs.dom.HTMLElement
import org.typelevel.log4cats.Logger

import scalajs.js

trait SequenceEditorBuilder[D: Eq] extends SequenceEditOptics[D]:
  private type TableRow = Expandable[HeaderOrRow[SequenceHeaderContext[D], SequenceIndexedRow[D]]]

  /** What a future step row carries while dragged: its step id and sequence type. */
  protected type DragData = Option[(Step.Id, SequenceType)]

  private def useSeqTypeEditContext(
    isEditing:     View[IsEditing],
    isStepOngoing: Boolean,
    sequence:      View[List[Atom[D]]],
    remoteReplace: List[Atom[D]] => IO[List[Atom[D]]]
  )(using
    Logger[IO],
    ToastCtx[IO],
    Reusability[List[Atom[D]]]
  ): HookResult[SequenceEditContext[D]] =
    for
      editableSequence         <- useStateView(EditableSequence(sequence.get))
      resetEditableSequenceFrom = // Sequence => Callback
        (newSequence: List[Atom[D]]) => editableSequence.set(EditableSequence(newSequence))
      isEditInFlight           <- useStateView(IsEditInFlight.False)
      _                        <-
        useEffectWithDeps(sequence.get): newSequence =>
          (ToastCtx[IO] // Invalidate edit sequence if we were editing.
            .showToast(
              "The sequence was modified remotely. The edit session has been canceled.",
              Message.Severity.Warning,
              sticky = true
            )
            .runAsyncAndForget >>
            // A change during our own save is most likely the save itself.
            isEditing.set(IsEditing.False)).when_(isEditing.get && !isEditInFlight.get) >>
            // Keep editable sequence in sync with live sequence
            resetEditableSequenceFrom(newSequence).unless_(isEditInFlight.get)
      _                        <-
        // Also checked when a save ends, since one that failed leaves the session open.
        useEffectWithDeps((isStepOngoing, isEditInFlight.get.value)): (stepOngoing, _) =>
          (ToastCtx[IO] // The sequence can't be replaced while executing, so the edits are lost.
            .showToast(
              "The sequence started executing. The edit session has been canceled.",
              Message.Severity.Warning,
              sticky = true
            )
            .runAsyncAndForget >>
            isEditing.set(IsEditing.False) >>
            resetEditableSequenceFrom(sequence.get))
            .when_(stepOngoing && isEditing.get && !isEditInFlight.get)
      undoStacks               <- useStateView(UndoStacks.empty[IO, EditableSequence[D]])
      _                        <- useEffectWithDeps(isEditing.get.value): editing =>
                                    undoStacks.set(UndoStacks.empty[IO, EditableSequence[D]]) >>
                                      // Every edit session starts from the saved steps and their ids.
                                      resetEditableSequenceFrom(sequence.get).when_(editing)
    yield SequenceEditContext(
      isEditing,
      isEditInFlight.get,
      UndoContext(undoStacks, editableSequence),
      // Deferred so that the mutation input, which encodes the whole sequence, is only built when
      // the edit is accepted rather than on every render.
      onAccept = IO
        .defer(remoteReplace(editableSequence.get.value))
        .flatMap: saved =>
          // A sequence that changed during the save came from the ODB after it, so it is kept.
          sequence.async.mod(current => if current === sequence.get then saved else current)
        .onError: e =>
          ToastCtx[IO]
            .showToast(
              s"Failed to update sequence: ${e.getMessage}",
              Message.Severity.Error,
              sticky = true
            )
        .switching(isEditInFlight.as(IsEditInFlight.Value).async) >>
        isEditing.async.set(IsEditing.False),
      onCancel = isEditing.set(IsEditing.False) >> resetEditableSequenceFrom(sequence.get),
      sequence.get
    )

  /**
   * The edit contexts of both sequence types. An edit session in progress is canceled when
   * `isStepOngoing` becomes true.
   */
  protected def useEditContexts(
    isEditEnabled:        IsEditEnabled,
    isStepOngoing:        Boolean,
    isEditingAcquisition: View[IsEditing],
    isEditingScience:     View[IsEditing],
    acquisition:          View[List[Atom[D]]],
    science:              View[List[Atom[D]]],
    remoteReplace:        SequenceType => List[Atom[D]] => IO[List[Atom[D]]]
  )(using
    Logger[IO],
    ToastCtx[IO]
  ): HookResult[SequenceEditContexts[D]] =
    for
      acqEditContext <-
        useSeqTypeEditContext(
          isEditingAcquisition,
          isStepOngoing,
          acquisition,
          remoteReplace(SequenceType.Acquisition)
        )
      sciEditContext <-
        useSeqTypeEditContext(
          isEditingScience,
          isStepOngoing,
          science,
          remoteReplace(SequenceType.Science)
        )
    yield SequenceEditContexts(
      isEditEnabled = isEditEnabled,
      isStepOngoing = isStepOngoing,
      acquisition = acqEditContext,
      science = sciEditContext
    )

  /** The sequence types with an edit session open. */
  protected def editingTypes(editContexts: SequenceEditContexts[D]): Set[SequenceType] =
    Set(
      SequenceType.Acquisition -> editContexts.acquisition.isEditing.get.value,
      SequenceType.Science     -> editContexts.science.isEditing.get.value
    ).collect:
      case (sequenceType, true) => sequenceType

  /** Drag and drop of future steps, within the same sequence type. */
  protected def useSequenceTableDragAndDrop[TM <: SequenceTableMeta[D], CM, TF](
    table:        Table[TableRow, TM, CM, TF],
    containerRef: js.UndefOr[Ref.ToVdom[HTMLElement]]
  ): HookResult[UseVirtualizedTableDragAndDrop[DragData, TableRow, TM, CM, TF]] =
    useVirtualizedTableDragAndDrop(
      table,
      SequenceColumns.DragHandleColumnId,
      getData = _.original.value.toOption
        .flatMap:
          _.step match
            case SequenceRow.futureStep(fs) => (fs.stepId, fs.seqType).some
            case _                          => none,
      containerRef = containerRef,
      onDrop = (sourceData, target) =>
        (table.options.meta,
         sourceData.map(_._1),
         sourceData.map(_._2),
         target.flatMap(_.data.map(_._1)),
         target.map(_.edge)
        )
          .mapN: (meta, sourceStepId, seqType, targetStepId, edge) =>
            meta.editContexts.seqTypeMod(seqType):
              moveStep(sourceStepId, targetStepId, edge)
          .orEmpty,
      canDrop = (targetData, sourceArgs) =>
        targetData.exists: // Only allow dragging into same sequence type
          case (targetStepId, targetSeqType) =>
            sourceArgs.source.data.value.exists: (sourceStepId, sourceSeqType) =>
              sourceSeqType === targetSeqType && sourceStepId != targetStepId
    )

  /** The drag handle and edit controls columns take space only while editing. */
  protected def useEditColumnsCollapse(
    dynTable:  UseDynTable,
    isEditing: Boolean
  ): HookResult[Unit] =
    useEffectWithDeps(isEditing): isEditing =>
      dynTable.modCollapsedCols(_ => !isEditing)

  protected val EditColumnIds: Set[ColumnId] =
    Set(SequenceColumns.DragHandleColumnId, SequenceColumns.EditControlsColumnId)

  /** Header cell mod for the columns in `EditColumnIds`. */
  protected def editColumnHeaderCellMod(isEditing: Boolean): TagMod =
    SequenceStyles.HiddenColTableHeader.unless(isEditing)

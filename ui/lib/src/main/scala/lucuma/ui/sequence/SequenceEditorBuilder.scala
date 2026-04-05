// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import cats.Eq
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import japgolly.scalajs.react.*
import lucuma.core.enums.SequenceType
import lucuma.core.model.sequence.Atom
import lucuma.react.primereact.Message
import lucuma.ui.primereact.ToastCtx
import lucuma.ui.reusability.given
import lucuma.ui.sequence.*
import lucuma.ui.syntax.effect.*
import lucuma.ui.undo.UndoContext
import lucuma.ui.undo.UndoStacks
import org.typelevel.log4cats.Logger
// import lucuma.core.enums.Instrument

trait SequenceEditorBuilder[D: Eq]: // (instrument: Instrument):

  private def useSeqTypeEditContext(
    isEditing:     View[IsEditing],
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
            isEditing.set(IsEditing.False)).when_(isEditing.get) >>
            // Keep editable sequence in sync with live sequence
            resetEditableSequenceFrom(newSequence).unless_(isEditInFlight.get)
      undoStacks               <- useStateView(UndoStacks.empty[IO, EditableSequence[D]])
      _                        <- useEffectWithDeps(isEditing.get.value): _ =>
                                    undoStacks.set(UndoStacks.empty[IO, EditableSequence[D]])
    yield SequenceEditContext(
      isEditing,
      isEditInFlight.get,
      UndoContext(undoStacks, editableSequence),
      onAccept = ((remoteReplace(editableSequence.get.value) >>= sequence.async.set).onError: e =>
        ToastCtx[IO]
          .showToast(
            s"Failed to update sequence: ${e.getMessage}",
            Message.Severity.Error,
            sticky = true
          )
          .switching(isEditInFlight.as(IsEditInFlight.Value).async)) >>
        isEditing.async.set(IsEditing.False),
      onCancel = isEditing.set(IsEditing.False) >> resetEditableSequenceFrom(sequence.get),
      sequence.get
    )

  protected def useEditContexts(
    isEditEnabled:        IsEditEnabled,
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
          acquisition,
          remoteReplace(SequenceType.Acquisition)
        )
      sciEditContext <-
        useSeqTypeEditContext(
          isEditingScience,
          science,
          remoteReplace(SequenceType.Science)
        )
    yield SequenceEditContexts(
      isEditEnabled = isEditEnabled,
      acquisition = acqEditContext,
      science = sciEditContext
    )

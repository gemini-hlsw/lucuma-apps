// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import cats.Endo
import cats.effect.IO
import crystal.react.View
import japgolly.scalajs.react.callback.Callback
import lucuma.core.enums.SequenceType
import lucuma.core.model.sequence.Atom
import lucuma.ui.undo.UndoContext

final case class SequenceEditContext[D](
  isEditing:      View[IsEditing],
  isEditInFlight: IsEditInFlight,
  undoCtx:        UndoContext[EditableSequence[D]],
  onAccept:       IO[Unit],
  onCancel:       Callback,
  liveSequence:   List[Atom[D]]
):
  def resolvedSequence: List[Atom[D]] =
    if (isEditing.get) undoCtx.get.value else liveSequence

final case class SequenceEditContexts[D](
  isEditEnabled: IsEditEnabled,
  acquisition:   SequenceEditContext[D],
  science:       SequenceEditContext[D]
):
  def isEditing: Boolean =
    acquisition.isEditing.get || science.isEditing.get

  def forSequenceType(sequenceType: SequenceType): SequenceEditContext[D] =
    sequenceType match
      case SequenceType.Acquisition => acquisition
      case SequenceType.Science     => science

  def seqTypeMod(seqType: SequenceType): Endo[List[Atom[D]]] => Callback =
    seqType match
      case SequenceType.Acquisition =>
        f => acquisition.undoCtx.zoom(EditableSequence.Value).mod(f)
      case SequenceType.Science     =>
        f => science.undoCtx.zoom(EditableSequence.Value).mod(f)

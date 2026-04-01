// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import cats.effect.IO
import crystal.react.View
import japgolly.scalajs.react.callback.Callback
import lucuma.core.enums.SequenceType

final case class SequenceEditContext(
  isEditable:           Boolean,
  editingSequenceTypes: View[EditingSequenceTypes],
  isEditInFlight:       Boolean,
  onAccept:             IO[Unit],
  onCancel:             Callback
):
  def isEditing(seqType: SequenceType): Boolean =
    editingSequenceTypes.get.isEditing(seqType)

  def isEditing: Boolean = editingSequenceTypes.get.isEditing

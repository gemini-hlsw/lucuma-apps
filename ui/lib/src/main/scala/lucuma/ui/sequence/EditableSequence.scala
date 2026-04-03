// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import lucuma.core.model.sequence.Atom
import monocle.Iso

opaque type EditableSequence[D] = List[Atom[D]]
object EditableSequence:
  def apply[D](value: List[Atom[D]]): EditableSequence[D]           = value
  def Value[D]: Iso[EditableSequence[D], List[Atom[D]]]             =
    Iso[EditableSequence[D], List[Atom[D]]](s => s)(l => l)
  extension [D](s:    EditableSequence[D]) def value: List[Atom[D]] = s

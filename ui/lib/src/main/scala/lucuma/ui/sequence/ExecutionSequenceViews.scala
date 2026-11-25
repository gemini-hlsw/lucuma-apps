// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import cats.syntax.all.*
import crystal.react.View
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.ExecutionConfig
import lucuma.core.model.sequence.ExecutionSequence

// The whole remaining sequence as an editable list: `nextAtom` followed by `possibleFuture`.
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

  def flatScience: View[List[Atom[D]]] =
    execution.zoom(ExecutionConfig.science).flatExecutionSequence

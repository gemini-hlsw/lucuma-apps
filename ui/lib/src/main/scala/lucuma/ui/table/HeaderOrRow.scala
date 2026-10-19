// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.table

import cats.syntax.all.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.table.RowId

/**
 * A header row grouping the rows under it. A non `collapsible` one shows no chevron and doesn't
 * toggle its group on click; it is up to the table to keep it expanded.
 */
case class HeaderRow[A](rowId: RowId, content: A => VdomNode, collapsible: Boolean = true):
  def toHeaderOrRow[R]: HeaderOrRow[A, R] = this.asLeft

object HeaderRow:
  def apply[A](row: RowId, content: VdomNode): HeaderRow[A] =
    HeaderRow(row, _ => content)

  def apply[A](row: RowId, content: VdomNode, collapsible: Boolean): HeaderRow[A] =
    HeaderRow(row, _ => content, collapsible)

type HeaderOrRow[A, +R] = Either[HeaderRow[A], R]

extension [R](row: R) def toHeaderOrRow[A]: HeaderOrRow[A, R] = row.asRight

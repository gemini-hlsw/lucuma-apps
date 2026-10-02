// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.Css
import lucuma.react.table.Column
import lucuma.react.table.ColumnId

/**
 * Pins the leading columns of a table while it scrolls horizontally.
 *
 * Each sticky column is pinned at its offset from the table's left edge, taken from the sizes of
 * the visible columns before it. The offsets therefore follow resizing and hidden columns, and the
 * pinned columns stay flush. Use the same mod for header and body cells, so the headers stay pinned
 * too.
 *
 * @param columnClasses
 *   The classes of the sticky columns, which should include `ExploreStyles.StickyColumn`. Other
 *   columns get nothing.
 */
case class StickyColumns(columnClasses: Map[ColumnId, Css]):
  def mod(column: Column[?, ?, ?, ?, ?, ?, ?]): TagMod =
    columnClasses
      .get(column.id)
      .map(css => TagMod(css, ^.left := column.getStart().render))
      .getOrElse(TagMod.empty)

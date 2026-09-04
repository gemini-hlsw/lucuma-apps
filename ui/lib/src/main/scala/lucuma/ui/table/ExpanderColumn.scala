// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.table

import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.*

object ExpanderColumn:
  /**
   * The chevron cell of an expander column: blank while the row cannot expand, rotated while it is
   * expanded, and toggling without triggering the row's own click handling.
   */
  def cell(canExpand: Boolean, expanded: Boolean, toggle: Callback): VdomNode =
    if canExpand then
      <.span(
        ^.cursor.pointer,
        TableStyles.ExpanderChevron,
        TableStyles.ExpanderChevronOpen.when(expanded),
        ^.onClick ==> (_.stopPropagationCB *> toggle)
      )(TableIcons.ChevronRight.withFixedWidth(true))
    else EmptyVdom

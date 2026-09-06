// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.table.hooks

import japgolly.scalajs.react.*
import lucuma.react.table.*

private object UseResetHiddenFilters:
  /**
   * Clears column and global filters whenever the filter UI is hidden.
   */
  def useResetHiddenFilters[T, TM, CM, TF](
    table:       Table[T, TM, CM, TF],
    showFilters: Boolean
  ): HookResult[Unit] =
    val state      = table.getState()
    val hasFilters = state.columnFilters.value.nonEmpty || state.globalFilter.isDefined
    useEffectWithDeps((showFilters, hasFilters)): (showFilters, hasFilters) =>
      (table.resetColumnFilters() >> table.resetGlobalFilter())
        .when_(!showFilters && hasFilters)

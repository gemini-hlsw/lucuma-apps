// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.table.hooks

import cats.Eq
import japgolly.scalajs.react.Callback
import lucuma.react.table.*

import scalajs.js.JSConverters.*

/**
 * The subset of a table's state that constitutes a user preference
 */
case class TablePreferences[TF](
  columnVisibility: ColumnVisibility,
  columnSizing:     ColumnSizing,
  columnPinning:    ColumnPinning,
  sorting:          Sorting,
  columnFilters:    ColumnFilters,
  globalFilter:     Option[TF]
):
  /**
   * Merge with stored preferences on top of the table's defaults, stored entries win.
   */
  def withDefaultVisibility(defaults: ColumnVisibility): TablePreferences[TF] =
    copy(columnVisibility = ColumnVisibility(defaults.value ++ columnVisibility.value))

  def withoutColumns(columnIds: Set[ColumnId]): TablePreferences[TF] =
    if columnIds.isEmpty then this
    else
      copy(
        columnVisibility = ColumnVisibility(columnVisibility.value -- columnIds),
        columnSizing = ColumnSizing(columnSizing.value -- columnIds),
        columnPinning = ColumnPinning(
          columnPinning.left.filterNot(columnIds.contains),
          columnPinning.right.filterNot(columnIds.contains)
        ),
        sorting = Sorting(sorting.value.filterNot(sort => columnIds.contains(sort.columnId))),
        columnFilters = ColumnFilters(columnFilters.value -- columnIds)
      )

  def toTableState: TableState[TF] =
    TableState[TF](
      columnVisibility = columnVisibility,
      columnSizing = columnSizing,
      columnPinning = columnPinning,
      sorting = sorting,
      columnFilters = columnFilters,
      globalFilter = globalFilter.orUndefined
    )

  def applyTo[T, TM, CM](
    table:                Table[T, TM, CM, TF],
    appControlledColumns: Set[ColumnId]
  ): Callback =
    Callback.suspend:
      val appControlled: Map[ColumnId, Visibility] =
        table
          .getState()
          .columnVisibility
          .value
          .filter((colId, _) => appControlledColumns.contains(colId))
      table.setColumnVisibility(ColumnVisibility(columnVisibility.value ++ appControlled)) >>
        table.setColumnSizing(columnSizing) >>
        table.setColumnPinning(columnPinning) >>
        table.setSorting(sorting) >>
        table.setColumnFilters(columnFilters) >>
        table.setGlobalFilter(globalFilter)

object TablePreferences:
  def Empty[TF]: TablePreferences[TF] =
    TablePreferences(
      ColumnVisibility.Empty,
      ColumnSizing.Empty,
      ColumnPinning.Empty,
      Sorting.Empty,
      ColumnFilters.Empty,
      None
    )

  def fromState[TF](state: TableState[TF]): TablePreferences[TF] =
    TablePreferences(
      state.columnVisibility,
      state.columnSizing,
      state.columnPinning,
      state.sorting,
      state.columnFilters,
      state.globalFilter
    )

  given [TF]: Eq[TablePreferences[TF]] = Eq.fromUniversalEquals

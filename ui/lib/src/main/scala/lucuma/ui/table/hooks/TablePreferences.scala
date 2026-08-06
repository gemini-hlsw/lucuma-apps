// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.table.hooks

import cats.Eq
import japgolly.scalajs.react.Callback
import lucuma.react.table.*

/**
 * The subset of a table's state that constitutes a user preference
 */
case class TablePreferences(
  columnVisibility: ColumnVisibility,
  columnSizing:     ColumnSizing,
  columnPinning:    ColumnPinning,
  sorting:          Sorting
):
  /**
   * Merge with stored preferences on top of the table's defaults, stored entries win.
   */
  def withDefaultVisibility(defaults: ColumnVisibility): TablePreferences =
    copy(columnVisibility = ColumnVisibility(defaults.value ++ columnVisibility.value))

  def withoutColumns(columnIds: Set[ColumnId]): TablePreferences =
    if columnIds.isEmpty then this
    else
      copy(
        columnVisibility = ColumnVisibility(columnVisibility.value -- columnIds),
        columnSizing = ColumnSizing(columnSizing.value -- columnIds),
        columnPinning = ColumnPinning(
          columnPinning.left.filterNot(columnIds.contains),
          columnPinning.right.filterNot(columnIds.contains)
        ),
        sorting = Sorting(sorting.value.filterNot(sort => columnIds.contains(sort.columnId)))
      )

  def toTableState[TF]: TableState[TF] =
    TableState[TF](
      columnVisibility = columnVisibility,
      columnSizing = columnSizing,
      columnPinning = columnPinning,
      sorting = sorting
    )

  def applyTo[T, TM, CM, TF](
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
        table.setSorting(sorting)

object TablePreferences:
  val Empty: TablePreferences =
    TablePreferences(ColumnVisibility.Empty, ColumnSizing.Empty, ColumnPinning.Empty, Sorting.Empty)

  def fromState[TF](state: TableState[TF]): TablePreferences =
    TablePreferences(state.columnVisibility, state.columnSizing, state.columnPinning, state.sorting)

  given Eq[TablePreferences] = Eq.fromUniversalEquals

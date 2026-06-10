// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.table.hooks

import cats.syntax.all.*
import lucuma.react.SizePx
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.ui.table.*
import lucuma.ui.table.hooks.DynTable.ColState
import monocle.Focus
import monocle.Lens

import scala.annotation.tailrec

/**
 * Definition of a dynamic table to be passed to `useDynTable`. Avoid creating in the hook call,
 * since it performs coherence checks upon creation. Pass a static instance to the hook instead.
 *
 * @param columnSizes
 *   Size definitions for all the columns in the table.
 * @param droppableColumns
 *   Columns that can be dropped when there is not enough space. The ones at the beginning are
 *   removed first. Missing columns are not removed by overflow.
 * @param initialState
 */
case class DynTable(
  columnSizes:      Map[ColumnId, ColumnSize],
  droppableColumns: List[ColumnId],
  initialState:     DynTable.ColState,
  // Columns that can be collapsed to 0px but remain visible. Collapsing is triggered programatically, and they start collapsed.
  collapsibleCols:  Set[ColumnId] = Set.empty
):
  // Check columns comply with the ones passed in columnSizes
  droppableColumns.foreach(colId =>
    assert(
      columnSizes.keySet.contains(colId),
      s"DynTable.initialState.droppableColumns contains unknown column [$colId] not in columnSizes"
    )
  )
  initialState.resized.value.keySet.foreach(colId =>
    assert(
      columnSizes.keySet.contains(colId),
      s"DynTable.initialState.resized contains unknown column [$colId] not in columnSizes"
    )
  )
  initialState.visibility.value.keySet.foreach(colId =>
    assert(
      columnSizes.keySet.contains(colId),
      s"DynTable.initialState.visibility contains unknown column [$colId] not in columnSizes"
    )
  )
  initialState.droppedColumns.foreach(colId =>
    assert(
      columnSizes.keySet.contains(colId),
      s"DynTable.initialState.droppedColumns contains unknown column [$colId] not in columnSizes"
    )
  )

  // Precompute these sets to avoid doing it on every resize:
  // All undroppable columns with their sizes.
  private val allUndroppableColSizes: Set[(ColumnId, ColumnSize)] =
    columnSizes.filterNot((colId, _) => droppableColumns.contains(colId)).toSet

  // All possible sets of droppable columns with their sizes.
  private val allDroppableColSizesList: List[Set[(ColumnId, ColumnSize)]] =
    droppableColumns.tails.map(_.toSet.map(colId => colId -> columnSizes(colId))).toList

  // Filter a set of column sizes by a set of hidden columns.
  private def visibleColSizes(
    colSizes:   Set[(ColumnId, ColumnSize)],
    hiddenCols: Set[ColumnId]
  ): Set[(ColumnId, ColumnSize)] =
    colSizes.filterNot((colId, _) => hiddenCols.contains(colId))

  // Get the minimum widths of a set of columns, taking into account collapsible columns state.
  private def colSetWithMinWidths(
    colSizes:         Set[(ColumnId, ColumnSize)],
    areColsCollapsed: Boolean
  ): Set[(ColumnId, Int)] =
    colSizes
      .collect:
        case (colId, colSize) =>
          colId -> (
            if areColsCollapsed && collapsibleCols.contains(colId) then 0
            else colSize.effectiveMinSize.value
          )

  // Get the total minimum width of a set of columns, taking into account collapsible columns state.
  private def minColSetWidth(
    colSizes:         Set[(ColumnId, ColumnSize)],
    areColsCollapsed: Boolean
  ): Int =
    colSetWithMinWidths(colSizes, areColsCollapsed).toList.map(_._2).sum

  // Selects the maximal set of columns that fit in the given width, dropping columns
  // by priority and taking into account hidden columns and collapsible columns state.
  // If all droppable columns had to be dropped and still don't fit, this is returned as isOverflowing.
  private def selectFittingColumns(
    width:            SizePx,
    hiddenCols:       Set[ColumnId],
    areColsCollapsed: Boolean
  ): (fittingCols: Set[ColumnId], isOverflowing: Boolean) = {
    val undroppableColSizes: Set[(ColumnId, ColumnSize)] =
      visibleColSizes(allUndroppableColSizes, hiddenCols)
    val undroppableColsMinWidth: Int                     = minColSetWidth(undroppableColSizes, areColsCollapsed)
    allDroppableColSizesList
      .collectFirstSome: droppableColSizesSet =>
        val droppableColSizes: Set[(ColumnId, ColumnSize)] =
          visibleColSizes(droppableColSizesSet, hiddenCols)
        val droppableColSizesMinWidth: Int                 = minColSetWidth(droppableColSizes, areColsCollapsed)
        if (undroppableColsMinWidth + droppableColSizesMinWidth <= width.value)
          (undroppableColSizes ++ droppableColSizes).map(_._1).some
        else none
      .map((_, false))
      .getOrElse((undroppableColSizes.map(_._1), true))
  }

  // Initial column sizes, taking into account collapsible columns state. To be used when initializing the table.
  val initialColumnSizes: Map[ColumnId, ColumnSize] =
    columnSizes ++ collapsibleCols
      .map: colId =>
        colId -> ColumnSize.Resizable(0.toPx, min = 0.toPx.some, max = columnSizes(colId).maxSize)
      .toMap

  // Distributes `width` among the given columns proportionally to their input sizes, respecting each
  // column's min/max bounds. A column that can't absorb its proportional share (because of a bound)
  // is pinned to the bound and the remainder is redistributed among the rest. If the columns' max
  // sizes don't add up to `width`, the leftover space is left unfilled.
  private def fitColumnWidths(
    inputSizes:       Map[ColumnId, SizePx],
    width:            SizePx,
    areColsCollapsed: Boolean
  ): Map[ColumnId, SizePx] = {
    // Collapsed collapsible columns are pinned to 0; otherwise use the column's declared bounds.
    def boundsOf(colId: ColumnId): (Int, Option[Int]) =
      if areColsCollapsed && collapsibleCols.contains(colId) then (0, 0.some)
      else
        val colSize = columnSizes(colId)
        (colSize.effectiveMinSize.value, colSize.maxSize.map(_.value))

    // Allocates `target` px among `weights` proportionally, as integers that sum exactly to `target`
    // (largest-remainder method).
    def allocate(weights: Map[ColumnId, Int], target: Int): Map[ColumnId, Int] = {
      val total: Int = weights.values.sum
      // With no proportion basis (all inputs 0) columns stay at 0 rather than being spread evenly.
      if weights.isEmpty || total <= 0 then weights.view.mapValues(_ => 0).toMap
      else
        val raw: Map[ColumnId, Double] =
          weights.view.mapValues(w => w.toDouble * target / total).toMap
        val floors: Map[ColumnId, Int] = raw.view.mapValues(r => Math.floor(r).toInt).toMap
        val leftover: Int              = target - floors.values.sum
        // The `leftover` px from truncation go to the columns with the largest fractional parts.
        val getsExtra: Set[ColumnId]   =
          raw.toList.sortBy((_, r) => Math.floor(r) - r).map(_._1).take(leftover.max(0)).toSet
        floors.map((colId, f) => colId -> (f + (if getsExtra(colId) then 1 else 0)))
    }

    // The pinned set only grows, so this terminates in at most `inputSizes.size` iterations.
    @tailrec
    def loop(free: Map[ColumnId, Int], pinned: Map[ColumnId, Int]): Map[ColumnId, Int] = {
      val proposed: Map[ColumnId, Int]    = allocate(free, width.value - pinned.values.sum)
      val newlyPinned: Map[ColumnId, Int] =
        proposed.flatMap: (colId, size) =>
          val (min, max): (Int, Option[Int]) = boundsOf(colId)
          if size < min then (colId -> min).some
          else max.filter(size > _).map(colId -> _)
      if newlyPinned.isEmpty then pinned ++ proposed
      else loop(free -- newlyPinned.keySet, pinned ++ newlyPinned)
    }

    loop(inputSizes.view.mapValues(_.value).toMap, Map.empty).view.mapValues(SizePx(_)).toMap
  }

  // Adjusts the state to fit the given width, by dropping columns if necessary
  // according to the defined priority and resizing the rest to fit the remaining space.
  def adjustColSizes(width: SizePx, areColsCollapsed: Boolean)(
    colState: DynTable.ColState
  ): DynTable.ColState = {
    val hiddenCols: Set[ColumnId] =
      colState.visibility.value
        .collect:
          case (colId, Visibility.Hidden) => colId
        .toSet

    val (fittingColumns, isOverflowing): (Set[ColumnId], Boolean) =
      selectFittingColumns(width, hiddenCols, areColsCollapsed)

    if isOverflowing then
      colState.copy(
        resized = ColumnSizing(
          colSetWithMinWidths(
            visibleColSizes(allUndroppableColSizes, hiddenCols),
            areColsCollapsed
          ).map((colId, colSize) => colId -> colSize.toPx).toMap
        ),
        droppedColumns = droppableColumns.toSet
      )
    else
      // Current sizes are the proportion basis.
      val inputSizes: Map[ColumnId, SizePx] =
        fittingColumns
          .map: colId =>
            colId -> colState.resized.value.getOrElse(colId, columnSizes(colId).initial)
          .toMap

      colState.copy(
        resized = ColumnSizing(fitColumnWidths(inputSizes, width, areColsCollapsed)),
        droppedColumns = droppableColumns.toSet -- fittingColumns
      )
  }

object DynTable:
  /**
   * State of the columns in a dynamic table.
   *
   * @param resized
   *   Resized columns. To be passed directly to table `state` as `PartialTableState.columnSizing`.
   * @param visibility
   *   Column visibility without taking into account overflows. Do not pass to table. See
   *   `computedVisibility` instead.
   * @param droppedColumns
   *   Columns dropped because they would overflow.
   */
  case class ColState(
    val resized:        ColumnSizing,
    val visibility:     ColumnVisibility,
    val droppedColumns: Set[ColumnId] = Set.empty
  ):
    /**
     * Column visitibility. To be passed directly to table `state` as
     * `PartialTableState.columnVisibility`.
     */
    lazy val computedVisibility: ColumnVisibility =
      visibility.modify(_ ++ droppedColumns.map(_ -> Visibility.Hidden))

    /**
     * The same state without overflows. Useful when recomputing them.
     */

  object ColState:
    val resized: Lens[ColState, ColumnSizing]         = Focus[ColState](_.resized)
    val visibility: Lens[ColState, ColumnVisibility]  = Focus[ColState](_.visibility)
    val droppedColumns: Lens[ColState, Set[ColumnId]] = Focus[ColState](_.droppedColumns)

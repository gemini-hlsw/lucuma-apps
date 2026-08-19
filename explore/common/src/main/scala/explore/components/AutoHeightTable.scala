// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

/**
 * A virtualized table in an auto-height tile sizes itself to the space it is given, so it cannot be
 * measured: it has to declare the height it wants as the tile's `contentHeightPx`.
 */
object AutoHeightTable:
  val RowHeightPx: Int = 32

  // A single row of the same font, plus its padding and bottom border.
  private val HeaderPx: Int = 40

  /** The `contentHeightPx` a table of `rowCount` rows wants. */
  def heightPx(rowCount: Int): Int =
    HeaderPx + rowCount * RowHeightPx

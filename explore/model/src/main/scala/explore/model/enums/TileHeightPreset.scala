// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import cats.Order

/**
 * Preset row spans selectable from a tile's title bar
 */
enum TileHeightPreset(val rows: Int):
  case Small  extends TileHeightPreset(6)
  case Medium extends TileHeightPreset(9)
  case Large  extends TileHeightPreset(14)
  case XLarge extends TileHeightPreset(20)

object TileHeightPreset:
  given Order[TileHeightPreset] = Order.by(_.rows)

// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import explore.components.*
import explore.model.ObsTabTileIds
import japgolly.scalajs.react.vdom.html_<^.*

final case class DaytimeConfigurationTile()
    extends Tile[DaytimeConfigurationTile](
      ObsTabTileIds.DaytimeConfigurationId.id,
      "Configuration"
    )(DaytimeConfigurationTile)

object DaytimeConfigurationTile
    extends TileComponent[DaytimeConfigurationTile]((_, _) => TileContents(EmptyVdom))

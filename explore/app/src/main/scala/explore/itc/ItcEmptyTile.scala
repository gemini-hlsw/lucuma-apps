// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import explore.components.*
import explore.components.ui.ExploreStyles
import explore.model.ObsTabTileIds
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.primereact.Message

final case class ItcEmptyTile()
    extends Tile[ItcEmptyTile](
      ObsTabTileIds.ItcId.id,
      "ITC",
      bodyClass = ExploreStyles.ItcTileBody
    )(ItcEmptyTile)

object ItcEmptyTile
    extends TileComponent[ItcEmptyTile]((_, _) =>
      TileContents:
        Message(
          text = "Select an observing mode to view ITC results",
          severity = Message.Severity.Info
        )
    )

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import explore.model.enums.TileSizeState
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.style.*

case class SimpleTile(
  override val id:               Tile.TileId,
  override val title:            VdomNode,
  override val renderBackButton: Option[VdomNode] = None,
  override val canMinimize:      Boolean = true,
  override val canMaximize:      Boolean = true,
  override val hidden:           Boolean = false,
  override val initialSizeState: TileSizeState = TileSizeState.Maximized,
  override val controllerClass:  Css =
    Css.Empty, // applied to wrapping div when in a TileController.
  override val bodyClass:        Css = Css.Empty, // applied to tile body
  override val tileClass:        Css = Css.Empty, // applied to the tile
  override val tileTitleClass:   Css = Css.Empty  // applied to the title
)(
  render:                        TileSizeState => HookResult[TileContents]
) extends Tile[SimpleTile](
      id,
      title,
      renderBackButton,
      canMinimize,
      canMaximize,
      hidden,
      initialSizeState,
      controllerClass,
      bodyClass,
      tileClass,
      tileTitleClass
    )(SimpleTile):
  val renderFn: TileSizeState => HookResult[TileContents] = render

object SimpleTile
    extends TileComponent[SimpleTile]({ (props, tileSize) =>
      props.renderFn(tileSize)
    })

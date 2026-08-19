// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*

/**
 * @param title
 *   title of the tile
 * @param body
 *   body of the tile
 * @param contentHeightPx
 *   the height the body wants, in pixels. Only meaningful on an auto-height tile, where it replaces
 *   the measurement of the rendered body.
 */
final case class TileContents(
  title:           TagMod,
  body:            TagMod,
  contentHeightPx: Option[Int] = None
)

object TileContents:
  def apply(body: TagMod): TileContents =
    TileContents(EmptyVdom, body)

  given Conversion[TileContents, HookResult[TileContents]] with
    def apply(tc: TileContents): HookResult[TileContents] =
      HookResult(tc)

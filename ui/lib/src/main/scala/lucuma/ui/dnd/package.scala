// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.dnd

import lucuma.react.pragmaticdnd.facade.Edge
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.SizePx

private val OpeningColor = "purple"

def dragOverStyle(height: SizePx, edge: Edge): TagMod =
  edge match
    case Edge.Top    =>
      TagMod(
        ^.paddingTop         := height.render,
        ^.transitionDuration := "0.1s",
        ^.backgroundImage    := s"linear-gradient(to bottom, $OpeningColor 0px, $OpeningColor ${height.render}, transparent ${height.render})"
      )
    case Edge.Bottom =>
      TagMod(
        ^.paddingBottom      := height.render,
        ^.transitionDuration := "0.1s",
        ^.backgroundImage    := s"linear-gradient(to top, $OpeningColor 0px, $OpeningColor ${height.render}, transparent ${height.render})"
      )
    case _           => TagMod.empty

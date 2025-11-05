// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.visualization

import cats.syntax.all.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.svg_<^.*
import lucuma.react.common.Css
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.tooltip.*
import lucuma.ui.syntax.all.given

case class BlindOffsetTarget(
  offP:     Double,
  offQ:     Double,
  maxP:     Long,
  radius:   Double,
  pointCss: Css,
  title:    Option[String] = "blind offset".some
) extends ReactFnProps(BlindOffsetTarget)

object BlindOffsetTarget
    extends ReactFnComponent[BlindOffsetTarget](p =>
      val pointCss = VisualizationStyles.BlindOffsetTarget |+| p.pointCss

      <.circle(VisualizationStyles.VisualizationTooltipTarget)(
        ^.cx := scale(p.offP),
        ^.cy := scale(p.offQ),
        ^.r  := scale(p.maxP * p.radius),
        pointCss
      ).withTooltipOptions(content = p.title.getOrElse("<>"))
    )

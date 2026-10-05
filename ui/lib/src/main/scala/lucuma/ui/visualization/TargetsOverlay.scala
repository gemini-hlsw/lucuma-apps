// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.visualization

import cats.syntax.all.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.svg_<^.*
import lucuma.react.common.Css
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Tooltip
import lucuma.react.primereact.tooltip.*
import lucuma.ui.aladin.*
import lucuma.ui.syntax.all.given

case class TargetsOverlay(
  width:   Int,
  height:  Int,
  aladin:  Aladin,
  targets: List[SvgTarget]
) extends ReactFnProps(TargetsOverlay)

object TargetsOverlay
    extends ReactFnComponent[TargetsOverlay](p =>
      // The svg is drawn in canvas pixels and positions come from aladin's own projection.
      // Anything well outside the canvas is dropped or clipped, as large coordinates are
      // clamped by some browsers
      val minX = -ClipMargin
      val minY = -ClipMargin
      val maxX = p.width + ClipMargin
      val maxY = p.height + ClipMargin

      def inView(x: Double, y: Double): Boolean =
        x >= minX && x <= maxX && y >= minY && y <= maxY

      val targetsWithPixels: List[(Double, Double, SvgTarget)] = p.targets
        .flatMap: target =>
          p.aladin
            .world2pixel(target.coordinates)
            .filter: (x, y) =>
              target match
                case SvgTarget.LineTo(_, _, _, _) => true // Clipped when drawn
                case _                            => inView(x, y)
            .map((x, y) => (x, y, target))

      // 24 October 2024 - scalafix failing to parse with fewer braces
      val guideStarTooltips: List[VdomNode] =
        p.targets.collect:
          case SvgTarget.GuideStarCandidateTarget(analysis = ags) =>
            Tooltip(
              clazz = VisualizationStyles.VisualizationTooltip,
              targetCss = ags.target.selector
            )(GuideStarTooltip(ags))
          case SvgTarget.GuideStarTarget(analysis = ags)          =>
            Tooltip(
              clazz = VisualizationStyles.VisualizationTooltip,
              targetCss = ags.target.selector
            )(GuideStarTooltip(ags))

      val svg: VdomNode = <.svg(
        VisualizationStyles.TargetsSvg,
        ^.viewBox    := s"0 0 ${p.width} ${p.height}",
        canvasWidth  := s"${p.width}px",
        canvasHeight := s"${p.height}px"
      )(
        <.g(VisualizationStyles.JtsTargets)(
          targetsWithPixels
            .collect[VdomNode] {
              case (x, y, SvgTarget.CircleTarget(_, css, radius, title))    =>
                val pointCss: Css = VisualizationStyles.CircleTarget |+| css

                <.circle(
                  ^.cx := x,
                  ^.cy := y,
                  ^.r  := radius,
                  pointCss,
                  title.map(<.title(_))
                )
              case (x, y, SvgTarget.CrosshairTarget(_, css, sidePx, title)) =>
                val pointCss = VisualizationStyles.CrosshairTarget |+| css

                val lines = List(
                  <.line(
                    ^.x1 := x - sidePx,
                    ^.x2 := x + sidePx,
                    ^.y1 := y,
                    ^.y2 := y,
                    pointCss
                  ),
                  <.line(
                    ^.x1 := x,
                    ^.x2 := x,
                    ^.y1 := y - sidePx,
                    ^.y2 := y + sidePx,
                    pointCss
                  )
                )
                title.fold[VdomNode](
                  <.g(lines*)
                )(t =>
                  <.g(VisualizationStyles.VisualizationTooltipTarget)(
                    (lines :+ <.circle(
                      ^.cx            := x,
                      ^.cy            := y,
                      ^.r             := sidePx,
                      ^.fill          := "transparent",
                      ^.pointerEvents := "all"
                    ))*
                  ).withTooltipOptions(content = t)
                )

              case (x, y, SvgTarget.SkyPositionTarget(_, css, sidePx, title)) =>
                val pointCss  = VisualizationStyles.SkyPositionTarget |+| css
                val side      = sidePx
                val points    =
                  s"$x,${y - side} ${x + side},$y $x,${y + side} ${x - side},$y"
                val hitSide   = side + 5.0
                val hitPoints =
                  s"$x,${y - hitSide} ${x + hitSide},$y $x,${y + hitSide} ${x - hitSide},$y"
                <.g(VisualizationStyles.VisualizationTooltipTarget)(
                  <.polygon(pointCss, ^.points := points),
                  <.polygon(
                    ^.points         := hitPoints,
                    ^.fill           := "transparent",
                    ^.pointerEvents  := "all"
                  )
                ).withTooltipOptions(content = title.getOrElse("<>"))

              case (x, y, SvgTarget.ScienceTarget(_, css, selectedCss, sidePx, selected, title)) =>
                val pointCss = VisualizationStyles.CrosshairTarget |+| css

                CrossTarget(x, y, sidePx, pointCss, selectedCss, selected, title)

              case (x, y, SvgTarget.GuideStarCandidateTarget(_, css, radius, ags, _)) =>
                val pointCss = VisualizationStyles.GuideStarCandidateTarget |+| css
                GuideStarTarget(x, y, radius, pointCss, ags)

              case (x, y, SvgTarget.GuideStarTarget(_, css, radius, ags, _)) =>
                val pointCss = VisualizationStyles.GuideStarTarget |+| css
                GuideStarTarget(x, y, radius, pointCss, ags)

              case (x, y, SvgTarget.OffsetIndicator(_, idx, o, oType, css, radius, title)) =>
                val pointCss = VisualizationStyles.OffsetPosition |+| css
                OffsetSvg(x, y, radius, pointCss, oType, idx, o)

              case (x,
                    y,
                    SvgTarget.BlindOffsetTarget(_, css, selectedCss, radius, selected, title)
                  ) =>
                BlindOffsetTarget(
                  x,
                  y,
                  radius,
                  css,
                  selectedCss,
                  selected,
                  s"Blind Offset: ${title.getOrElse("<>")}"
                )

              case (x, y, SvgTarget.LineTo(_, d, css, title)) =>
                val pointCss: Css = VisualizationStyles.ArrowBetweenTargets |+| css

                p.aladin
                  .world2pixel(d)
                  .flatMap(clipSegment(x, y, _, _, minX, minY, maxX, maxY))
                  .fold(EmptyVdom): (x1, y1, x2, y2) =>
                    <.line(
                      ^.x1 := x1,
                      ^.x2 := x2,
                      ^.y1 := y1,
                      ^.y2 := y2,
                      pointCss,
                      title.map(<.title(_))
                    )
            }
            .toTagMod
        )
      )

      val textTooltip: VdomNode = Tooltip(
        clazz = VisualizationStyles.VisualizationTooltip,
        targetCss = VisualizationStyles.VisualizationTooltipTarget
      )

      val tooltips: VdomNode = // Remount when targets change, so that the tooltips are reattached
        React.Fragment.withKey(p.targets.length)((textTooltip +: guideStarTooltips)*)

      React.Fragment(svg, tooltips)
    )

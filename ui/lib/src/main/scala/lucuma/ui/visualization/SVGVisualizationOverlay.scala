// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.visualization

import cats.data.NonEmptyList
import cats.syntax.all.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.svg_<^.*
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.ShapeInterpreter
import lucuma.core.geom.ShapePolygon
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.ui.aladin.Fov
import lucuma.ui.syntax.all.given

case class SvgVisualizationOverlay(
  width:        Int,
  height:       Int,
  fov:          Fov,
  screenOffset: Offset,
  shapes:       NonEmptyList[(Css, ShapeExpression)],
  clazz:        Css = Css.Empty,
  labels:       List[(Css, String)] = List.empty
)(using val interpreter: ShapeInterpreter)
    extends ReactFnProps(SvgVisualizationOverlay.component)

object SvgVisualizationOverlay {
  private type Props = SvgVisualizationOverlay

  // Axis-aligned bounds of a set of vertices, in microarcseconds.
  private case class Envelope(minX: Double, minY: Double, maxX: Double, maxY: Double):
    def width: Double                   = maxX - minX
    def height: Double                  = maxY - minY
    def union(that: Envelope): Envelope =
      Envelope(minX.min(that.minX), minY.min(that.minY), maxX.max(that.maxX), maxY.max(that.maxY))

  private object Envelope:
    val Empty: Envelope = Envelope(0, 0, 0, 0)

    def of(polygons: List[ShapePolygon]): Option[Envelope] =
      polygons
        .flatMap(_.exterior.toList)
        .map(xy)
        .map((x, y) => Envelope(x, y, x, y))
        .reduceOption(_.union(_))

  // Offset p is flipped so it increases to the left, as the geometry engines do.
  private def xy(o: Offset): (Double, Double) =
    (-Angle.signedMicroarcseconds.get(o.p.toAngle).toDouble,
     Angle.signedMicroarcseconds.get(o.q.toAngle).toDouble
    )

  private def point(o: Offset): String =
    val (x, y) = xy(o)
    s"${scale(x)},${scale(y)}"

  private def forPolygon(css: Css, p: ShapePolygon): VdomNode =
    if (p.holes.isEmpty)
      <.polygon(css |+| VisualizationStyles.JtsPolygon,
                ^.points := p.exterior.toList.map(point).mkString(" ")
      )
    else
      // A polygon with holes cannot be a single <polygon>: its coordinates would run the hole
      // rings onto the shell, drawing spurious connecting lines. 
      // Each ring becomes a subpath instead.
      def subpath(ring: NonEmptyList[Offset]): String =
        ring.toList.map(point).mkString("M", " L", " Z")

      <.path(
        css |+| VisualizationStyles.JtsPolygon,
        ^.d        := (p.exterior :: p.holes).map(subpath).mkString(" "),
        ^.fillRule := "evenodd"
      )

  private def forPolygons(css: Css, polygons: List[ShapePolygon]): VdomNode =
    polygons match
      case Nil      => EmptyVdom
      case p :: Nil => forPolygon(css, p)
      case ps       =>
        <.g(css |+| VisualizationStyles.JtsCollection, ps.map(forPolygon(css, _)).toTagMod)

  // Screen-space sizes for shape labels, converted to user units at render time.
  private val labelFontSizePx = 11.0
  private val labelPaddingPx  = 5.0

  private val hatchLine    = Css("hatch-line")
  private val hatchLineSel = Css("hatch-line-selected")

  private val component =
    ScalaFnComponent[Props] { p =>
      import p.interpreter

      val evald: NonEmptyList[(Css, List[ShapePolygon])] =
        p.shapes.map((css, shape) => (css, shape.eval.polygons))

      // The viewBox covers the whole geometry, in microarcseconds.
      val envelope =
        evald.toList.flatMap((_, polygons) => Envelope.of(polygons)).reduceOption(_.union(_))

      val (x, y, w, h) =
        envelope.fold((0.0, 0.0, 0.0, 0.0))(e => (e.minX, e.minY, e.width, e.height))

      val (viewBoxX, viewBoxY, viewBoxW, viewBoxH) =
        calculateViewBox(x, y, w, h, p.fov, p.screenOffset)

      // The viewBox is in scaled microarcseconds, so a label's font and padding have to be
      // converted from pixels or they'd resize with the zoom level.
      val userUnitsPerPixel: Double = viewBoxW / p.width

      // Drawn outside the y-flipped group, or the glyphs would come out mirrored. A label whose
      // shape isn't in this overlay, or whose shape is empty, is simply skipped.
      val labels: List[VdomNode] =
        p.labels.flatMap: (css, text) =>
          evald
            .find(_._1 === css)
            .flatMap((_, polygons) => Envelope.of(polygons))
            .map: env =>
              <.g(
                css |+| VisualizationStyles.VizShapeLabel,
                <.text(
                  ^.x         := (scale(env.minX) + scale(env.maxX)) / 2,
                  // maxY is the top of the shape once the y flip is undone
                  ^.y         := -scale(env.maxY) - labelPaddingPx * userUnitsPerPixel,
                  textAnchor  := "middle",
                  svgFontSize := labelFontSizePx * userUnitsPerPixel,
                  text
                )
              )

      <.svg(
        VisualizationStyles.VisualizationSvg |+| p.clazz,
        ^.viewBox    := s"$viewBoxX $viewBoxY $viewBoxW $viewBoxH",
        canvasWidth  := s"${p.width}px",
        canvasHeight := s"${p.height}px",
        // defs are harmless if unused for non-ghost
        hatchDefs(hatchLine, hatchLineSel),
        <.g(
          ^.transform := s"scale(1, -1)",
          evald.toList.map((css, polygons) => forPolygons(css, polygons)).toTagMod
        ),
        labels.toTagMod
      )
    }
}

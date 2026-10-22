// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.findercharts

import crystal.react.View
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.Transformation
import explore.model.inputCodecs
import japgolly.scalajs.react.*
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.Angle
import lucuma.react.common.ReactFnProps
import lucuma.react.fa.Transform
import lucuma.react.primereact.Divider
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given

case class ControlOverlay(
  parallacticAngle: Option[Angle],
  ops:              View[Transformation]
) extends ReactFnProps[ControlOverlay](ControlOverlay.component)

object ControlOverlay {
  type Props = ControlOverlay

  private val component =
    ScalaFnComponent[Props]: p =>
      val rotateDeg = p.ops.zoom(Transformation.rotateDeg)
      val scaleY    = p.ops.zoom(Transformation.scaleYVal)
      val scaleX    =
        p.ops.zoom(Transformation.scaleXVal).withOnMod(x => scaleY.mod(y => y.signum * x))
      val inverted  = p.ops.zoom(Transformation.inverted)

      ReactFragment(
        <.div(
          ExploreStyles.FinderChartsTools,
          <.span(Icons.Wrench, " Viewer Controls"),
          Divider(),
          <.div(ExploreStyles.FinderChartsButton,
                ^.onClick --> p.ops.mod(_.zoomOut),
                Icons.MagnifyingGlassMinus.withBorder(true)
          ),
          <.div("Zoom"),
          <.div(ExploreStyles.FinderChartsButton,
                ^.onClick --> p.ops.mod(_.zoomIn),
                Icons.MagnifyingGlassPlus.withBorder(true)
          ),
          FormInputTextView("zoom".refined, value = scaleX, codec = inputCodecs.FinderChartZoom),
          <.div(ExploreStyles.FinderChartsButton,
                ^.onClick --> p.ops.mod(_.rotateLeft),
                Icons.ArrowRotateLeft.withBorder(true)
          ),
          <.div("Rotate"),
          <.div(ExploreStyles.FinderChartsButton,
                ^.onClick --> p.ops.mod(_.rotateRight),
                Icons.ArrowRotateRight.withBorder(true)
          ),
          FormInputTextView("rotate".refined, value = rotateDeg, codec = inputCodecs.Integer),
          <.div(ExploreStyles.FinderChartsButton,
                ^.onClick --> p.ops.mod(_.vflip),
                Icons.ArrowsFromLine.withBorder(true)
          ),
          <.div("Flip"),
          <.div(
            ExploreStyles.FinderChartsButton,
            ^.onClick --> p.ops.mod(_.flip),
            Icons.ArrowsFromLine
              .withBorder(true)
              .withTransform(Transform(rotate = 90))
          ),
          p.parallacticAngle
            .map(pa =>
              <.div(
                ExploreStyles.FinderChartsButton,
                ^.onClick --> p.ops
                  .mod(_.rotateTo(pa)),
                <.div(ExploreStyles.FinderChartsButtonPA,
                      "Align to PA",
                      Icons.Angle.withBorder(true)
                )
              )
            )
            .getOrElse(<.div()), // keep the table working OK
          <.div(
            ExploreStyles.FinderChartsButton,
            Icons.ArrowsRepeatLight.withBorder(true),
            ^.onClick --> p.ops.mod(_.reset)
          ),
          <.div("Reset"),
          <.div(
            ExploreStyles.FinderChartsButton,
            Icons.CircleHalfStroke
              .withBorder(true)
              .withTransform(Transform(rotate = inverted.get.fold(0, 180))),
            ^.onClick --> inverted.mod(_.flip)
          ),
          <.div("Invert")
        )
      )
}

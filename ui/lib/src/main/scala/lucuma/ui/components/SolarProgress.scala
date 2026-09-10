// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.components

import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps

case class SolarProgress(css: Css = Css.Empty, message: Option[VdomNode] = None)
    extends ReactFnProps(SolarProgress.component)

object SolarProgress {
  private type Props = SolarProgress

  private val component = ScalaFnComponent[Props] { p =>
    // The wrapper is `display: contents`, so it adds no box of its own. The message is a
    // sibling of the orbits rather than a child because `.solar-system` is scaled down,
    // and anything inside it would be scaled with it.
    <.div(
      ^.cls := "solar-progress",
      <.div(
        ^.cls := "solar-system",
        p.css,
        <.div(
          ^.cls := "mars-orbit orbit",
          <.div(^.cls    := "planet mars"),
          <.div(
            ^.cls := "earth-orbit orbit",
            <.div(^.cls := "planet earth"),
            <.div(^.cls := "venus-orbit orbit",
                  <.div(^.cls := "planet venus"),
                  <.div(^.cls := "mercury-orbit orbit",
                        <.div(^.cls := "planet mercury"),
                        <.div(^.cls := "sun")
                  )
            )
          )
        )
      ),
      p.message.map(m => <.div(^.cls := "solar-progress-message", m))
    )
  }
}

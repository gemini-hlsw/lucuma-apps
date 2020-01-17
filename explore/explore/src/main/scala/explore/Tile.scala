// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.semanticui.collections.menu._
import react.semanticui.sizes._
import react.common._

object Tile {
  final case class Props(title: String)

  implicit val reuseProps: Reusability[Props] = Reusability.derive[Props]
  private val component =
    ScalaComponent
      .builder[Props]("Tile")
      .render_PC { (p, c) =>
        <.div(
          Menu(
            size       = Mini,
            attached   = MenuAttached.Top,
            compact    = true,
            borderless = true,
            tabular    = MenuTabular.Right
          )(
            MenuItem(as = "a")(Icons.BarsIcon, p.title)
          ),
          ^.cls := "tileTitle",
          c
        )
      }
      .configure(Reusability.shouldComponentUpdate)
      .build

  def apply(p: Props, c: VdomNode*) = component(p)(c: _*)
}

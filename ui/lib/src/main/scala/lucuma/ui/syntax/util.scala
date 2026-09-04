// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.syntax

import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.Reusable
import lucuma.react.common.EnumValue
import org.scalajs.dom

import scala.scalajs.js

trait util:
  extension [A](a: A | Unit)(using ev: EnumValue[A])
    def undefToJs: js.UndefOr[String] = a.map(ev.value)

  extension [A](reusableList: Reusable[List[A]])
    def sequenceList: List[Reusable[A]] =
      reusableList.value.map(x => reusableList.map(_ => x))

  extension (element: dom.Element)
    /**
     * Scroll the element into view if it is not fully visible within its scroll container,
     * scrolling as little as possible.
     *
     * `js.Dynamic` is needed since scalajs-dom only exposes `scrollIntoView(top: Boolean)`.
     */
    def scrollIfNeeded: Callback = Callback.lift(() =>
      val _ = element
        .asInstanceOf[js.Dynamic]
        .scrollIntoView(js.Dynamic.literal(block = "nearest", inline = "nearest"))
    )

object util extends util

// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.hooks

import crystal.react.hooks.*
import japgolly.scalajs.react.*
import org.scalajs.dom

import scala.scalajs.js

object UseViewportHeight:
  /**
   * The window's inner height in pixels, kept up to date on window resize.
   */
  def useViewportHeight: HookResult[Int] =
    for
      height <- useStateView(dom.window.innerHeight.toInt)
      _      <- useEffectOnMount:
                  val listener: js.Function1[dom.Event, Unit] =
                    _ => height.set(dom.window.innerHeight.toInt).runNow()
                  // Return the callback effect for unmount
                  Callback(dom.window.addEventListener("resize", listener))
                    .map(_ => Callback(dom.window.removeEventListener("resize", listener)))
    yield height.get

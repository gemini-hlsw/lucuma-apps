// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.AppendTo
import lucuma.react.primereact.Toast
import lucuma.react.primereact.ToastRef
import org.scalajs.dom
import org.scalajs.dom.document.body

case class ToastPortal(toastRef: ToastRef) extends ReactFnProps(ToastPortal.component)

// Use the mutation observer to put the toast always last
// This requires direct dom manipulation
object ToastPortal:
  private type Props = ToastPortal

  private def movePortalLast(portal: dom.html.Element): Unit =
    // if not the last put it last, even if portal was in the dom will be moved last
    if (body.lastElementChild != portal)
      body.appendChild(portal): Unit

  private def setup(portal: dom.html.Element, observer: dom.MutationObserver): Callback = Callback {
    observer.observe(body, new dom.MutationObserverInit { childList = true })
    movePortalLast(portal)
  }

  private def teardown(observer: dom.MutationObserver): CallbackTo[Unit] =
    CallbackTo(Callback(observer.disconnect())).void

  private val component = ScalaFnComponent[Props]: props =>
    // Only one node is ever needed
    val elem: dom.html.Element =
      dom.document.createElement("div") match
        case e: dom.html.Element =>
          e.className = "toast-portal"
          body.appendChild(e)
          e

    // MutationObserver to listen for don mchanges
    val observer: dom.MutationObserver =
      new dom.MutationObserver((_, _) => movePortalLast(elem))

    useEffectOnMount(setup(elem, observer) *> teardown(observer)).map: _ =>
      <.div(
        Toast(
          position = Toast.Position.BottomRight,
          baseZIndex = 2000,
          appendTo = AppendTo.Element(elem)
        ).withRef(props.toastRef.ref)
      )

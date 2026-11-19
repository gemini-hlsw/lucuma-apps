// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.hooks

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.*
import japgolly.scalajs.react.hooks.*
import lucuma.ui.input.InputmaskOptions
import lucuma.ui.input.facade.Inputmask
import org.scalajs.dom
import org.scalajs.dom.html

import scala.scalajs.js

/**
 * Keeps an Inputmask attached to the input with the given id while the component is mounted, when a
 * mask is given. The input is located by id, like `FormInputTextView` does, so the id must be
 * unique in the document.
 *
 * The mask is attached when the input first gets focus, not on mount. Attaching a mask to an input
 * that holds text makes Inputmask read the input's computed style, which forces the browser to
 * recalculate the styles of the whole page. With many inputs mounted at once (e.g. hundreds of
 * explicit offsets), mounting them took tens of seconds. The mask only matters while typing.
 *
 * Attaching a mask re-masks the text the input holds. For a codec whose printed text is valid for
 * its mask (see `InputCodecLaws.canonicalIsValidForMask`) the text is unchanged; other text is
 * rewritten to what the mask accepts. Attaching also moves the caret, so the selection the input
 * had when it got focus (where it was clicked, or all of it when tabbed into) is put back.
 */
def useInputmask(inputId: NonEmptyString, options: Option[InputmaskOptions]): HookResult[Unit] =
  useEffectWithDeps(options)(UseInputmask.attachOnFocus(inputId, _))

private object UseInputmask:
  /**
   * Attaches the mask, if any, when the input gets focus (or right away if it already has it), and
   * returns the callback that removes the mask and the pending listener.
   */
  def attachOnFocus(
    inputId: NonEmptyString,
    options: Option[InputmaskOptions]
  ): CallbackTo[Callback] =
    options.fold(CallbackTo(Callback.empty)): opts =>
      CallbackTo:
        dom.document.getElementById(inputId.value) match
          case input: html.Input =>
            val attach: js.Function1[dom.Event, Unit] = _ =>
              val selection: Option[(Int, Int)] =
                (Option(input.selectionStart), Option(input.selectionEnd)).tupled
              new Inputmask(opts.toJs).mask(input)
              selection.foreach(input.setSelectionRange(_, _))

            if dom.document.activeElement == input then attach(new dom.Event("focus"))
            else
              input.addEventListener("focus", attach, new dom.EventListenerOptions { once = true })

            Callback:
              input.removeEventListener("focus", attach)
              Inputmask.remove(input)
          case _                 => Callback.empty

// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.hooks

import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.*
import japgolly.scalajs.react.hooks.*
import lucuma.ui.input.facade.Inputmask
import lucuma.ui.input.facade.InputmaskOptions
import org.scalajs.dom.document
import org.scalajs.dom.html

/**
 * Keeps an Inputmask attached to the input with the given id while the component is mounted, when
 * a mask is given. The input is located by id, like `FormInputTextView` does, so the id must be
 * unique in the document.
 */
def useInputmask(inputId: NonEmptyString, options: Option[InputmaskOptions]): HookResult[Unit] =
  useEffectOnMount(UseInputmask.attach(inputId, options))

private object UseInputmask:
  private def findInput(id: NonEmptyString): CallbackTo[Option[html.Input]] =
    CallbackTo(Option(document.getElementById(id.value)).map(_.asInstanceOf[html.Input]))

  /** Attaches the mask, if any, and returns the callback that removes it. */
  def attach(inputId: NonEmptyString, options: Option[InputmaskOptions]): CallbackTo[Callback] =
    options.fold(CallbackTo(Callback.empty)): opts =>
      findInput(inputId).map: input =>
        input.foreach(el => new Inputmask(opts).mask(el))
        Callback(input.foreach(Inputmask.remove))

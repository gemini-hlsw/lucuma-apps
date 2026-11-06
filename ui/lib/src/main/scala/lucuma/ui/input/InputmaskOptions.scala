// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.input

import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.Reusable
import lucuma.ui.input.facade.InputmaskOptionsJs

import scala.annotation.nowarn
import scala.scalajs.js

/** Inputmask's `casing` option: the case the typed text is forced to. */
enum Casing(val toJs: String):
  case Upper extends Casing("upper")
  case Lower extends Casing("lower")
  case Title extends Casing("title")

object Casing:
  given Reusability[Casing] = Reusability.by_==

/** The HTML `inputmode` attribute: the virtual keyboard a mobile browser offers. */
enum InputMode(val toJs: String):
  case NoKeyboard extends InputMode("none")
  case Text       extends InputMode("text")
  case Decimal    extends InputMode("decimal")
  case Numeric    extends InputMode("numeric")
  case Tel        extends InputMode("tel")
  case Search     extends InputMode("search")
  case Email      extends InputMode("email")
  case Url        extends InputMode("url")

object InputMode:
  given Reusability[InputMode] = Reusability.by_==

final case class InputmaskOptions(
  mask:                 js.UndefOr[String] = js.undefined,
  regex:                js.UndefOr[String] = js.undefined,
  placeholder:          js.UndefOr[String] = js.undefined,
  jitMasking:           js.UndefOr[Boolean] = js.undefined,
  clearIncomplete:      js.UndefOr[Boolean] = js.undefined,
  clearMaskOnLostFocus: js.UndefOr[Boolean] = js.undefined,
  showMaskOnFocus:      js.UndefOr[Boolean] = js.undefined,
  showMaskOnHover:      js.UndefOr[Boolean] = js.undefined,
  shiftPositions:       js.UndefOr[Boolean] = js.undefined,
  insertMode:           js.UndefOr[Boolean] = js.undefined,
  undoOnEscape:         js.UndefOr[Boolean] = js.undefined,
  casing:               js.UndefOr[Casing] = js.undefined,
  nullable:             js.UndefOr[Boolean] = js.undefined,
  inputMode:            js.UndefOr[InputMode] = js.undefined,
  onComplete:           js.UndefOr[Reusable[Callback]] = js.undefined,
  onIncomplete:         js.UndefOr[Reusable[Callback]] = js.undefined,
  onCleared:            js.UndefOr[Reusable[Callback]] = js.undefined
):
  def toJs: InputmaskOptionsJs =
    val js: InputmaskOptionsJs = new InputmaskOptionsJs {}
    mask.foreach(js.mask = _)
    regex.foreach(js.regex = _)
    placeholder.foreach(js.placeholder = _)
    jitMasking.foreach(js.jitMasking = _)
    clearIncomplete.foreach(js.clearIncomplete = _)
    clearMaskOnLostFocus.foreach(js.clearMaskOnLostFocus = _)
    showMaskOnFocus.foreach(js.showMaskOnFocus = _)
    showMaskOnHover.foreach(js.showMaskOnHover = _)
    shiftPositions.foreach(js.shiftPositions = _)
    insertMode.foreach(js.insertMode = _)
    undoOnEscape.foreach(js.undoOnEscape = _)
    casing.foreach(c => js.casing = c.toJs)
    nullable.foreach(js.nullable = _)
    inputMode.foreach(m => js.inputmode = m.toJs)
    onComplete.foreach(cb => js.oncomplete = cb.value.toJsFn)
    onIncomplete.foreach(cb => js.onincomplete = cb.value.toJsFn)
    onCleared.foreach(cb => js.oncleared = cb.value.toJsFn)
    js

object InputmaskOptions:
  // The derive macro always emits a `suspend` helper for recursive types, unused on a flat case class.
  @nowarn("msg=unused local definition")
  given Reusability[InputmaskOptions] = Reusability.derive

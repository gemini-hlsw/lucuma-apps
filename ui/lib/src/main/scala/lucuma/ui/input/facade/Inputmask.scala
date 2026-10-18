// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.input.facade

import org.scalajs.dom.html

import scala.annotation.unused
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

/**
 * Facade for Inputmask (https://github.com/RobinHerbots/Inputmask). Only the options we use are
 * declared; see https://robinherbots.github.io/Inputmask for the rest.
 */
@js.native
@JSImport("inputmask", JSImport.Default)
class Inputmask(@unused options: InputmaskOptions) extends js.Object:
  /** Attaches the mask to the element and returns this instance. */
  def mask(element: html.Input): Inputmask = js.native

  /** Attaches the mask to the element with the given id and returns this instance. */
  def mask(elementId: String): Inputmask = js.native

  def remove(): Unit = js.native

  def isComplete(): Boolean = js.native

  def isValid(value: String): Boolean = js.native

  def unmaskedvalue(): String = js.native

  def setValue(value: String): Unit = js.native

@js.native
@JSImport("inputmask", JSImport.Default)
object Inputmask extends js.Object:
  def remove(element: html.Input): Unit = js.native

  /** Removes the mask from the element with the given id. */
  def remove(elementId: String): Unit = js.native

  def isValid(value: String, options: InputmaskOptions): Boolean = js.native

  def format(value: String, options: InputmaskOptions): String = js.native

/**
 * Inputmask options; only the ones we use are declared.
 *
 * The `numeric` alias and its options (`digits`, `allowMinus`, `integerDigits`, ...) are left out
 * on purpose: numbers are masked with regexes instead (see `Masks.integerOptions`). The alias
 * toggles the sign when a minus is typed over a selection instead of replacing it, and ignores
 * `integerDigits`. Its `k`/`m` multiplier shortcuts are also hard to switch off: Inputmask merges
 * the alias options with ours recursively, so `shortcuts = {}` keeps the alias's `k` and `m`
 * entries; only `null` removes them.
 */
trait InputmaskOptions extends js.Object:
  var mask: js.UndefOr[String]                     = js.undefined
  var regex: js.UndefOr[String]                    = js.undefined
  var placeholder: js.UndefOr[String]              = js.undefined
  var jitMasking: js.UndefOr[Boolean]              = js.undefined
  var clearIncomplete: js.UndefOr[Boolean]         = js.undefined
  var clearMaskOnLostFocus: js.UndefOr[Boolean]    = js.undefined
  var showMaskOnFocus: js.UndefOr[Boolean]         = js.undefined
  var showMaskOnHover: js.UndefOr[Boolean]         = js.undefined
  var shiftPositions: js.UndefOr[Boolean]          = js.undefined
  var insertMode: js.UndefOr[Boolean]              = js.undefined
  var undoOnEscape: js.UndefOr[Boolean]            = js.undefined
  var casing: js.UndefOr[String]                   = js.undefined
  var nullable: js.UndefOr[Boolean]                = js.undefined
  var inputmode: js.UndefOr[String]                = js.undefined
  var oncomplete: js.UndefOr[js.Function0[Unit]]   = js.undefined
  var onincomplete: js.UndefOr[js.Function0[Unit]] = js.undefined
  var oncleared: js.UndefOr[js.Function0[Unit]]    = js.undefined

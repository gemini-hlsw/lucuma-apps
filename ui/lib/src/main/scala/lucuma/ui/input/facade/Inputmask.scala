// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.input.facade

import org.scalajs.dom.html

import scala.annotation.unused
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

/**
 * Minimal facade for Inputmask (https://github.com/RobinHerbots/Inputmask), enough to evaluate it.
 * Only the options we exercise are declared; see the library documentation for the rest.
 */
@js.native
@JSImport("inputmask", JSImport.Default)
class Inputmask(@unused options: InputmaskOptions) extends js.Object:
  /** Attaches the mask to the element and returns this instance. */
  def mask(element: html.Input): Inputmask = js.native

  def remove(): Unit = js.native

  def isComplete(): Boolean = js.native

  def unmaskedvalue(): String = js.native

  def setValue(value: String): Unit = js.native

@js.native
@JSImport("inputmask", JSImport.Default)
object Inputmask extends js.Object:
  def remove(element: html.Input): Unit = js.native

  def isValid(value: String, options: InputmaskOptions): Boolean = js.native

  def format(value: String, options: InputmaskOptions): String = js.native

trait InputmaskOptions extends js.Object:
  var mask: js.UndefOr[String]                     = js.undefined
  var regex: js.UndefOr[String]                    = js.undefined
  var alias: js.UndefOr[String]                    = js.undefined
  var inputFormat: js.UndefOr[String]              = js.undefined
  var placeholder: js.UndefOr[String]              = js.undefined
  var jitMasking: js.UndefOr[Boolean]              = js.undefined
  var clearIncomplete: js.UndefOr[Boolean]         = js.undefined
  var clearMaskOnLostFocus: js.UndefOr[Boolean]    = js.undefined
  var showMaskOnFocus: js.UndefOr[Boolean]         = js.undefined
  var showMaskOnHover: js.UndefOr[Boolean]         = js.undefined
  var shiftPositions: js.UndefOr[Boolean]          = js.undefined
  var insertMode: js.UndefOr[Boolean]              = js.undefined
  var autoUnmask: js.UndefOr[Boolean]              = js.undefined
  var greedy: js.UndefOr[Boolean]                  = js.undefined
  var oncomplete: js.UndefOr[js.Function0[Unit]]   = js.undefined
  var onincomplete: js.UndefOr[js.Function0[Unit]] = js.undefined
  var oncleared: js.UndefOr[js.Function0[Unit]]    = js.undefined

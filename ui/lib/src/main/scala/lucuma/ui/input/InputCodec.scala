// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.input

import cats.syntax.all.*
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.math.validation.MathValidators
import lucuma.core.validation.InputValidFormat
import lucuma.ui.input.facade.InputmaskOptions

/**
 * Everything a text input needs for a value of type `A`: the codec between the text and `A`, used on
 * blur to parse and to print the canonical text, and the Inputmask options constraining the text
 * while typing, if any. `valid` is an `InputValidFormat`, lucuma-core's name for a validating
 * parse/print pair.
 */
final case class InputCodec[A](
  valid: InputValidFormat[A],
  mask:  Option[InputmaskOptions]
)

object InputCodec:
  /** A format with a regex mask, see [[Masks]]. */
  def regex[A](valid: InputValidFormat[A], pattern: String): InputCodec[A] =
    InputCodec(valid, Masks.regexOptions(pattern).some)

  val TruncatedRa: InputCodec[RightAscension] = regex(MathValidators.truncatedRA, Masks.Ra)

  val TruncatedDec: InputCodec[Declination] = regex(MathValidators.truncatedDec, Masks.Dec)

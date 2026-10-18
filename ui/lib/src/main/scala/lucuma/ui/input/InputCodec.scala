// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.input

import cats.syntax.all.*
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.math.validation.MathValidators
import lucuma.core.validation.InputValidFormat
import lucuma.core.validation.InputValidSplitEpi
import lucuma.ui.input.facade.InputmaskOptions

/**
 * Everything a text input needs for a value of type `A`: the codec between the text and `A`, used
 * on blur to parse and to print the canonical text, and the Inputmask options constraining the text
 * while typing, if any. `valid` is an `InputValidFormat`, lucuma-core's name for a validating
 * parse/print pair.
 *
 * Build one from the parser with `toCodec`, then pick the typing constraint: `withIntegerMask`,
 * `withDecimalMask`, `withUnboundedDecimalMask`, `withScientificMask`, or `withRegexMask` with a
 * pattern built from [[Masks]] fragments; `withMask` takes raw Inputmask options for anything else.
 */
final case class InputCodec[A](
  valid: InputValidFormat[A],
  mask:  Option[InputmaskOptions]
):
  def withMask(options: InputmaskOptions): InputCodec[A] = copy(mask = options.some)

  /** Constrains the text to a regex, see [[Masks]] for fragments and our standard options. */
  def withRegexMask(pattern: String): InputCodec[A] = withMask(Masks.regexOptions(pattern))

  /** Integer digits with an optional minus sign. */
  def withIntegerMask(
    allowMinus:    Boolean = true,
    integerDigits: Option[Int] = none
  ): InputCodec[A] =
    withMask(Masks.integerOptions(allowMinus, integerDigits))

  /** A decimal number with up to `decimals` decimals. */
  def withDecimalMask(
    decimals:      Int,
    allowMinus:    Boolean = true,
    integerDigits: Option[Int] = none
  ): InputCodec[A] =
    withMask(Masks.decimalOptions(decimals.some, allowMinus, integerDigits))

  /** A decimal number with any number of decimals. */
  def withUnboundedDecimalMask(allowMinus: Boolean = true): InputCodec[A] =
    withMask(Masks.decimalOptions(none, allowMinus, none))

  /** A decimal with an optional exponent, like `1.234e-05`. */
  def withScientificMask(
    integerDigits:  Int = 1,
    decimals:       Int = 3,
    exponentDigits: Int = 2,
    allowMinus:     Boolean = true
  ): InputCodec[A] =
    withRegexMask(Masks.scientific(integerDigits, decimals, exponentDigits, allowMinus))

object InputCodec:
  def apply[A](valid: InputValidFormat[A]): InputCodec[A] = InputCodec(valid, none)

  /** Plain text, no parsing and no mask. */
  val Text: InputCodec[String] = InputCodec(InputValidSplitEpi.id)

  val TruncatedRa: InputCodec[RightAscension] =
    MathValidators.truncatedRA.toCodec.withRegexMask(Masks.Ra)

  val TruncatedDec: InputCodec[Declination] =
    MathValidators.truncatedDec.toCodec.withRegexMask(Masks.Dec)

extension [A](valid: InputValidFormat[A]) def toCodec: InputCodec[A] = InputCodec(valid)

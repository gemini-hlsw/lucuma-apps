// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.input

import lucuma.ui.input.facade.InputmaskOptions

/**
 * Regex fragments for Inputmask masks, and the options we attach them with.
 *
 * Fragments compose by string interpolation. Any fragment containing an alternation (`|`) must be
 * wrapped in its own parentheses, otherwise the `|` splits the whole mask it is interpolated into.
 * The helpers below always emit the parentheses; keep them when writing a fragment by hand.
 */
object Masks:

  /**
   * Regex matching the integers in `[min, max]` written with exactly `digits` digits, zero padded.
   * `range(0, 23, 2)` is `([0-1][0-9]|2[0-3])`.
   */
  def range(min: Int, max: Int, digits: Int): String =
    require(0 <= min && min <= max, s"Invalid range [$min, $max]")
    require(max < pow10(digits), s"$max does not fit in $digits digits")
    rangeRegexAlternatives(pad(min, digits), pad(max, digits)).mkString("(", "|", ")")

  /**
   * Regex for a segment holding `0..max`, typed with 1 to `digits` digits, so that leading zeros
   * are optional: `segment(23, 2)` accepts "3" as well as "03". The parser pads on blur.
   */
  def segment(max: Int, digits: Int): String =
    (1 to digits)
      .flatMap(w => rangeRegexAlternatives(pad(0, w), pad(max.min(pow10(w) - 1), w)))
      .mkString("(", "|", ")")

  /** Optional decimal part with 1 to `digits` decimals. */
  def decimals(digits: Int): String = s"(\\.[0-9]{1,$digits})?"

  val Hours: String   = segment(23, 2)
  val Minutes: String = segment(59, 2)
  val Degrees: String = segment(90, 2)

  def seconds(decimalDigits: Int): String = segment(59, 2) + decimals(decimalDigits)

  val Hm: String  = s"$Hours:$Minutes"
  val Hms: String = s"$Hours:$Minutes:${seconds(3)}"
  val Ra: String  = Hms
  val Dec: String = s"[+-]?$Degrees:$Minutes:${seconds(2)}"

  /**
   * Our standard options for a regex mask: nothing displayed until typed, separators inserted just
   * in time, incomplete text left in place for the parser to flag on blur.
   */
  def regexOptions(pattern: String): InputmaskOptions =
    new InputmaskOptions:
      regex = pattern
      placeholder = ""
      jitMasking = true
      showMaskOnHover = false
      clearIncomplete = false

  private def pow10(n: Int): Int = List.fill(n)(10).product

  private def pad(n: Int, digits: Int): String = n.toString.reverse.padTo(digits, '0').reverse

  private def digitRun(n: Int): String =
    n match
      case 0 => ""
      case 1 => "[0-9]"
      case k => s"[0-9]{$k}"

  private def charClass(from: Char, to: Char): String =
    if from == to then from.toString else s"[$from-$to]"

  /**
   * The regex alternatives covering the zero-padded integers from `lo` to `hi`, which have equal
   * length; the caller joins them with `|`. `rangeRegexAlternatives("007", "123")` is
   * `List("00[7-9]", "0[1-9][0-9]", "1[0-1][0-9]", "12[0-3]")`.
   */
  private def rangeRegexAlternatives(lo: String, hi: String): List[String] =
    if lo.isEmpty then List("")
    else if lo.head == hi.head then
      rangeRegexAlternatives(lo.tail, hi.tail).map(lo.head.toString + _)
    else
      val n: Int            = lo.length - 1
      val loIsRound: Boolean = lo.tail.forall(_ == '0')
      val hiIsRound: Boolean = hi.tail.forall(_ == '9')
      // Leading digits whose whole suffix range is covered collapse into one character class.
      val midFrom: Char     = if loIsRound then lo.head else (lo.head + 1).toChar
      val midTo: Char       = if hiIsRound then hi.head else (hi.head - 1).toChar
      val low: List[String] =
        if loIsRound then Nil
        else rangeRegexAlternatives(lo.tail, "9" * n).map(lo.head.toString + _)
      val mid: List[String] =
        if midFrom <= midTo then List(charClass(midFrom, midTo) + digitRun(n)) else Nil
      val high: List[String] =
        if hiIsRound then Nil
        else rangeRegexAlternatives("0" * n, hi.tail).map(hi.head.toString + _)
      low ++ mid ++ high

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
 *
 * Inputmask has its own regex engine, and its alternation is weaker than a real regex engine's:
 * literal digits must be written as one-character classes (`[9][0]`, not `90`), shorter
 * alternatives must use a quantifier (`[0-9]{1,2}`, not `[0-9]|[0-9][0-9]`), an alternation must
 * not follow a quantifier, and an alternation must not precede an optional group, otherwise valid
 * text is rejected or cannot be edited. The helpers follow these rules; `MasksSuite` checks them
 * against Inputmask itself.
 */
object Masks:

  /**
   * Regex matching the integers in `[min, max]` written with exactly `digits` digits, zero padded.
   * `range(0, 23, 2)` is `([0-1][0-9]|[2][0-3])`.
   */
  def range(min: Int, max: Int, digits: Int): String =
    require(0 <= min && min <= max, s"Invalid range [$min, $max]")
    require(max < pow10(digits), s"$max does not fit in $digits digits")
    rangeRegexAlternatives(pad(min, digits), pad(max, digits)).mkString("(", "|", ")")

  /**
   * Regex for a segment holding `0..max`, typed with 1 to `digits` digits, so that leading zeros
   * are optional: `segment(23, 2)` is `([0-9]|[0-1][0-9]|[2][0-3])` and accepts "3" as well as
   * "03". The parser pads on blur.
   */
  def segment(max: Int, digits: Int): String =
    require(digits >= 2, "A segment needs at least two digits")
    val shorter: String    = if digits == 2 then "[0-9]" else s"[0-9]{1,${digits - 1}}"
    val full: List[String] = rangeRegexAlternatives(pad(0, digits), pad(max, digits))
    (shorter :: full).mkString("(", "|", ")")

  /** Optional decimal part with 1 to `digits` decimals. */
  def decimals(digits: Int): String = s"(\\.[0-9]{1,$digits})?"

  val Hours: String   = segment(23, 2)
  val Minutes: String = segment(59, 2)
  val Degrees: String = segment(90, 2)

  /**
   * Durations have unbounded hours, unlike times of day. Their minutes and seconds are two loose
   * digits: Inputmask's engine rejects an alternation placed right after a quantifier, so the 0-59
   * bound is left to the parser.
   */
  val DurationHm: String  = "[0-9]{1,5}:[0-9][0-9]?"
  val DurationHms: String = s"$DurationHm:[0-9][0-9]?${decimals(3)}"

  /**
   * Seconds are two loose digits, validated on blur by the parser: Inputmask cannot edit a range
   * alternation that sits right before an optional group (the decimals). Deleting a leading digit
   * then shifts every later character left through the decimals, and when the result is invalid the
   * edit is refused, so the hours of a coordinate could not be edited at all.
   */
  def seconds(decimalDigits: Int): String = "[0-9]{1,2}" + decimals(decimalDigits)

  val Hm: String  = s"$Hours:$Minutes"
  val Hms: String = s"$Hours:$Minutes:${seconds(3)}"

  /**
   * Coordinates keep their live range limits for hours, degrees and minutes (seconds are validated
   * on blur, see `seconds`). Shortcut story 7027 asked for less obstructive coordinate entry; the
   * mask answers it without dropping the limits: separators are inserted as you type, Backspace
   * works across them, pasting a full value works, and single-digit segments like "3:9:5" are
   * accepted and padded on blur.
   */
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
      // Inputmask's Escape would restore the pre-focus text without an input event, leaving React's
      // state on the edited text; the parent component owns the text, so Escape does nothing.
      undoOnEscape = false

  /** Digits used for "unbounded" numeric parts; Inputmask needs a finite quantifier. */
  private val MaxDigits: Int = 20

  private def sign(allowMinus: Boolean): String = if allowMinus then "-?" else ""

  /** An integer as a regex fragment. */
  def integerPattern(allowMinus: Boolean, integerDigits: Option[Int]): String =
    s"(${sign(allowMinus)}[0-9]{1,${integerDigits.getOrElse(MaxDigits)}})"

  /**
   * A decimal number as a regex fragment. The integer part may be empty so that ".5" can be typed;
   * the parser normalizes it on blur.
   */
  def decimalPattern(integerDigits: Int, decimalDigits: Int, allowMinus: Boolean): String =
    s"(${sign(allowMinus)}[0-9]{0,$integerDigits}${decimals(decimalDigits)})"

  /** A decimal with an optional exponent: `-?d.ddde-dd`. */
  def scientific(
    integerDigits:  Int,
    decimalDigits:  Int,
    exponentDigits: Int,
    allowMinus:     Boolean
  ): String =
    s"${decimalPattern(integerDigits, decimalDigits, allowMinus)}(e[+-]?[0-9]{1,$exponentDigits})?"

  /** One or more `element`s separated by commas, with an optional space after each comma. */
  def commaList(element: String): String = s"$element(,\\s?$element)*"

  /**
   * Numeric masks are regexes rather than Inputmask's `numeric` alias: the alias toggles the sign
   * when a minus is typed over a selection, ignores `integerDigits`, and keeps its `k`/`m`
   * multiplier shortcuts because options are deep-merged.
   */
  def integerOptions(allowMinus: Boolean, integerDigits: Option[Int]): InputmaskOptions =
    numeric(integerPattern(allowMinus, integerDigits))

  /** `decimals` is the maximum number of decimals, or `None` for no practical limit. */
  def decimalOptions(
    decimals:      Option[Int],
    allowMinus:    Boolean,
    integerDigits: Option[Int]
  ): InputmaskOptions =
    numeric(
      decimalPattern(integerDigits.getOrElse(MaxDigits), decimals.getOrElse(MaxDigits), allowMinus)
    )

  /** A regex mask that also asks mobile keyboards for the decimal layout. */
  private def numeric(pattern: String): InputmaskOptions =
    val options: InputmaskOptions = regexOptions(pattern)
    options.inputmode = "decimal"
    options

  /** Free text forced to upper case. */
  val UpperCaseOptions: InputmaskOptions =
    new InputmaskOptions:
      mask = "*{*}"
      casing = "upper"
      placeholder = ""
      showMaskOnHover = false
      undoOnEscape = false

  private def pow10(n: Int): Int = List.fill(n)(10).product

  private def pad(n: Int, digits: Int): String = n.toString.reverse.padTo(digits, '0').reverse

  private def digitRun(n: Int): String =
    n match
      case 0 => ""
      case 1 => "[0-9]"
      case k => s"[0-9]{$k}"

  /** A single digit is still a class, see the note on Inputmask's engine above. */
  private def charClass(from: Char, to: Char): String =
    if from == to then s"[$from]" else s"[$from-$to]"

  /**
   * The regex alternatives covering the zero-padded integers from `lo` to `hi`, which have equal
   * length; the caller joins them with `|`. `rangeRegexAlternatives("007", "123")` is
   * `List("00[7-9]", "0[1-9][0-9]", "1[0-1][0-9]", "12[0-3]")`.
   */
  private def rangeRegexAlternatives(lo: String, hi: String): List[String] =
    if lo.isEmpty then List("")
    else if lo.head == hi.head then
      rangeRegexAlternatives(lo.tail, hi.tail).map(s"[${lo.head}]" + _)
    else
      val n: Int             = lo.length - 1
      val loIsRound: Boolean = lo.tail.forall(_ == '0')
      val hiIsRound: Boolean = hi.tail.forall(_ == '9')
      // Leading digits whose whole suffix range is covered collapse into one character class.
      val midFrom: Char      = if loIsRound then lo.head else (lo.head + 1).toChar
      val midTo: Char        = if hiIsRound then hi.head else (hi.head - 1).toChar
      val low: List[String]  =
        if loIsRound then Nil
        else rangeRegexAlternatives(lo.tail, "9" * n).map(s"[${lo.head}]" + _)
      val mid: List[String]  =
        if midFrom <= midTo then List(charClass(midFrom, midTo) + digitRun(n)) else Nil
      val high: List[String] =
        if hiIsRound then Nil
        else rangeRegexAlternatives("0" * n, hi.tail).map(s"[${hi.head}]" + _)
      low ++ mid ++ high

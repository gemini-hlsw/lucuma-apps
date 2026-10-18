// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.primereact

import cats.*
import cats.data.NonEmptyChain
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.validation.*
import lucuma.react.common.*
import lucuma.react.primereact.PrimeStyles
import lucuma.react.primereact.TooltipOptions
import lucuma.ui.hooks.useInputmask
import lucuma.ui.input.InputCodec
import org.scalajs.dom.Element
import org.scalajs.dom.HTMLInputElement
import org.scalajs.dom.KeyCode

import scala.scalajs.js

import scalajs.js.JSConverters.*

/**
 * FormInput component that uses a crystal View to share the content of the field.
 *
 * The `codec` parses the text on blur and Enter, prints the canonical text of the value, and
 * constrains the text while typing through its Inputmask options, if any.
 */
final case class FormInputTextView[V[_], A](
  id:              NonEmptyString,
  label:           js.UndefOr[TagMod] = js.undefined,
  units:           js.UndefOr[String] = js.undefined,
  preAddons:       List[TagMod] = List.empty,
  postAddons:      List[TagMod] = List.empty,
  size:            js.UndefOr[PlSize] = js.undefined,
  groupClass:      js.UndefOr[Css] = js.undefined,
  inputClass:      js.UndefOr[Css] = js.undefined,
  labelClass:      js.UndefOr[Css] = js.undefined,
  disabled:        js.UndefOr[Boolean] = js.undefined,
  error:           js.UndefOr[NonEmptyString] = js.undefined,
  placeholder:     js.UndefOr[String] = js.undefined,
  tooltipOptions:  js.UndefOr[TooltipOptions] = js.undefined,
  value:           V[A],
  codec:           InputCodec[A],
  onTextChange:    String => Callback = _ => Callback.empty,
  onValidChange:   FormInputTextView.ChangeCallback[Boolean] = _ => Callback.empty,
  onFocus:         js.UndefOr[ReactFocusEventFromInput => Callback] = js.undefined,
  onBlur:          FormInputTextView.ChangeCallback[EitherErrors[A]] = (_: EitherErrors[A]) =>
    Callback.empty,
  validateOnPaste: Boolean = true,
  modifiers:       Seq[TagMod] = Seq.empty
)(using val eq: Eq[A], val vl: ViewLike[V])
    extends ReactFnProps(FormInputTextView.component):
  def stringValue: String                  = value.get.foldMap(codec.valid.reverseGet)
  def addModifiers(modifiers: Seq[TagMod]) = copy(modifiers = this.modifiers ++ modifiers)
  def withMods(mods:          TagMod*)     = addModifiers(mods)
  def apply(mods:             TagMod*)     = addModifiers(mods)
  def addPostAddons(addons: List[TagMod])  =
    copy(postAddons = this.postAddons ++ addons)
  def withPostAddons(addons:  TagMod*)     = addPostAddons(addons.toList)

object FormInputTextView:
  type AnyF[_] = Any

  protected type Props[V[_], A]    = FormInputTextView[V, A]
  protected type ChangeCallback[A] = A => Callback

  protected def buildComponent[V[_], A] =
    def validate(
      text:          String,
      valid:         InputValidFormat[A],
      onValidChange: FormInputTextView.ChangeCallback[Boolean],
      cb:            EitherErrors[A] => Callback = _ => Callback.empty
    ): Callback =
      val validated: EitherErrors[A] = valid.getValid(text)
      onValidChange(validated.isRight) >> cb(validated)

    ScalaFnComponent[Props[V, A]]: props =>
      import props.given

      for
        displayValue <- useState(props.stringValue)
        errors       <- useState(none[NonEmptyChain[NonEmptyString]])
        _            <- useEffectWithDeps(props.stringValue): newValue =>
                          val displayMatchesExternal: Boolean =
                            props.codec.valid.getValid(displayValue.value).toOption ===
                              props.codec.valid.getValid(newValue).toOption
                          Option
                            .unless(displayMatchesExternal)(displayValue.setState(newValue))
                            .orEmpty >>
                            errors.setState(none)
        _            <- useEffectOnMount(
                          validate(props.stringValue, props.codec.valid, props.onValidChange)
                        )
        _            <- useInputmask(props.id, props.codec.mask)
      yield
        val errorChain: Option[NonEmptyChain[NonEmptyString]] =
          (props.error.toOption, errors.value) match
            case (Some(a), Some(b)) => (a +: b).some
            case (None, Some(b))    => b.some
            case (Some(a), None)    => NonEmptyChain(a).some
            case (None, None)       => none

        val error: Option[String] = errorChain.map(_.mkString_(", "))

        def handleTextChange(text: String): Callback =
          // First update the internal state, then call the outside listener
          displayValue.setState(text) >>
            errors.setState(none) >>
            props.onTextChange(text) >>
            validate(text, props.codec.valid, props.onValidChange)

        val onTextChange: ReactEventFrom[HTMLInputElement & Element] => Callback =
          (e: ReactEventFrom[HTMLInputElement & Element]) => handleTextChange(e.target.value)

        val submit: Callback =
          validate(
            displayValue.value,
            props.codec.valid,
            props.onValidChange,
            { validated =>
              val validatedCB: Callback = validated match
                case Right(a) =>
                  // Only set if resulting A changed. Always redisplay the formatted string, since
                  // the external update is skipped when the typed text parses to the same value.
                  Option.when(props.value.get.exists(_ =!= a))(props.value.set(a)).orEmpty >>
                    displayValue.setState(props.codec.valid.reverseGet(a))
                case Left(e)  =>
                  errors.setState(e.some)
              validatedCB >> props.onBlur(validated)
            }
          )

        val onKeyDown: ReactKeyboardEventFromInput => Callback = e =>
          // TODO keyCode can be undefined (despite the facade). This happens when selecting a value in form auto-fill.
          Option.when(e.keyCode === KeyCode.Enter)(submit).orEmpty

        val onPaste: ReactClipboardEvent => Callback = e =>
          val input: HTMLInputElement    = e.target.asInstanceOf[HTMLInputElement]
          val current: String            = input.value
          val start: Int                 = input.selectionStart
          val end: Int                   = input.selectionEnd
          val prefix: String             = current.substring(0, start)
          val suffix: String             = current.substring(end)
          val paste: String              = e.clipboardData.getData("text")
          val result: String             = s"$prefix$paste$suffix"
          val normalized: Option[String] =
            props.codec.valid.getValid(result).map(props.codec.valid.reverseGet).toOption
          e.preventDefaultCB >> normalized.foldMap(handleTextChange)

        // In many cases it is desireable to validate and normalize pasted text. Numeric fields, for example.
        // However, in some cases we want to allow pasting of anything and just show the error state. See
        // https://app.shortcut.com/lucuma/story/5817/allow-pasting-into-the-invitation-email-field
        // With a mask, Inputmask handles the paste itself.
        val allModifiers: Seq[TagMod] =
          if props.validateOnPaste && props.codec.mask.isEmpty then
            (^.onPaste ==> onPaste) +: props.modifiers
          else props.modifiers

        FormInputText(
          id = props.id,
          label = props.label,
          units = props.units,
          size = props.size,
          groupClass = props.groupClass,
          inputClass =
            error.map(_ => PrimeStyles.Invalid).orEmpty |+| props.inputClass.toOption.orEmpty,
          labelClass = props.labelClass,
          tooltip = error.orUndefined,
          tooltipOptions = props.tooltipOptions,
          disabled = props.disabled,
          preAddons = props.preAddons,
          postAddons = props.postAddons,
          onFocus = props.onFocus,
          onBlur = _ => submit,
          onChange = onTextChange,
          onKeyDown = onKeyDown,
          placeholder = props.placeholder,
          value = displayValue.value,
          modifiers = allModifiers
        )

  protected val component = buildComponent[AnyF, Any]

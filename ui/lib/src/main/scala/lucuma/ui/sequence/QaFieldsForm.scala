// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.DatasetQaState
import lucuma.core.util.Enumerated
import lucuma.react.common.*
import lucuma.react.primereact.Button
import lucuma.react.primereact.InputText
import lucuma.react.primereact.SelectButtonOptional
import lucuma.react.primereact.SelectItem
import lucuma.ui.primereact.*
import lucuma.ui.sequence.given
import lucuma.ui.syntax.render.*
import org.scalajs.dom
import org.scalajs.dom.KeyCode

case class QaFieldsForm(
  qaFields:       View[EditableQaFields],
  submit:         Callback,
  cancel:         Callback,
  submitDisabled: Boolean = false,
  header:         TagMod = TagMod.empty
) extends ReactFnProps(QaFieldsForm)

object QaFieldsForm
    extends ReactFnComponent[QaFieldsForm](props =>
      val submit: Callback = Callback.unless(props.submitDisabled)(props.submit)

      <.form(SequenceStyles.QaEditorPanel)(
        ^.onSubmit ==> (e => e.preventDefaultCB >> submit),
        // Enter submits from the status strip or the comment input (preventDefault keeps the input
        // from submitting again). A focused button keeps its own Enter, so Cancel cancels.
        ^.onKeyDown ==> { (e: ReactKeyboardEventFromHtml) =>
          e.keyCode match
            case KeyCode.Enter if !e.target.isInstanceOf[dom.HTMLButtonElement] =>
              e.preventDefaultCB >> submit
            case _                                                              => Callback.empty
        },
        ^.onClick ==> (_.stopPropagationCB)
      )(
        props.header,
        SelectButtonOptional[DatasetQaState](
          clazz = SequenceStyles.QaStatusButtonStrip,
          value = props.qaFields.get.qaState,
          options = Enumerated[DatasetQaState].all.map: qaState =>
            SelectItem(qaState, qaState.tag),
          onChange = value => props.qaFields.mod(EditableQaFields.qaState.replace(value)),
          itemTemplate = qaState =>
            React.Fragment(
              qaState.value.some.renderVdom,
              qaState.value.tag
            )
        ).compact,
        InputText(
          id = "qaComment",
          value = props.qaFields.get.comment.map(_.value).orEmpty,
          placeholder = "Comment",
          onChange = e =>
            props.qaFields.mod:
              EditableQaFields.comment.replace(NonEmptyString.from(e.target.value).toOption)
        ).withMods(^.autoComplete.off),
        <.div(SequenceStyles.QaEditorPanelButtons)(
          Button(
            icon = SequenceIcons.XMark,
            severity = Button.Severity.Danger,
            onClick = props.cancel
          ).compact("Cancel"),
          Button(
            icon = SequenceIcons.Check,
            severity = Button.Severity.Success,
            disabled = props.submitDisabled
          ).compact
            .withMods(^.`type` := "submit")( // This way it works with "Enter" key.
              "Save"
            )
        )
      )
    )

// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.syntax.all.*
import explore.Icons
import explore.model.Observation
import explore.utils.testId
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CloneSequenceMode
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.Dialog
import lucuma.react.primereact.DialogPosition
import lucuma.ui.primereact.*

/**
 * Asks what a Duplicate of `obs` takes from its Materialized Sequences. Hidden while there is no
 * observation or nothing to ask. Pass the observation as it was when Duplicate was pressed, so the
 * choices don't shift while the dialog is open.
 */
case class SequenceCopyDialog(
  obs:      Option[Observation],
  onChoose: CloneSequenceMode => Callback,
  onCancel: Callback
) extends ReactFnProps(SequenceCopyDialog):
  val choices: List[CloneSequenceMode] = obs.foldMap(_.execution.sequenceCopyChoices)

object SequenceCopyDialog
    extends ReactFnComponent[SequenceCopyDialog](props =>
      def button(choice: CloneSequenceMode): VdomNode =
        val (label, id) = choice match
          case CloneSequenceMode.None         =>
            ("Generate new", "explore-sequence-copy-generate-new")
          case CloneSequenceMode.PendingSteps =>
            ("Copy pending steps", "explore-sequence-copy-pending-steps")
          case CloneSequenceMode.AllSteps
              if props.choices.contains(CloneSequenceMode.PendingSteps) =>
            ("Copy all steps", "explore-sequence-copy-all-steps")
          case CloneSequenceMode.AllSteps     =>
            ("Copy steps", "explore-sequence-copy-all-steps")
        val isDefault   = choice === CloneSequenceMode.None
        Button(
          label = label,
          icon = if isDefault then Icons.New else Icons.Clone,
          onClick = props.onChoose(choice)
        ).small.withMods(^.key := label, ^.autoFocus := isDefault, testId := id)

      val footer: VdomNode =
        <.div(
          props.choices.map(button).toTagMod,
          Button(
            label = "Cancel",
            icon = Icons.Close,
            severity = Button.Severity.Secondary,
            onClick = props.onCancel
          ).small.withMods(testId := "explore-sequence-copy-cancel")
        )

      Dialog(
        visible = props.choices.nonEmpty,
        onHide = props.onCancel,
        header = "Duplicate observation",
        footer = footer,
        position = DialogPosition.Top,
        modal = true,
        resizable = false,
        clazz = LucumaPrimeStyles.Dialog.Small,
        modifiers = List(testId := "explore-sequence-copy-dialog")
      )(
        <.div(
          s"Observation ${props.obs.foldMap(_.displayLabel)} has a stored sequence. " +
            "What should the duplicate start with?"
        )
      )
    )

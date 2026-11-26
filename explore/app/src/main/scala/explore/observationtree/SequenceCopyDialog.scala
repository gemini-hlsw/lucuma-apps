// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.syntax.all.*
import explore.Icons
import explore.model.Group
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

/** A group to duplicate with the choices it had when Duplicate was pressed. */
case class GroupSequenceCopy(group: Group, choices: List[CloneSequenceMode])

/**
 * Asks what a Duplicate of `obs`, or of `groupCopy` when given, takes from its Materialized
 * Sequences. Hidden while there is nothing to duplicate or nothing to ask. Pass them as they were
 * when Duplicate was pressed, so the choices don't shift while the dialog is open.
 */
case class SequenceCopyDialog(
  obs:       Option[Observation],
  onChoose:  CloneSequenceMode => Callback,
  onCancel:  Callback,
  groupCopy: Option[GroupSequenceCopy] = none
) extends ReactFnProps(SequenceCopyDialog):
  val choices: List[CloneSequenceMode] =
    groupCopy.fold(obs.foldMap(_.execution.sequenceCopyChoices))(_.choices)

object SequenceCopyDialog
    extends ReactFnComponent[SequenceCopyDialog](props =>
      def button(choice: CloneSequenceMode): VdomNode =
        val (label, id) = choice match
          case CloneSequenceMode.None         =>
            ("Generate new", "explore-sequence-copy-generate-new")
          case CloneSequenceMode.PendingSteps =>
            ("Copy pending steps", "explore-sequence-copy-pending-steps")
          case CloneSequenceMode.AllSteps     =>
            ("Copy all steps", "explore-sequence-copy-all-steps")
        val isDefault   = choice === CloneSequenceMode.None
        Button(
          label = label,
          icon = if isDefault then Icons.New else Icons.Clone,
          onClick = props.onChoose(choice)
        ).small.compact.withMods(^.key := label, ^.autoFocus := isDefault, testId := id)

      val footer: VdomNode =
        <.div(
          props.choices.map(button).toTagMod,
          Button(
            label = "Cancel",
            icon = Icons.Close,
            severity = Button.Severity.Secondary,
            onClick = props.onCancel
          ).small.compact.withMods(testId := "explore-sequence-copy-cancel")
        )

      Dialog(
        visible = props.choices.nonEmpty,
        onHide = props.onCancel,
        header = if props.groupCopy.isDefined then "Duplicate group" else "Duplicate observation",
        footer = footer,
        position = DialogPosition.Top,
        modal = true,
        resizable = false,
        modifiers = List(testId := "explore-sequence-copy-dialog")
      )(
        <.div(
          props.groupCopy.fold(
            s"${props.obs.foldMap(_.displayLabel)} has a stored sequence. Duplicate with:"
          )(gc => s"Observations in ${groupLabel(gc.group)} have stored sequences. Duplicate with:")
        )
      )
    )

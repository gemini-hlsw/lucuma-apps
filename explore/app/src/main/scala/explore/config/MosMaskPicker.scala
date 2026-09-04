// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import crystal.react.View
import explore.components.HelpIcon
import explore.model.Attachment
import explore.model.AttachmentList
import explore.model.Help
import explore.model.MosMaskSelection
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Instrument
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Dropdown
import lucuma.react.primereact.SelectItem
import lucuma.ui.primereact.*

import scala.collection.immutable.SortedSet

/**
 * Single-select picker for the MOS mask bound to a MOS observation. Only masks cut for the
 * observing mode's instrument are offered.
 */
final case class MosMaskPicker(
  instrument:       Instrument,
  attachmentIdView: View[Option[Attachment.Id]],
  attachments:      View[AttachmentList],
  obsAttachmentIds: View[SortedSet[Attachment.Id]],
  helpId:           Help.Id,
  disabled:         Boolean
) extends ReactFnProps(MosMaskPicker.component)

object MosMaskPicker:
  private type Props = MosMaskPicker

  private val component =
    ScalaFnComponent[Props]: props =>
      val binding    = props.attachmentIdView.get
      val selectable = MosMaskSelection.selectable(props.attachments.get, props.instrument)
      val shown      = MosMaskSelection.shown(props.attachments.get, props.instrument, binding)

      // The "No mask selected" option is a selectable state, it nulls the attachment id.
      val options: List[SelectItem[Option[Attachment.Id]]] =
        Option
          .when(shown.nonEmpty):
            SelectItem(value = none, label = "No mask selected")
          .toList ++
          shown.map(a => SelectItem(value = a.id.some, label = a.displayName.value))

      val placeholder =
        if (selectable.isEmpty) s"No ${props.instrument.longName} masks uploaded"
        else "No mask selected"

      // Binding goes through the Aligner to support undo.
      def handleChange(oid: Option[Attachment.Id]): Callback =
        props.attachmentIdView.set(oid) >>
          oid.fold(Callback.empty): id =>
            props.obsAttachmentIds.mod(_ + id).when_(!props.obsAttachmentIds.get.contains(id))

      React.Fragment(
        <.label(
          ^.htmlFor := "mos-mask",
          LucumaPrimeStyles.FormFieldLabel,
          "Mask ID",
          HelpIcon(props.helpId)
        ),
        Dropdown[Option[Attachment.Id]](
          id = "mos-mask",
          value = binding,
          options = options,
          placeholder = placeholder,
          disabled = props.disabled,
          clazz = LucumaPrimeStyles.FormField,
          onChange = handleChange
        )
      )

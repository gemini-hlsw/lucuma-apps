// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import crystal.react.View
import explore.Icons
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model.Attachment
import explore.model.AttachmentList
import explore.model.Help
import explore.model.syntax.all.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.AttachmentType
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Dropdown
import lucuma.react.primereact.SelectItem
import lucuma.ui.primereact.*

import scala.collection.immutable.SortedSet

/**
 * Single-select picker for the MOS mask bound to a MOS observation
 */
final case class MosMaskPicker(
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
      val disabled   = props.disabled
      val mosMasks   = props.attachments.get
        .listForType(AttachmentType.MosMask)
        .sortBy(a => a.maskName.getOrElse(a.fileName).value)
      val mosMaskIds = mosMasks.map(_.id).toSet
      val binding    = props.attachmentIdView.get
      val dangling   = binding.exists(id => !mosMaskIds.contains(id))

      // The "No mask selected" option is a selectable state — nulls the attachment id
      val options: List[SelectItem[Option[Attachment.Id]]] =
        SelectItem(value = none, label = "No mask selected") ::
          mosMasks.map(a =>
            SelectItem(value = a.id.some, label = a.maskName.getOrElse(a.fileName).value)
          )

      // Binding goes through the Aligner to support undo.
      def handleChange(oid: Option[Attachment.Id]): Callback =
        props.attachmentIdView.set(oid) >>
          oid.fold(Callback.empty): id =>
            props.obsAttachmentIds.mod(_ + id).when_(!props.obsAttachmentIds.get.contains(id))

      React.Fragment(
        <.label(
          ^.htmlFor := "mos-mask",
          LucumaPrimeStyles.FormFieldLabel,
          "MOS Mask",
          HelpIcon(props.helpId)
        ),
        if (dangling)
          // The bound id survives in the observing mode even though no attachment matches it.
          React.Fragment(
            <.div(ExploreStyles.WarningLabel)(
              Icons.ExclamationTriangle.withClass(ExploreStyles.WarningIcon),
              <.span(binding.fold("Missing attachment")(id => s"Missing attachment ($id)"))
            ),
            Dropdown[Option[Attachment.Id]](
              id = "mos-mask",
              value = none,
              options = options,
              placeholder = "Select to replace…",
              disabled = disabled,
              clazz = LucumaPrimeStyles.FormField,
              onChange = handleChange
            )
          )
        else
          Dropdown[Option[Attachment.Id]](
            id = "mos-mask",
            value = binding,
            options = options,
            placeholder = "No mask selected",
            disabled = disabled,
            clazz = LucumaPrimeStyles.FormField,
            onChange = handleChange
          )
      )

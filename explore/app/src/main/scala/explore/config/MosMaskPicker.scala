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
import lucuma.refined.*
import lucuma.ui.primereact.*

import scala.collection.immutable.SortedSet

/**
 * Single-select picker for the MOS mask bound to a GMOS MOS observation's `customMask.attachmentId`.
 *
 * See `docs/adr/0005-mos-mask-binding-is-authoritative-and-sequence-affecting.md`. The binding is
 * authoritative and undo-tracked through `attachmentIdView` (an Aligner view). Selecting a mask
 * additionally tags it onto the observation via `attachmentIds`; the mirror is one-way and
 * additive only — clearing or rebinding never removes a tag.
 *
 * When the bound attachment is no longer present in the program's attachment list (deleted, or
 * invisible to the user), the picker renders an explicit missing-attachment entry naming the id
 * rather than falling back to "Not yet defined".
 */
final case class MosMaskPicker(
  attachmentIdView: View[Option[Attachment.Id]],
  attachments:      View[AttachmentList],
  attachmentIds:    View[SortedSet[Attachment.Id]],
  disabled:         Boolean
) extends ReactFnProps(MosMaskPicker.component)

object MosMaskPicker:
  private type Props = MosMaskPicker

  private val HelpId: Help.Id = "configuration/gmos/mos-mask.md".refined

  private val component =
    ScalaFnComponent[Props]: props =>
      val disabled  = props.disabled
      val mosMasks   = props.attachments.get.listForType(AttachmentType.MosMask)
        .sortBy(_.fileName.value)
      val mosMaskIds = mosMasks.map(_.id).toSet
      val binding    = props.attachmentIdView.get
      val dangling   = binding.exists(id => !mosMaskIds.contains(id))

      // The "Not yet defined" option is a normal, selectable state — null is the schema's
      // pre-Phase-2 default — so it appears in the list alongside the program's masks.
      val options: List[SelectItem[Option[Attachment.Id]]] =
        SelectItem(value = none, label = "Not yet defined") ::
          mosMasks.map(a => SelectItem(value = a.id.some, label = a.fileName.value))

      // Binding goes through the Aligner (undo-tracked). Tagging is additive-only and not
      // undo-tracked: we add the id to the observation's attachment set only when binding to a
      // real mask, and never on clear, rebind or undo. Adding an id already present is a no-op.
      def handleChange(oid: Option[Attachment.Id]): Callback =
        props.attachmentIdView.set(oid) >>
          oid.fold(Callback.empty): id =>
            props.attachmentIds.mod(_ + id).when_(!props.attachmentIds.get.contains(id))

      React.Fragment(
        <.label(
          ^.htmlFor      := "mos-mask",
          LucumaPrimeStyles.FormFieldLabel,
          "MOS Mask",
          HelpIcon(HelpId)
        ),
        if (dangling)
          // The bound id survives in the observing mode (and every sequence step) even though no
          // attachment matches it. Show it as a distinct missing entry, and keep the control
          // operable: selecting a mask rebinds, selecting "Not yet defined" clears.
          React.Fragment(
            <.div(ExploreStyles.WarningLabel)(
              Icons.ExclamationTriangle.withClass(ExploreStyles.WarningIcon),
              <.span(binding.fold("Missing attachment")(id => s"Missing attachment ($id)"))
            ),
            Dropdown[Option[Attachment.Id]](
              id          = "mos-mask",
              value       = none,
              options     = options,
              placeholder = "Select to replace…",
              disabled    = disabled,
              clazz       = LucumaPrimeStyles.FormField,
              onChange    = handleChange
            )
          )
        else
          Dropdown[Option[Attachment.Id]](
            id          = "mos-mask",
            value       = binding,
            options     = options,
            placeholder = "Not yet defined",
            disabled    = disabled,
            clazz       = LucumaPrimeStyles.FormField,
            onChange    = handleChange
          )
      )

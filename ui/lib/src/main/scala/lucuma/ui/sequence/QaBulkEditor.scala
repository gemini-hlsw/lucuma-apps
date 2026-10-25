// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import cats.data.NonEmptyList
import cats.syntax.all.*
import crystal.react.hooks.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.*
import lucuma.react.primereact.Button
import lucuma.react.primereact.Dialog
import lucuma.react.primereact.OverlayPanel
import lucuma.react.primereact.hooks.all.*
import lucuma.schemas.model.Dataset
import lucuma.ui.primereact.*

case class QaBulkEditor(
  datasets: NonEmptyList[Dataset],
  inFlight: Boolean,
  onChange: NonEmptyList[Dataset.Id] => EditableQaFields => Callback
) extends ReactFnProps(QaBulkEditor)

object QaBulkEditor
    extends ReactFnComponent[QaBulkEditor](props =>
      for
        qaFields    <- useStateView(EditableQaFields.Empty)
        panelRef    <- useOverlayPanelRef
        toOverwrite <- useStateView(none[(NonEmptyList[Dataset], EditableQaFields)])
      yield
        val unset: List[Dataset] = props.datasets.filter(_.qaState.isEmpty)

        def overwritten(targets: NonEmptyList[Dataset], fields: EditableQaFields): (Int, Int) =
          (targets.count(_.qaState.isDefined).toInt,
           if fields.comment.isDefined then targets.count(_.comment.isDefined).toInt else 0
          )

        def setQa(targets: NonEmptyList[Dataset], fields: EditableQaFields): Callback =
          props.onChange(targets.map(_.id))(fields)

        // The panel is hidden before confirming, since clicking the dialog would dismiss it anyway.
        def submit(targets: NonEmptyList[Dataset]): Callback =
          val fields: EditableQaFields = qaFields.get
          panelRef.hide >>
            (if overwritten(targets, fields) =!= (0, 0) then toOverwrite.set((targets, fields).some)
             else setQa(targets, fields))

        val noState: Boolean = qaFields.get.qaState.isEmpty

        val reset: Callback = qaFields.set(EditableQaFields.Empty)

        <.span(SequenceStyles.QaBulkEditor)(
          Button(
            icon = SequenceIcons.Pencil,
            label = "Set QA",
            loading = props.inFlight,
            tooltip = "Set the QA state of the datasets of these steps",
            // preventDefault rather than stopPropagation: see the header cell in SequenceColumns.
            onClickE = e => e.preventDefaultCB >> panelRef.toggle(e)
          ).mini.veryCompact,
          OverlayPanel(onHide = reset)(
            QaFieldsForm(
              qaFields,
              NonEmptyList.fromList(unset).foldMap(submit),
              cancel = panelRef.hide,
              submitDisabled = noState || unset.isEmpty,
              submitLabel = s"Set for unset (${unset.size})",
              extraActions = Button(
                icon = SequenceIcons.Check,
                severity = Button.Severity.Warning,
                disabled = noState,
                onClick = submit(props.datasets)
              ).compact(s"Set all (${props.datasets.size})")
            )
          ).withMods(SequenceStyles.QaEditorOverlay).withRef(panelRef.ref),
          Dialog(
            header = "Overwrite QA?",
            visible = toOverwrite.get.isDefined,
            modal = true,
            onHide = toOverwrite.set(none),
            footer = <.div(
              Button(
                label = "Cancel",
                icon = SequenceIcons.XMark,
                severity = Button.Severity.Secondary,
                onClick = toOverwrite.set(none)
              ).compact,
              Button(
                label = "Overwrite",
                icon = SequenceIcons.Check,
                severity = Button.Severity.Danger,
                onClick = toOverwrite.get.foldMap((targets, fields) =>
                  setQa(targets, fields)
                ) >> toOverwrite.set(none)
              ).compact
            )
          )(
            toOverwrite.get.map { (targets, fields) =>
              val (withState, withComment) = overwritten(targets, fields)
              <.div(
                "This will overwrite the existing values of:",
                <.ul(
                  <.li(s"${DatasetQaBulkEdit.datasetCount(withState)} with a QA state")
                    .when(withState > 0),
                  <.li(s"${DatasetQaBulkEdit.datasetCount(withComment)} with a comment")
                    .when(withComment > 0)
                )
              )
            }
          )
        )
    )

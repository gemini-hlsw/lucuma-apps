// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import cats.data.NonEmptyList
import crystal.react.hooks.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.*
import lucuma.react.primereact.Button
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
        qaFields <- useStateView(EditableQaFields.Empty)
        panelRef <- useOverlayPanelRef
      yield
        val submit: Callback =
          props.onChange(props.datasets.map(_.id))(qaFields.get) >> panelRef.hide
        val reset: Callback  = qaFields.set(EditableQaFields.Empty)

        <.span(SequenceStyles.QaBulkEditor)(
          Button(
            icon = SequenceIcons.Pencil,
            label = "Set QA",
            loading = props.inFlight,
            tooltip = "Set the QA state of the datasets that have none yet",
            // preventDefault rather than stopPropagation: see the header cell in SequenceColumns.
            onClickE = e => e.preventDefaultCB >> panelRef.toggle(e)
          ).mini.veryCompact,
          OverlayPanel(onHide = reset)(
            QaFieldsForm(
              qaFields,
              submit,
              cancel = panelRef.hide,
              submitDisabled = qaFields.get.qaState.isEmpty,
              header = <.span(SequenceStyles.QaBulkEditorCount)(
                s"Applies to ${DatasetQaBulkEdit.datasetCount(props.datasets.size)} with no QA state"
              )
            )
          ).withMods(SequenceStyles.QaEditorOverlay).withRef(panelRef.ref)
        )
    )

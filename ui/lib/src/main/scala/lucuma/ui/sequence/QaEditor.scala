// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import crystal.react.hooks.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.*
import lucuma.react.primereact.OverlayPanel
import lucuma.react.primereact.hooks.all.*
import lucuma.schemas.model.Dataset

case class QaEditor(
  dataset:    Dataset,
  renderIcon: VdomNode,
  onChange:   Dataset.Id => EditableQaFields => Callback
) extends ReactFnProps(QaEditor):
  val editableQaFields: EditableQaFields = EditableQaFields.fromDataset.get(dataset)

object QaEditor
    extends ReactFnComponent[QaEditor](props =>
      for
        qaFields <- useStateView(props.editableQaFields)
        panelRef <- useOverlayPanelRef
      yield
        val submit: Callback = props.onChange(props.dataset.id)(qaFields.get) >> panelRef.hide
        val reset: Callback  = qaFields.set(props.editableQaFields)

        <.span(SequenceStyles.QaStatusEditable, ^.onClick ==> panelRef.toggle)(
          props.renderIcon,
          <.span(SequenceStyles.QaStatusSelect)(SequenceIcons.ChevronDown),
          OverlayPanel(onHide = reset)(QaFieldsForm(qaFields, submit, cancel = panelRef.hide))
            .withMods(SequenceStyles.QaEditorOverlay)
            .withRef(panelRef.ref)
        )
    )

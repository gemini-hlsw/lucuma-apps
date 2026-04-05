// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import lucuma.react.common.Css

object SequenceStyles:
  val SequenceTable: Css = Css("lucuma-sequence-table")
  val StepGuided: Css    = Css("lucuma-sequence-step-guided")

  val TableHeader           = Css("lucuma-sequence-header")
  val TableHeaderExpandable = Css("lucuma-sequence-header-expandable")
  val TableHeaderContent    = Css("lucuma-sequence-header-content")

  val HiddenColTableHeader: Css = Css("lucuma-sequence-hidden-col-table-header")
  val RowHasExtra: Css          = Css("lucuma-sequence-row-has-extra")
  val ExtraRowShown: Css        = Css("lucuma-sequence-extra-row-shown")

  val VisitHeader                   = Css("lucuma-sequence-visit-header")
  val VisitStepExtra                = Css("lucuma-sequence-visit-extra-row")
  val VisitStepExtraDatetime        = Css("lucuma-sequence-visit-extra-row-datetime")
  val VisitStepExtraStatus          = Css("lucuma-sequence-visit-extra-row-status")
  val VisitStepExtraDatasets        = Css("lucuma-sequence-visit-extra-row-datasets")
  val VisitStepExtraDatasetItem     = Css("lucuma-sequence-visit-extra-row-dataset-item")
  val VisitStepExtraDatasetQAStatus = Css("lucuma-sequence-visit-extra-row-dataset-qa-status")

  val SequenceInput = Css("lucuma-sequence-input")

  val CurrentHeader = Css("lucuma-sequence-current-header")

  val EditorButtons: Css    = Css("lucuma-sequence-editor-buttons")
  val DragHandleCell: Css   = Css("lucuma-sequence-drag-handle-cell")
  val EditControlsCell: Css = Css("lucuma-sequence-edit-controls-cell")
  val CloneButton: Css      = Css("lucuma-sequence-clone-button")
  val DeleteButton: Css     = Css("lucuma-sequence-delete-button")

  val QaStatusEditable: Css     = Css("lucuma-sequence-qa-status-editable")
  val QaStatusSelect: Css       = Css("lucuma-sequence-qa-status-select")
  val QaEditorOverlay: Css      = Css("lucuma-sequence-qa-editor-overlay")
  val QaEditorPanel: Css        = Css("lucuma-sequence-qa-editor-panel")
  val QaStatusButtonStrip: Css  = Css("lucuma-sequence-qa-status-button-strip")
  val QaEditorPanelButtons: Css = Css("lucuma-sequence-qa-editor-panel-buttons")

  object StepType:
    val Bias: Css   = Css("lucuma-sequence-step-type-bias")
    val Dark: Css   = Css("lucuma-sequence-step-type-dark")
    val Arc: Css    = Css("lucuma-sequence-step-type-arc")
    val Flat: Css   = Css("lucuma-sequence-step-type-flat")
    val Object: Css = Css("lucuma-sequence-step-type-object")

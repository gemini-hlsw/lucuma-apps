// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui

import lucuma.react.common.style.Css

object observeStyles:
  val OnlySmallScreens = Css("only-small-screens")
  val OnlyLargeScreens = Css("only-large-screens")

  val LoginTitle: Css = Css("observe-login-title")

  val Centered: Css       = Css("observe-centered")
  val ComponentLabel: Css = Css("observe-component-label")
  val Shrinkable: Css     = Css("observe-shrinkable")

  // Prime components restyling
  val SequenceTabPanel: Css = Css("observe-sequence-tab-panel")

  val TopPanel: Css              = Css("observe-top-panel")
  val MainPanel: Css             = Css("observe-main-panel")
  val ActiveInstrumentLabel: Css = Css("observe-active-instrument-label")
  val LabelPointer: Css          = Css("observe-label-pointer")
  val IdleTag: Css               = Css("observe-idle-tag")
  val RunningTag: Css            = Css("observe-running-tag")

  val LogArea: Css = Css("observe-log-area")
  val Footer: Css  = Css("observe-footer")

  val ObservationArea: Css           = Css("observe-observation-area")
  val ObservationAreaError: Css      = Css("observe-observation-area-error")
  val SequenceTableExpandButton: Css = Css("observation-area-sequence-table-expand-button")

  val ObserveTable: Css = Css("observe-observe-table")

  val RowIdle: Css     = Css("observe-row-idle")
  val RowPositive: Css = Css("observe-row-positive")
  val RowWarning: Css  = Css("observe-row-warning")
  val RowActive: Css   = Css("observe-row-active")
  val RowNegative: Css = Css("observe-row-negative")
  val RowError: Css    = Css("observe-row-error")
  val RowDisabled: Css = Css("observe-row-disabled")
  val RowDone: Css     = Css("observe-row-done")

  val AcquisitionPrompt: Css     = Css("observe-acquisition-prompt")
  val AcquisitionPromptMain: Css = Css("observe-acquisition-prompt-main")
  val AcquisitionPromptBusy: Css = Css("observe-acquisition-prompt-busy")

  val ObsListTable: Css   = Css("observe-obs-list-table")
  val ObsClassSelect: Css = Css("observe-obs-class-select")
  val LoadButtonCell: Css = Css("observe-load-button-cell")
  val LoadButton: Css     = Css("observe-load-button")

  val ConfigSection: Css       = Css("observe-config-section")
  val ConditionsSection: Css   = Css("observe-conditions-section")
  val ConditionsLabel: Css     = Css("observe-conditions-label")
  val NamesSection: Css        = Css("observe-names-section")
  val ObserverArea: Css        = Css("observe-observer-area")
  val OperatorArea: Css        = Css("observe-operator-area")
  val ImageQualityArea: Css    = Css("observe-image-quality-area")
  val CloudExtinctionArea: Css = Css("observe-cloud-extinction-area")
  val WaterVaporArea: Css      = Css("observe-water-vapor-area")
  val SkyBackgroundArea: Css   = Css("observe-sky-background-area")

  val ConfiguringRow: Css     = Css("observe-configuring-row")
  val StepTable: Css          = Css("observe-step-table")
  val StepSettingsHeader: Css = Css("observe-step-settings-header")

  val StepRowWarning: Css        = Css("observe-step-row-warning")
  val StepRowError: Css          = Css("observe-step-row-error")
  val StepRowDone: Css           = Css("observe-step-row-done")
  val StepRowWithBreakpoint: Css = Css("observe-step-row-with-breakpoint")
  val StepRowFirstInAtom: Css    = Css("observe-stepRow-first-in-atom")
  val StepRowPossibleFuture: Css = Css("observe-step-row-possible-future")

  val ObservationStepProgressBar: Css = Css("observe-observation-progress-bar")
  val ControlButtonStrip: Css         = Css("observe-control-button-strip")
  val PauseButton: Css                = Css("observe-pause-button")
  val CancelPauseButton: Css          = Css("observe-cancel-pause-button")
  val StopButton: Css                 = Css("observe-stop-button")
  val PlayButton: Css                 = Css("observe-play-button")
  val AbortButton: Css                = Css("observe-abort-button")
  val ReloadButton: Css               = Css("observe-reload-button")
  val SingleButton: Css               = Css("observe-single-button")
  val IconSoft: Css                   = Css("observe-icon-soft")
  val QaStatusEditable: Css           = Css("observe-qa-status-editable")
  val QaStatusSelect: Css             = Css("observe-qa-status-select")
  val QaEditorOverlay: Css            = Css("observe-qa-editor-overlay")
  val QaEditorPanel: Css              = Css("observe-qa-editor-panel")
  val QaStatusButtonStrip: Css        = Css("observe-qa-status-button-strip")
  val QaEditorPanelButtons: Css       = Css("observe-qa-editor-panel-buttons")

  val GuidingCell: Css     = Css("observe-guiding-cell")
  val OffsetsBlock: Css    = Css("observe-offsets-block")
  val OffsetsNodLabel: Css = Css("observe-offsets-nod-label")
  val OffsetComponent: Css = Css("observe-offset-component")

  val StepTypeCell: Css              = Css("observe-step-type-cell")
  val StepTypeTag: Css               = Css("observe-step-type-tag")
  val StepTypeCompleted: Css         = Css("observe-step-type-completed")
  val StepTypeObject: Css            = Css("observe-step-type-object")
  val StepTypeArc: Css               = Css("observe-step-type-arc")
  val StepTypeFlat: Css              = Css("observe-step-type-flat")
  val StepTypeBias: Css              = Css("observe-step-type-bias")
  val StepTypeDark: Css              = Css("observe-step-type-dark")
  val StepTypeCalibration: Css       = Css("observe-step-type-calibration")
  val StepTypeAlignAndCalib: Css     = Css("observe-stepType-align-and-calib")
  val StepTypeNodAndShuffle: Css     = Css("observe-stepType-nod-and-shuffle")
  val StepTypeNodAndShuffleDark: Css = Css("observe-stepTypeNod-and-shuffle-dark")

  val BreakpointTableCell: Css = Css("observe-breakpoint-table-cell")

  val BreakpointHandle: Css = Css("observe-breakpoint-handle")
  val BreakpointIcon: Css   = Css("observe-breakpoint-icon")
  val ActiveBreakpoint: Css = Css("observe-active-breakpoint")
  val SkipHandle: Css       = Css("observe-skip-handle")
  val SkipIconSet: Css      = Css("observe-skip-icon-set")

  val DefaultCursor: Css     = Css("observe-default-cursor")
  val ConfigButtonStrip: Css = Css("observe-config-button-strip")
  val ConfigButton: Css      = Css("observe-config-button")

  val SyncingPanel: Css = Css("observe-syncing-panel")

  val ObsHeader: Css            = Css("observe-obs-header")
  val ObsSummary: Css           = Css("observe-obs-summary")
  val ObsSummaryTitle: Css      = Css("observe-obs-summary-title")
  val ObsSummaryDetails: Css    = Css("observe-obs-summary-details")
  val ObsSummarySubsystems: Css = Css("observe-obs-summary-subsystems")
  val ObsSummaryButton: Css     = Css("observe-obs-summary-button")
  val ObsLoadSection: Css       = Css("observe-obs-load-section")

  val LogTable: Css      = Css("observe-log-table")
  val LogWarningRow: Css = Css("observe-log-warning-row")
  val LogErrorRow: Css   = Css("observe-log-error-row")

  val ExternalLink: Css = Css("observe-external-link")

  val Popup: Css = Css("observe-popup")

  object Prime:
    val EmptyProgressBar: Css      = Css("p-progressbar p-component")
    val EmptyProgressBarLabel: Css = Css("p-progressbar-label")

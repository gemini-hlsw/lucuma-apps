// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.components.sequence

import cats.Eq
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.hooks.*
import crystal.react.syntax.effect.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.hooks.Hooks.UseRef
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.enums.SmartGcalType
import lucuma.core.model.sequence.*
import lucuma.core.syntax.display.*
import lucuma.core.util.Enumerated
import lucuma.react.SizePx
import lucuma.react.common.*
import lucuma.react.primereact.Button
import lucuma.react.primereact.MenuItem
import lucuma.react.primereact.PopupMenu
import lucuma.react.primereact.hooks.all.*
import lucuma.react.resizeDetector.hooks.*
import lucuma.react.table.*
import lucuma.schemas.model.enums.StepExecutionState
import lucuma.typed.tanstackVirtualCore as rawVirtual
import lucuma.ui.primereact.*
import lucuma.ui.react.given
import lucuma.ui.reusability.given
import lucuma.ui.sequence.*
import lucuma.ui.table.*
import lucuma.ui.table.hooks.*
import lucuma.ui.undo.UndoContext
import lucuma.ui.undo.UndoStacks
import observe.model.SequenceStatus
import observe.model.StepState
import observe.ui.Icons
import observe.ui.ObserveStyles
import observe.ui.components.sequence.steps.*
import observe.ui.display.given
import observe.ui.model.AppContext
import observe.ui.model.enums.OperationRequest
import observe.ui.model.reusability.given
import observe.ui.services.ODBQueryApi
import observe.ui.services.SequenceApi

import scala.collection.immutable.HashSet
import scala.concurrent.duration.*
import scala.scalajs.LinkingInfo

// Helper to build component objects for instrument sequence tables.
private trait SequenceTableBuilder[S, D: Eq](protected val instrument: Instrument)
    extends SequenceTableDefs[D]:
  private type Props = SequenceTable[S, D]

  private val InsertedStepFlashDuration: FiniteDuration = 2.seconds

  private val ScrollOptions =
    rawVirtual.mod
      .ScrollToOptions()
      .setBehavior(rawVirtual.mod.ScrollBehavior.smooth)
      .setAlign(rawVirtual.mod.ScrollAlignment.start)

  private def scrollToRowId(
    virtualizerRef: UseRef[Option[HTMLTableVirtualizer]],
    table:          Table[SequenceTableRowType, TableMeta, Nothing, Nothing]
  )(rowIdCandidates: List[String]): Callback =
    rowIdCandidates.some
      .filter(_.nonEmpty)
      .flatMap: rowIds =>
        table
          .getRowModel()
          .rows
          .indexWhere(row => rowIds.contains_(row.id.value))
          .some
          .filter(_ >= 0)
      .foldMap: rowIndex =>
        virtualizerRef.get.flatMap: refOpt =>
          Callback: // Auto scroll to running step or next step.
            refOpt.map:
              _.scrollToIndex(rowIndex - 1, ScrollOptions)

  private val DaytimeCalibrationHiddenColumns: List[ColumnId] =
    List(
      SequenceColumns.PColumnId,
      SequenceColumns.QColumnId,
      SequenceColumns.PeakColumnId,
      SequenceColumns.SNColumnId
    )

  protected[sequence] val component =
    ScalaFnComponent[Props]: props =>
      for
        resize                   <- useResizeDetector
        ctx                      <- useContext(AppContext.ctx)
        sequenceApi              <- useContext(SequenceApi.ctx)
        cols                     <-
          useMemo((props.clientMode, props.instrument, props.obsId, props.isPreview)):
            import ctx.given
            columnDefs(_, _, _, _)
        visitsData               <-
          useMemo(
            (props.instrumentVisits,
             props.currentRecordedStepId,
             props.currentRecordedVisit.map(_.visitId)
            )
          ):
            visitsSequences(_, _, _)
        visits                    = visitsData.map(_._1)
        nextScienceIndex          = visitsData.map(_._2)
        acquisitionPromptClicked <- useStateViewWithReuse(none[SequenceType])
        _                        <-
          // The request is also watched because a failed target check keeps the sequence waiting.
          // It only clears on Idle, so the click (Idle -> InFlight) keeps the busy state.
          useEffectWithDeps(
            (props.executionState.isWaitingAcquisitionPrompt, props.requests.acquisitionPrompt)
          ): (_, request) =>
            acquisitionPromptClicked.set(none).when_(request === OperationRequest.Idle)
        sequence                 <-
          useMemo(
            (visits,
             nextScienceIndex,
             props.acquisitionRows,
             props.scienceRows,
             props.currentRecordedVisit.map(_.visitId),
             props.executionState.breakpoints,
             props.executionState.sequenceType,
             props.executionState.isWaitingAcquisitionPrompt,
             props.alertPosition,
             props.requests.acquisitionPrompt,
             acquisitionPromptClicked
            )
          ):
            (
              visits,
              nextScienceIndex,
              acquisitionSteps,
              scienceSteps,
              currentVisitId,
              _,
              sequenceType,
              isWaitingAcquisitionPrompt,
              alertPosition,
              acquisitionPromptRequest,
              acquisitionPromptClicked
            ) =>
              import ctx.given
              val acquisitionPrompt: Option[AlertRow] =
                Option.when(isWaitingAcquisitionPrompt)(
                  AlertRow(
                    sequenceType,
                    alertPosition,
                    AcquisitionPrompt(
                      sequenceApi
                        .proceedAfterPrompt(props.obsId, SequenceType.Science)
                        .runAsync,
                      sequenceApi
                        .proceedAfterPrompt(props.obsId, SequenceType.Acquisition)
                        .runAsync,
                      acquisitionPromptRequest,
                      acquisitionPromptClicked
                    )
                  )
                )

              stitchSequence(
                visits,
                currentVisitId,
                nextScienceIndex,
                acquisitionSteps,
                scienceSteps,
                acquisitionPrompt
              )
        datasetIdsInFlight       <- useStateView(HashSet.empty[Dataset.Id])
        dynTable                 <- useDynTable(DynTableDef, SizePx(resize.width.orEmpty))
        _                        <-
          useEffectWithDeps(props.isDaytimeCalibration): isDaytimeCalibration =>
            dynTable.modifyColumnVisibility:
              _.modify: visibility =>
                if isDaytimeCalibration then
                  visibility ++ DaytimeCalibrationHiddenColumns.map(_ -> Visibility.Hidden)
                else visibility -- DaytimeCalibrationHiddenColumns
        tableState               <-
          useMemo(dynTable.columnSizing, dynTable.columnVisibility):
            (columnSizing, columnVisibility) =>
              PartialTableState(columnSizing = columnSizing, columnVisibility = columnVisibility)
        // TODO Remove the following 4 when sequence editing is supported in observe.
        dummyIsEditing           <- useStateView(IsEditing.False)
        dummyEditableSequence    <- useStateView(EditableSequence(List.empty[Atom[D]]))
        dummyUndoStacks          <- useStateView(UndoStacks.empty[IO, EditableSequence[D]])
        dummyEditContext          = SequenceEditContext[D](
                                      dummyIsEditing,
                                      IsEditInFlight.False,
                                      UndoContext(dummyUndoStacks, dummyEditableSequence),
                                      IO.unit,
                                      Callback.empty,
                                      Nil
                                    )
        table                    <-
          useReactTable:
            TableOptions(
              cols.map(dynTable.setInitialColWidths),
              sequence,
              enableColumnResizing = true,
              enableExpanding = true,
              // Otherwise every data change resets the expanded state to the initial one.
              autoResetExpanded = false,
              getRowId = (row, _, _) => getRowId(row),
              getSubRows = (row, _) => row.subRows,
              columnResizeMode = ColumnResizeMode.OnChange,
              initialState = TableState(expanded = CurrentExpandedState),
              state = tableState,
              onColumnSizingChange = dynTable.onColumnSizingChangeHandler,
              meta = TableMeta(
                props.requests,
                props.executionState,
                props.progress,
                props.selectedRowId,
                props.visits,
                datasetIdsInFlight,
                props.onBreakpointFlip,
                SequenceEditContexts(IsEditEnabled.False, dummyEditContext, dummyEditContext),
                props.maskName,
                DatasetQaBulkEdit(
                  datasetIdsInFlight.get, {
                    import ctx.given
                    onDatasetsQaChange(props.visits, datasetIdsInFlight)
                  }
                )
              )
            )
        odbQueryApi              <- useContext(ODBQueryApi.ctx)
        smartGcalMenuRef         <- usePopupMenuRef
        smartGcalItems           <-
          useMemo(props.obsId): obsId =>
            import ctx.given

            Enumerated[SmartGcalType].all.map: smartGcalType =>
              MenuItem.Item(
                label = smartGcalType.shortName,
                command = odbQueryApi.insertSmartGcal(obsId, smartGcalType).runAsync
              )
        virtualizerRef           <- useRef(none[HTMLTableVirtualizer])
        _                        <-
          useEffectOnMount: // If sequence is not running, auto select next step.
            val autoScrollCandidates: List[String] =
              AlertRowId.toString +:
                (props.loadedStepId ++ props.nextStepId).map(_.toString).toList

            Callback.when(props.loadedStepId.isEmpty)(
              props.nextStepId
                .map(stepId => props.setSelectedRowId(SelectedRowId.forFutureStep(stepId)))
                .orEmpty
            ) >>
              scrollToRowId(virtualizerRef, table)(autoScrollCandidates)
                .delayMs(1) // https://github.com/TanStack/virtual/issues/615
                .toCallback
        _                        <-
          // When sequence changes state or step, auto scroll to running step.
          useEffectWithDeps(
            (props.executionState.sequenceStatus.isRunning,
             props.executionState.sequenceStatus.isWaitingUserPrompt,
             props.loadedStepId
            )
          ): (_, _, loadedStepId) =>
            val autoScrollCandidates: List[String] =
              AlertRowId.toString +: loadedStepId.map(_.toString).toList

            scrollToRowId(virtualizerRef, table)(autoScrollCandidates)
        _                        <- // If sequence completes, show the last visit whole.
          useEffectWithDeps(props.executionState.sequenceStatus):
            case SequenceStatus.Completed =>
              setRowsExpanded(table, getRowId(sequence.last) :: lastVisitRowIds(visits.value), true)
            case _                        => Callback.empty
        _                        <- // The current visit's runs start expanded.
          useEffectWithDeps(
            currentVisitRowIds(
              visits.value,
              props.currentRecordedVisit.map(_.visitId),
              props.acquisitionRows.nonEmpty
            ).map(_.value)
          ): rowIds =>
            expandNewRows(table, rowIds.map(RowId(_)))
        _                        <- // Runs of past visits start collapsed.
          useEffectWithDeps(props.currentRecordedVisit.map(_.visitId)): currentVisitId =>
            setRowsExpanded(table, pastVisitRunRowIds(visits.value, currentVisitId), false)
        renderedInsertedSteps     =
          props.scienceRows
            .flatMap(_.id.right)
            .filter(props.requests.insertedSteps.contains)
            .sortBy(_.toString)
        _                        <- // Inserted steps are flashed once their rows render, then forgotten.
          useEffectWithDeps(renderedInsertedSteps): stepIds =>
            import ctx.given

            Callback.when(stepIds.nonEmpty):
              (IO.sleep(InsertedStepFlashDuration) >>
                odbQueryApi.clearInsertedSteps(props.obsId, stepIds.toSet)).runAsync
      yield
        extension (step: SequenceRow[D]) {
          def isSelected: Boolean =
            props.selectedRowId match
              case Some(rowId) => step.selectableRowId.contains(rowId)
              case _           => false

          def isLoaded: Boolean =
            step.selectableRowId === props.loadedStepId.map(SelectedRowId.forFutureStep(_))
        }

        val tableStyle: Css =
          ObserveStyles.ObserveTable |+| ObserveStyles.StepTable |+| SequenceStyles.SequenceTable

        // The error of the loaded step, which is the one Play will re-run.
        val loadedStepError: Option[String] =
          props.executionState.loadedStep
            .map(_.status)
            .collect:
              case StepState.Failed(msg) => msg

        def computeRowMods(row: SequenceTableRowType): TagMod =
          row.value.toOption
            .map(_.step)
            .map: step =>
              val selectableRowIdOpt: Option[SelectedRowId] = step.selectableRowId
              val stepHasBreakpoint: Boolean                =
                selectableRowIdOpt
                  .exists: selectedRowId =>
                    props.executionState.breakpoints.contains_(selectedRowId.stepId)
              val stepWasInserted: Boolean                  =
                selectableRowIdOpt
                  .exists: selectedRowId =>
                    props.requests.insertedSteps.contains_(selectedRowId.stepId)

              TagMod(
                selectableRowIdOpt
                  .map: selectableRowId =>
                    TagMod(
                      // Only in dev mode, show step id on hover.
                      if (LinkingInfo.developmentMode) {
                        val executionState: Option[StepExecutionState] = step match
                          case SequenceRow.Executed.ExecutedStep(_, stepRecord, _, _) =>
                            stepRecord.executionState.some
                          case _                                                      =>
                            none
                        ^.title := s"${selectableRowId.stepId}" +
                          selectableRowId.visitId.fold("")(vid => s" Visit: $vid") +
                          executionState.fold("")(es => s" ($es)")
                      } else TagMod.empty,
                      (^.onClick --> props.setSelectedRowId(selectableRowId))
                        .unless:
                          props.executionState.isLocked
                    )
                  .whenDefined,
                SequenceStyles.RowHasExtra
                  .when_(step.isSelected || step.isLoaded || step.isFinished),
                ObserveStyles.RowIdle.unless_(step.isSelected || step.isLoaded),
                ObserveStyles.StepRowWithBreakpoint.when_(stepHasBreakpoint),
                ObserveStyles.StepRowInserted.when_(stepWasInserted),
                ObserveStyles.StepRowFirstInAtom.when_(step.isFirstInAtom),
                loadedStepError
                  .filter(_ => step.isLoaded)
                  .map(msg => TagMod(ObserveStyles.StepRowError, ^.title := msg))
                  .whenDefined,
                step.stepState match
                  case s if s.hasError                      => ObserveStyles.StepRowError
                  case StepState.Paused                     => ObserveStyles.StepRowWarning
                  case StepState.Completed                  => ObserveStyles.StepRowDone
                  case StepState.Aborted if step.isFinished => ObserveStyles.StepRowError
                  case _                                    => TagMod.empty
              )
            .orEmpty

        def computeHeaderCellMods(
          headerCell: Header[SequenceTableRowType, ?, TableMeta, ?, ?, ?, ?]
        ): TagMod =
          headerCell.column.id match
            case id if id == HeaderColumnId     => SequenceStyles.HiddenColTableHeader
            case id if id == BreakpointColumnId => SequenceStyles.HiddenColTableHeader
            case id if id == ExtraRowColumnId   => SequenceStyles.HiddenColTableHeader
            case _                              => TagMod.empty

        val extraRowMod: TagMod =
          TagMod(
            SequenceStyles.ExtraRowShown,
            resize.width
              .map: w =>
                ^.width := s"${w - ColumnSizes(BreakpointSpaceColumnId).initial.value}px"
              .whenDefined
          )

        def computeCellMods(
          cell: Cell[SequenceTableRowType, ?, TableMeta, ?, ?, ?, ?]
        ): TagMod =
          cell.row.original.value match
            case Left(_)        => // Header
              cell.column.id match
                case id if id == HeaderColumnId =>
                  TagMod(^.colSpan := table.getVisibleLeafColumns().length)
                case _                          => ^.display.none
            case Right(stepRow) =>
              cell.column.id match
                case id if id == HeaderColumnId     => // TODO MOVE TO STYLE
                  TagMod(^.border := "0px", ^.padding := "0px")
                case id if id == BreakpointColumnId =>
                  ObserveStyles.BreakpointTableCell
                case id if id == ExtraRowColumnId   =>
                  stepRow.step match // Extra row is shown in a selected row or in an executed step row.
                    case SequenceRow.Executed.ExecutedStep(_, _, _, _) => extraRowMod
                    case step if step.isSelected || step.isLoaded      => extraRowMod
                    case _                                             => TagMod.empty
                case _                              =>
                  TagMod.empty

        // The current visit holds the live sections, so it's never collapsed.
        val pastVisits: List[VisitData]         =
          visits.value.filterNot: visit =>
            props.currentRecordedVisit.map(_.visitId).contains_(visit.visitId)
        val visitIds: Set[RowId]                = pastVisits.map(_.rowId).toSet
        val runIds: List[(RowId, SequenceType)] =
          pastVisits.flatMap: visit =>
            visit.runs.map(run => visit.runRowId(run) -> run.sequenceType)
        // Collapses every visit and, so that they start afresh when reopened, their runs.
        val collapseVisits: Callback            = table.modExpanded:
          case Expanded.AllRows    =>
            Expanded.Rows(
              (CurrentExpandedRowIds.map(_ -> true) ++ visitIds.map(_ -> false) ++
                runIds.map(_._1 -> false)).toMap
            )
          case Expanded.Rows(rows) =>
            Expanded.Rows(rows ++ visitIds.map(_ -> false) ++ runIds.map(_._1 -> false))

        // Expands the visits having runs of `sequenceType`, and those runs. Nothing is collapsed.
        def expandVisits(sequenceType: SequenceType): Callback = table.modExpanded:
          case Expanded.AllRows    => Expanded.AllRows
          case Expanded.Rows(rows) =>
            Expanded.Rows(
              rows ++
                pastVisits
                  .filter(_.runs.exists(_.sequenceType === sequenceType))
                  .map(_.rowId -> true) ++
                runIds.collect:
                  case (rowId, runType) if runType === sequenceType => rowId -> true
            )

        val rows: List[Row[SequenceTableRowType, TableMeta, Nothing, Nothing]] =
          table.getExpandedRowModel().rows

        def forAllVisits(
          onContains: Row[SequenceTableRowType, TableMeta, Nothing, Nothing] => Boolean
        ): Boolean =
          rows.forall: row =>
            if visitIds.contains(row.id) then onContains(row) else true

        val allVisitsAreCollapsed: Boolean = forAllVisits(!_.getIsExpanded())

        val isInsertSmartGcalInFlight: Boolean =
          props.requests.insertSmartGcal === OperationRequest.InFlight

        def estimateRowHeight(index: Int): SizePx =
          table.getRowModel().rows.get(index).map(_.original.value) match
            case Some(
                  Right(SequenceIndexedRow(SequenceRow.Executed.ExecutedStep(_, _, _, _), _))
                ) =>
              SequenceRowHeight.WithExtra
            case _ =>
              SequenceRowHeight.Regular

        React.Fragment(
          <.div(ObserveStyles.SequenceTableToolbar)(
            props.sequenceControls,
            <.div(ObserveStyles.SequenceTableVisitButtons)(
              React
                .Fragment(
                  Button(
                    icon = Icons.Plus,
                    label = "Expand all acquisitions",
                    disabled = !runIds.exists(_._2 === SequenceType.Acquisition),
                    onClick = expandVisits(SequenceType.Acquisition)
                  ).mini.compact,
                  Button(
                    icon = Icons.Plus,
                    label = "Expand all science",
                    disabled = !runIds.exists(_._2 === SequenceType.Science),
                    onClick = expandVisits(SequenceType.Science)
                  ).mini.compact,
                  Button(
                    icon = Icons.Minus,
                    label = "Collapse all visits",
                    disabled = allVisitsAreCollapsed,
                    onClick = collapseVisits
                  ).mini.compact
                )
                .when(visitIds.nonEmpty)
            ),
            <.div(ObserveStyles.SequenceTableToolbarEnd)(
              Button(
                icon = Icons.Plus,
                label = "SmartGcal",
                severity = Button.Severity.Success,
                loading = isInsertSmartGcalInFlight,
                onClickE = smartGcalMenuRef.toggle,
                disabled =
                  props.executionState.sequenceStatus.isCompleted || isInsertSmartGcalInFlight
              ).mini.compact.unless(props.isPreview || props.isDaytimeCalibration),
              PopupMenu(model = smartGcalItems).withRef(smartGcalMenuRef.ref)
            )
          ),
          PrimeAutoHeightVirtualizedTable(
            table,
            estimateSize = estimateRowHeight,
            overscan = 8,
            containerRef = resize.ref,
            virtualizerRef = virtualizerRef,
            tableMod = TagMod(tableStyle),
            rowMod = rowTagMod(row => computeRowMods(row.original)),
            headerCellMod = computeHeaderCellMods,
            cellMod = cellTagMod(computeCellMods)
          )
        )

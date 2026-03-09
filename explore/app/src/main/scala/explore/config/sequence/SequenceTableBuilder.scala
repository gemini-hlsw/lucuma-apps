// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import cats.Endo
import cats.Eq
import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import crystal.react.View
import crystal.react.hooks.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Instrument
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Dataset
import lucuma.react.SizePx
import lucuma.react.primereact.ToastRef
import lucuma.react.resizeDetector.hooks.*
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.schemas.ObservationDB
import lucuma.schemas.model.ExecutionVisits
import lucuma.schemas.model.enums.StepExecutionState
import lucuma.ui.reusability.given
import lucuma.ui.sequence.*
import lucuma.ui.sequence.SequenceColumns.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import lucuma.ui.table.ColumnSize.*
import lucuma.ui.table.hooks.*
import org.typelevel.log4cats.Logger

import scala.collection.immutable.HashSet
import scala.scalajs.LinkingInfo

private type SequenceColumnsType[D] =
  SequenceColumns[D, SequenceIndexedRow[D], SequenceRow[D], Nothing, Nothing, Nothing]
private type ColumnType[D]          =
  ColumnDef[Expandable[HeaderOrRow[SequenceIndexedRow[D]]], ?, Nothing, Nothing, Nothing, Any, Any]

private trait SequenceTableBuilder[S, D: Eq](instrument: Instrument)
    extends SequenceRowBuilder[D]
    with SequenceEditOptics[D]:
  private type Props = SequenceTable[S, D]

  private case class TableMeta[D](
    allVisits:          View[Option[ExecutionVisits]],
    datasetIdsInFlight: View[HashSet[Dataset.Id]],
    toastRef:           ToastRef,
    isEditing:          IsEditing = IsEditing.False,
    modAcquisition:     Endo[Option[Atom[D]]] => Callback,
    modScience:         Endo[List[Atom[D]]] => Callback,
    isUserStaffOrAdmin: Boolean
  ) extends SequenceTableMeta[D]

  private lazy val ColDef = ColumnDef[SequenceTableRowType].WithTableMeta[TableMeta[D]]

  private val HeaderColumnId: ColumnId   = ColumnId("header")
  private val ExtraRowColumnId: ColumnId = ColumnId("extraRow")

  private lazy val ColumnSizes: Map[ColumnId, ColumnSize] = Map(
    HeaderColumnId   -> FixedSize(0.toPx),
    ExtraRowColumnId -> FixedSize(0.toPx)
  ) ++ SequenceColumns.BaseColumnSizes(instrument)

  private def columns(using FetchClient[IO, ObservationDB], Logger[IO]): List[ColDef.Type] =
    List(
      SequenceColumns
        .headerCell(HeaderColumnId, ColDef)
        .withColumnSize(ColumnSizes(HeaderColumnId)),
      ColDef(
        ExtraRowColumnId,
        header = "",
        cell = cell =>
          cell.table.options.meta.map: meta =>
            cell.row.original.value.toOption
              .map(_.step)
              .collect:
                case step @ SequenceRow.Executed.ExecutedStep(_, _) =>
                  renderVisitExtraRow(
                    step,
                    showOngoingLabel = true,
                    enableQaEditor =
                      meta.isUserStaffOrAdmin && (step.executionState match // QA editor only in completed steps
                        case StepExecutionState.Completed | StepExecutionState.Stopped => true
                        case _                                                         => false),
                    allVisits = meta.allVisits,
                    datasetIdsInFlight = meta.datasetIdsInFlight,
                    toastRef = meta.toastRef
                  )
      ).withColumnSize(ColumnSizes(ExtraRowColumnId))
    ) ++ SequenceColumns(ColDef, _.step.some, _.index.some)(instrument)

  private lazy val DynTableDef = DynTable(
    ColumnSizes,
    SequenceColumns.BaseColumnPriorities(instrument),
    DynTable.ColState(
      resized = ColumnSizing(),
      visibility = ColumnVisibility()
    ),
    collapsibleCols = Set(DragHandleColumnId, EditControlsColumnId)
  )

  protected[sequence] val component =
    ScalaFnComponent[Props]: props =>
      for
        ctx                <- useContext(AppContext.ctx)
        visitsData         <- useMemo(props.instrumentVisits):
                                visitsSequences(_, none)
        resize             <- useResizeDetector
        dynTable           <- useDynTable(DynTableDef, SizePx(resize.width.orEmpty))
        cols               <- useMemo(()): _ =>
                                import ctx.given
                                dynTable.setInitialColWidths(columns)
        rows               <-
          useMemo(
            (visitsData, props.acquisitionRows, props.scienceRows, props.currentVisitId)
          ): (visitsData, acquisition, science, currentVisitId) =>
            val (visitRows, nextScienceIndex): (List[VisitData], StepIndex) = visitsData.value
            stitchSequence(
              visitRows,
              currentVisitId,
              nextScienceIndex,
              acquisition,
              science
            )
        datasetIdsInFlight <- useStateView(HashSet.empty[Dataset.Id])
        // TODO Change SequenceQaEditHelper to use an implicit ToastCtx when it's unified with observe
        toastRef           <- useEffectResultOnMount(ctx.toastRef.get)
        tableState         <-
          useMemo(dynTable.columnSizing, dynTable.columnVisibility):
            (columnSizing, columnVisibility) =>
              PartialTableState(columnSizing = columnSizing, columnVisibility = columnVisibility)
        table              <-
          useReactTable:
            TableOptions(
              cols,
              rows,
              enableSorting = false,
              enableColumnResizing = true,
              enableExpanding = true,
              getRowId = (row, _, _) => getRowId(row),
              getSubRows = (row, _) => row.subRows,
              columnResizeMode = ColumnResizeMode.OnChange,
              initialState = TableState(
                expanded = CurrentExpandedState
              ),
              state = tableState,
              onColumnSizingChange = dynTable.onColumnSizingChangeHandler,
              meta = TableMeta(
                props.visits,
                datasetIdsInFlight,
                toastRef.value.value.toOption.getOrElse(null),
                props.isEditing,
                props.modAcquisition,
                props.modScience,
                props.isUserStaffOrAdmin
              )
            )
        _                  <-
          useEffectWithDeps(props.isEditing.value): isEditing =>
            dynTable.modCollapsedCols(_ => !isEditing)
        tableDnd           <- useVirtualizedTableDragAndDrop(
                                table,
                                DragHandleColumnId,
                                getData = _.original.value.toOption
                                  .flatMap:
                                    _.step match
                                      case SequenceRow.futureStep(fs) => (fs.stepId, fs.seqType).some
                                      case _                          => none,
                                containerRef = resize.ref,
                                onDrop = (sourceData, target) =>
                                  (table.options.meta,
                                   sourceData.map(_._1),
                                   sourceData.map(_._2),
                                   target.flatMap(_.data.map(_._1)),
                                   target.map(_.edge)
                                  )
                                    .mapN: (meta, sourceStepId, seqType, targetStepId, edge) =>
                                      meta.seqTypeMod(seqType):
                                        moveStep(sourceStepId, targetStepId, edge)
                                    .orEmpty,
                                canDrop = (targetData, sourceArgs) =>
                                  targetData.exists: // Only allow dragging into same sequence type
                                    case (targetStepId, targetSeqType) =>
                                      sourceArgs.source.data.value.exists: (sourceStepId, sourceSeqType) =>
                                        sourceSeqType === targetSeqType && sourceStepId != targetStepId
                              )
      yield
        val extraRowMod: TagMod =
          TagMod(
            SequenceStyles.ExtraRowShown,
            resize.width
              .map: w =>
                ^.width := s"${w}px"
              .whenDefined
          )

        tableDnd
          .context:
            PrimeAutoHeightVirtualizedTable(
              table,
              estimateSize = index =>
                table.getRowModel().rows.get(index).map(_.original.value) match
                  case Some(
                        Right(SequenceIndexedRow(SequenceRow.Executed.ExecutedStep(_, _), _))
                      ) =>
                    SequenceRowHeight.WithExtra
                  case _ =>
                    SequenceRowHeight.Regular,
              overscan = 8,
              containerRef = tableDnd.containerRef,
              compact = Compact.Very,
              hoverableRows = true,
              celled = true,
              tableMod = SequenceStyles.SequenceTable,
              headerCellMod = _.column.id match
                case id if id == HeaderColumnId       => SequenceStyles.HiddenColTableHeader
                case id if id == ExtraRowColumnId     => SequenceStyles.HiddenColTableHeader
                case id if id == DragHandleColumnId   =>
                  SequenceStyles.HiddenColTableHeader.unless(props.isEditing.value)
                case id if id == EditControlsColumnId =>
                  SequenceStyles.HiddenColTableHeader.unless(props.isEditing.value)
                case _                                => TagMod.empty,
              rowMod = tableDnd.rowMod: (row, _) =>
                row.original.value.fold(
                  _ => ExploreStyles.SequenceRowHeader,
                  stepRow =>
                    val step: SequenceRow[D] = stepRow.step
                    TagMod(
                      step match
                        case SequenceRow.Executed.ExecutedStep(step, _)                       =>
                          SequenceStyles.RowHasExtra |+|
                            ExploreStyles.SequenceRowDone.unless_(
                              step.executionState == StepExecutionState.Ongoing
                            )
                        case SequenceRow.FutureStep(_, _, firstOf, _, _) if firstOf.isDefined =>
                          ExploreStyles.SequenceRowFirstInAtom
                        case _                                                                => TagMod.empty,
                      if (LinkingInfo.developmentMode)
                        step.id.toOption.map(^.title := _.toString).whenDefined
                      else TagMod.empty
                    )
                ),
              cellMod = tableDnd.cellMod: (cell, _) =>
                cell.row.original.value match
                  case Left(_)        => // Header
                    cell.column.id match
                      case id if id == HeaderColumnId => TagMod(^.colSpan := cols.length)
                      case _                          => ^.display.none
                  case Right(stepRow) =>
                    cell.column.id match
                      case id if id == DragHandleColumnId   =>
                        TagMod(^.paddingRight := "0")
                      case id if id == EditControlsColumnId =>
                        TagMod(^.paddingLeft := "0")
                      case id if id == ExtraRowColumnId     =>
                        stepRow.step match // Extra row is shown in a selected row or in an executed step row.
                          case SequenceRow.Executed.ExecutedStep(_, _) => extraRowMod
                          case _                                       => TagMod.empty
                      case _                                =>
                        TagMod.empty
            )

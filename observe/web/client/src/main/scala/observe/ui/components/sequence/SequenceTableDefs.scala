// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.components.sequence

import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import crystal.react.View
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Instrument
import lucuma.core.model.Observation
import lucuma.core.model.sequence.*
import lucuma.react.SizePx
import lucuma.react.common.*
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.schemas.ObservationDB
import lucuma.schemas.model.ExecutionVisits
import lucuma.schemas.model.enums.StepExecutionState
import lucuma.ui.primereact.ToastCtx
import lucuma.ui.sequence.*
import lucuma.ui.table.*
import lucuma.ui.table.ColumnSize.*
import lucuma.ui.table.hooks.*
import observe.model.ExecutionState
import observe.model.StepProgress
import observe.model.Subsystem
import observe.model.enums.ActionStatus
import observe.model.enums.Resource
import observe.ui.Icons
import observe.ui.ObserveStyles
import observe.ui.components.sequence.steps.*
import observe.ui.model.ObservationRequests
import observe.ui.model.enums.ClientMode
import org.typelevel.log4cats.Logger

import scala.collection.immutable.HashSet

import scalajs.js

// Offload SequenceTable definitions to improve legibility.
trait SequenceTableDefs[D] extends SequenceRowBuilder[D]:
  protected def instrument: Instrument

  private lazy val isGmos: Boolean =
    List(Instrument.GmosNorth, Instrument.GmosSouth).contains_(instrument)

  private lazy val stepResources: StepConfig => Set[Subsystem] =
    case StepConfig.Bias | StepConfig.Dark => if isGmos then Set(Resource.Gcal) else Set.empty
    case StepConfig.Gcal(_, _, _, _)       => Set(Resource.TCS, Resource.Gcal)
    case StepConfig.Science                => Set(Resource.TCS, Resource.Gcal)
    case StepConfig.SmartGcal(_)           => throw new RuntimeException("Smart GCAL is not supported")

  protected case class TableMeta(
    requests:           ObservationRequests,
    executionState:     ExecutionState,
    progress:           Option[StepProgress],
    selectedRowId:      Option[SelectedRowId],
    allVisits:          View[Option[ExecutionVisits]],
    datasetIdsInFlight: View[HashSet[Dataset.Id]],
    onBreakpointFlip:   (Observation.Id, Step.Id) => Callback,
    editContexts:       SequenceEditContexts[D]
  ) extends SequenceTableMeta[D]

  protected val ColDef = ColumnDef[SequenceTableRowType].WithTableMeta[TableMeta]

  // Breakpoint column has width 0 but is translated and actually shown.
  // We display an empty BreakpointSpace column to show the space with correct borders.
  protected val HeaderColumnId: ColumnId          = ColumnId("header")
  protected val BreakpointColumnId: ColumnId      = ColumnId("breakpoint")
  protected val BreakpointSpaceColumnId: ColumnId = ColumnId("breakpointDummy")
  // Will be rendered as a full-width column in an extra row
  protected val ExtraRowColumnId: ColumnId        = ColumnId("extraRow")
  // protected val ObsModeColumnId: ColumnId         = ColumnId("obsMode")
  protected val CameraColumnId: ColumnId          = ColumnId("camera")
  protected val DeckerColumnId: ColumnId          = ColumnId("decker")
  protected val ReadModeColumnId: ColumnId        = ColumnId("readMode")
  protected val ImagingMirrorColumnId: ColumnId   = ColumnId("imagingMirror")
  protected val SettingsColumnId: ColumnId        = ColumnId("settings")

  // Observe-specific detail columns. Some of these ids (e.g. decker, readMode) are also provided
  // by the shared per-instrument `SequenceColumns` (e.g. for GNIRS and Flamingos2). In that case
  // we must not declare them again here: doing so would produce duplicate column ids and, because
  // these are hidden by default, would also hide the shared column. So they are filtered against
  // the instrument's shared columns below (`extraDetailColumnIds`).
  private val detailColumnIds: List[ColumnId] =
    List(CameraColumnId, DeckerColumnId, ReadModeColumnId, ImagingMirrorColumnId)

  private lazy val instrumentColDefs: List[ColDef.Type] =
    SequenceColumns(ColDef, _._1.some, _._2.some)(instrument)

  // Column ids contributed by the shared per-instrument `SequenceColumns`.
  private lazy val instrumentColumnIds: Set[ColumnId] =
    instrumentColDefs.map(_.id).toSet

  // Detail columns not already provided by the shared instrument columns.
  protected lazy val extraDetailColumnIds: List[ColumnId] =
    detailColumnIds.filterNot(instrumentColumnIds.contains)

  protected lazy val ColumnSizes: Map[ColumnId, ColumnSize] = Map(
    HeaderColumnId          -> FixedSize(0.toPx),
    BreakpointColumnId      -> FixedSize(0.toPx),
    BreakpointSpaceColumnId -> FixedSize(30.toPx),
    ExtraRowColumnId        -> FixedSize(0.toPx),
    CameraColumnId          -> Resizable(10.toPx),
    DeckerColumnId          -> Resizable(10.toPx),
    ReadModeColumnId        -> Resizable(180.toPx),
    ImagingMirrorColumnId   -> Resizable(10.toPx),
    SettingsColumnId        -> FixedSize(39.toPx)
  ) ++ SequenceColumns.BaseColumnSizes(instrument)

  // The order in which they are removed by overflow. The ones at the beginning go first.
  // Missing columns are not removed by overflow. (We declare them in reverse order)
  protected lazy val ColumnPriorities: List[ColumnId] =
    List(
      CameraColumnId,
      DeckerColumnId,
      ReadModeColumnId,
      ImagingMirrorColumnId,
      SettingsColumnId
    ).reverse ++ SequenceColumns.BaseColumnPriorities(instrument)

  protected lazy val DynTableDef = DynTable(
    ColumnSizes,
    ColumnPriorities,
    DynTable.ColState(
      resized = ColumnSizing(),
      visibility = ColumnVisibility(
        extraDetailColumnIds.map(_ -> Visibility.Hidden).toMap
      )
    ),
    collapsibleCols = Set(SequenceColumns.DragHandleColumnId, SequenceColumns.EditControlsColumnId)
      .filter(_ => instrument.isSequenceEditable)
  )

// [T, A, TM, CM, TF, CF, FM]
  private def column[V](
    id:     ColumnId,
    header: VdomNode,
    cell:   js.UndefOr[CellContext[SequenceTableRowType, V, TableMeta, ?, ?, ?, ?] => VdomNode] =
      js.undefined
  ): ColDef.TypeFor[V] =
    ColDef(id, header = _ => header, cell = cell)

  protected def columnDefs(
    clientMode: ClientMode,
    instrument: Instrument,
    obsId:      Observation.Id,
    isPreview:  Boolean
  )(using
    FetchClient[IO, ObservationDB],
    ToastCtx[IO],
    Logger[IO]
  ): List[ColDef.Type] =
    List(
      SequenceColumns
        .headerCell(HeaderColumnId, ColDef)
        .withColumnSize(ColumnSizes(HeaderColumnId)),
      column(
        BreakpointColumnId,
        "",
        cell =>
          (cell.row.original.value.toOption, cell.table.options.meta).mapN: (stepRow, meta) =>
            val step: SequenceRow[D]    = stepRow.step
            val stepId: Option[Step.Id] = step.id.toOption
            // val canSetBreakpoint =
            //   clientStatus.canOperate && step.get.canSetBreakpoint(
            //     execution.map(_.steps).orEmpty
            //   )

            <.div(
              <.div(
                ObserveStyles.BreakpointHandle,
                stepId
                  .map: sId =>
                    ^.onClick ==> (_.stopPropagationCB >> meta.onBreakpointFlip(obsId, sId))
                  .whenDefined
              )(
                Icons.CircleSolid
                  .withFixedWidth()
                  .withClass(
                    ObserveStyles.BreakpointIcon |+|
                      // ObserveStyles.FlippableBreakpoint.when_(canSetBreakpoint) |+|
                      ObserveStyles.ActiveBreakpoint
                        .when_(stepId.exists(meta.executionState.breakpoints.contains_(_)))
                  )
              ).when(
                cell.row.index.toInt > 0 &&
                  step.stepTime === StepTime.Future &&
                  meta.executionState.loadedStep.map(_.id) =!= stepId
              )
            )
      ),
      column(BreakpointSpaceColumnId, "", _ => EmptyVdom),
      column(
        ExtraRowColumnId,
        "",
        cell =>
          cell.table.options.meta.map: meta =>
            cell.row.original.value.toOption
              .map(_.step)
              .map[VdomNode]:
                case step @ SequenceRow.Executed.ExecutedStep(_, _, _) =>
                  renderVisitExtraRow(
                    step,
                    showOngoingLabel = false,
                    enableQaEditor = step.executionState match // QA editor only in completed steps
                      case StepExecutionState.Completed | StepExecutionState.Stopped => true
                      case _                                                         => false
                    ,
                    allVisits = meta.allVisits,
                    datasetIdsInFlight = meta.datasetIdsInFlight
                  )
                case step                                              =>
                  (step.selectableRowId, step.stepTypeDisplay, step.exposureTime, step.stepConfig)
                    .mapN: (selectableRowId, stepType, exposureTime, stepConfig) =>
                      List(
                        meta.selectedRowId,
                        meta.executionState.loadedStep.map(ls => SelectedRowId.forFutureStep(ls.id))
                      ).collectFirst {
                        case Some(selRowId) if selRowId === selectableRowId =>
                          val stepId: Step.Id = selRowId.stepId

                          def inactiveStepResourceMap: Map[Subsystem, ActionStatus] =
                            (stepResources(stepConfig) + instrument)
                              .map(_ -> ActionStatus.Pending)
                              .toMap

                          StepProgressCell(
                            clientMode = clientMode,
                            instrument = instrument,
                            stepId = stepId,
                            stepType = stepType,
                            isFinished = step.isFinished,
                            obsId = obsId,
                            requests = meta.requests,
                            loadedStepId = meta.executionState.loadedStep.map(_.id),
                            fileIds = step.fileIds,
                            sequenceStatus = meta.executionState.sequenceStatus,
                            isPausedInStep =
                              meta.executionState.pausedStep.exists(_.value === stepId),
                            subsystemStatus =
                              meta.executionState.stepResources.getOrElse(inactiveStepResourceMap),
                            systemOverrides = meta.executionState.systemOverrides,
                            exposureTime = exposureTime,
                            progress = meta.progress,
                            isPreview = isPreview
                          )
                      }
      )
    ) ++
      instrumentColDefs.map(colDef => colDef.withColumnSize(ColumnSizes(colDef.id))) ++
      List(
        // column(ObsModeColumnId, "Observing Mode"),
        column(CameraColumnId, "Camera"),
        column(DeckerColumnId, "Decker"),
        column(ReadModeColumnId, "ReadMode"),
        column(ImagingMirrorColumnId, "ImagingMirror")
      ).filterNot(c => instrumentColumnIds.contains(c.id)) ++
      List(
        column(
          SettingsColumnId,
          Icons.RectangleList,
          _ => SettingsCell() // TODO
        )
      )

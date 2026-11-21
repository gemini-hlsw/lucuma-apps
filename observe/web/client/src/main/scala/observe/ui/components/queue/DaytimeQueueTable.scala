// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.components.queue

import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Instrument
import lucuma.core.model.Observation
import lucuma.core.model.ObservationReference
import lucuma.core.model.Program
import lucuma.core.syntax.display.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.react.common.*
import lucuma.react.fa.FontAwesomeIcon
import lucuma.react.fa.IconSize
import lucuma.react.fa.LayeredIcon
import lucuma.react.primereact.Button
import lucuma.react.primereact.ProgressBar
import lucuma.react.primereact.Tag
import lucuma.react.primereact.Tooltip
import lucuma.react.primereact.TooltipOptions
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.ui.dnd.computeIndexInList
import lucuma.ui.format.DurationSpacedFormatter
import lucuma.ui.primereact.*
import lucuma.ui.sequence.SequenceIcons
import lucuma.ui.sequence.SequenceStyles
import lucuma.ui.table.*
import lucuma.ui.table.hooks.*
import observe.model.QueueItemId
import observe.model.Subsystem
import observe.model.enums.QueueItemStatus
import observe.model.given
import observe.ui.Icons
import observe.ui.ObserveStyles
import observe.ui.display.given
import observe.ui.model.AppContext
import observe.ui.model.DaytimeQueueObsRow
import observe.ui.model.DaytimeQueueProgress
import observe.ui.model.DaytimeQueueRow
import observe.ui.model.enums.AppTab
import observe.ui.model.reusability.given
import observe.ui.services.DaytimeQueueApi
import observe.ui.services.SequenceApi

import scala.scalajs.js.JSConverters.*

case class DaytimeQueueTable(
  rows:             List[DaytimeQueueRow],
  loadedObsIds:     Set[Observation.Id],
  holdRequestedIds: Set[Observation.Id],
  linkToExploreObs: Option[
    Reusable[Either[(Program.Id, Observation.Id), ObservationReference] => VdomNode]
  ],
  move:             (QueueItemId, Int) => Callback
) extends ReactFnProps(DaytimeQueueTable)

object DaytimeQueueTable
    extends ReactFnComponent[DaytimeQueueTable](props =>
      case class TableMeta(
        runObs:            Observation.Id => Callback,
        pauseObs:          Observation.Id => Callback,
        cancelPauseObs:    Observation.Id => Callback,
        holdRequestedIds:  Set[Observation.Id],
        abortObs:          Observation.Id => Callback,
        retryObs:          Observation.Id => Callback,
        remove:            QueueItemId => Callback,
        linkToExploreObs:  Either[(Program.Id, Observation.Id), ObservationReference] => VdomNode,
        loadedObsIds:      Set[Observation.Id],
        instrumentTabUrl:  Instrument => String,
        openInstrumentTab: Instrument => Callback
      )

      val ColDef = ColumnDef[DaytimeQueueRow].WithTableMeta[TableMeta]

      val DragHandleColumnId: ColumnId = ColumnId("dragHandle")
      val StatusColumnId: ColumnId     = ColumnId("status")
      val ControlsColumnId: ColumnId   = ColumnId("controls")
      val ObsColumnId: ColumnId        = ColumnId("observation")
      val InstrumentColumnId: ColumnId = ColumnId("instrument")
      val SubsystemsColumnId: ColumnId = ColumnId("subsystems")
      val EstimateColumnId: ColumnId   = ColumnId("estimate")
      val ProgressColumnId: ColumnId   = ColumnId("progress")
      val NoteColumnId: ColumnId       = ColumnId("note")
      val ActionsColumnId: ColumnId    = ColumnId("actions")

      // A pause shows its divider in the status column, spanning up to the actions.
      val PauseSpannedColumnIds: Set[ColumnId] =
        Set(
          ControlsColumnId,
          ObsColumnId,
          InstrumentColumnId,
          SubsystemsColumnId,
          EstimateColumnId,
          ProgressColumnId,
          NoteColumnId
        )
      val PauseColSpan: Int                    = PauseSpannedColumnIds.size + 1

      val tooltipOptions: TooltipOptions =
        TooltipOptions(position = Tooltip.Position.Top, showDelay = 100, showOnDisabled = true)

      def subsystemName(subsystem: Subsystem): String = Display[Subsystem].shortName(subsystem)

      def subsystemNames(subsystems: List[Subsystem]): String =
        subsystems.map(subsystemName).mkString(", ")

      def obsCell(row: DaytimeQueueRow)(render: DaytimeQueueObsRow => VdomNode): VdomNode =
        row match
          case DaytimeQueueRow.Obs(obs)    => render(obs)
          case DaytimeQueueRow.Pause(_, _) => EmptyVdom

      def statusSeverity(status: QueueItemStatus): Option[Tag.Severity] =
        status match
          case QueueItemStatus.Waiting   => none
          case QueueItemStatus.Running   => Tag.Severity.Warning.some
          case QueueItemStatus.Stopped   => Tag.Severity.Info.some
          case QueueItemStatus.Completed => Tag.Severity.Success.some
          case QueueItemStatus.Failed    => Tag.Severity.Danger.some
          case QueueItemStatus.Aborted   => Tag.Severity.Danger.some

      def rowClass(row: DaytimeQueueRow): Css =
        row match
          case DaytimeQueueRow.Pause(_, _) => ObserveStyles.DaytimeQueuePauseRow
          case DaytimeQueueRow.Obs(obs)    =>
            obs.status match
              case QueueItemStatus.Running                          => ObserveStyles.RowActive
              case QueueItemStatus.Completed                        => ObserveStyles.RowDone
              case QueueItemStatus.Failed | QueueItemStatus.Aborted => ObserveStyles.RowNegative
              case _                                                => Css.Empty

      // The same buttons the sequence has, so they look and read alike. Each is in a span of its
      // own: the tooltip moves the button into a wrapper it adds, and React must not find the
      // button gone when it removes it.
      def controlButton(
        icon:    FontAwesomeIcon | LayeredIcon,
        style:   Css,
        tooltip: String,
        enabled: Boolean,
        onClick: Callback
      ): VdomNode =
        <.span(
          Button(
            clazz = style |+| ObserveStyles.ObsSummaryButton,
            icon = icon,
            disabled = !enabled,
            tooltip = tooltip,
            tooltipOptions = tooltipOptions,
            onClick = onClick
          )
        )

      def removeButton(tooltip: String, enabled: Boolean, onClick: Callback): VdomNode =
        Button(
          icon = Icons.Trash,
          clazz = ObserveStyles.DaytimeQueueActionButton,
          text = true,
          severity = Button.Severity.Danger,
          disabled = !enabled,
          tooltip = tooltip,
          tooltipOptions = tooltipOptions,
          onClick = onClick
        ).tiny.compact

      def obsControls(obs: DaytimeQueueObsRow, meta: TableMeta): VdomNode =
        val runTooltip: String     =
          if obs.runApplies && obs.blockedBy.nonEmpty then
            s"Cannot run: ${subsystemNames(obs.blockedBy)} in use"
          else "Run now"
        val isRunning: Boolean     = obs.status === QueueItemStatus.Running
        val holdRequested: Boolean = meta.holdRequestedIds.contains(obs.obsId)

        <.div(ObserveStyles.DaytimeQueueControls)(
          controlButton(
            Icons.Play.withSize(IconSize.LG),
            ObserveStyles.PlayButton,
            runTooltip,
            obs.canRun,
            meta.runObs(obs.obsId)
          ).unless(isRunning),
          controlButton(
            Icons.PlayPause.withSize(IconSize.LG),
            ObserveStyles.SequenceHoldButton,
            "Hold after the current step",
            obs.canPause,
            meta.pauseObs(obs.obsId)
          ).when(isRunning && !holdRequested),
          // Shown while the hold is pending: it says so, and cancels it.
          controlButton(
            Icons.CancelSequenceHold.withSize(IconSize.LG),
            ObserveStyles.CancelSequenceHoldButton,
            "Holding after the current step: cancel",
            true,
            meta.cancelPauseObs(obs.obsId)
          ).when(isRunning && holdRequested),
          controlButton(
            SequenceIcons.XMark.withSize(IconSize.LG),
            ObserveStyles.AbortButton,
            "Abort",
            obs.canAbort,
            meta.abortObs(obs.obsId)
          ).when(isRunning),
          controlButton(
            Icons.ArrowsRotate.withSize(IconSize.LG),
            ObserveStyles.ReloadButton,
            "Retry: back to Waiting, for the queue to run it in turn",
            obs.canRetry,
            meta.retryObs(obs.obsId)
          ).when(obs.canRetry)
        )

      def progressBar(progress: DaytimeQueueProgress): VdomNode =
        ProgressBar(
          value = (progress.fraction * 100.0).floor,
          clazz = ObserveStyles.DaytimeQueueProgressBar
        )

      val columns: Reusable[List[ColDef.Type]] =
        Reusable.always:
          List(
            ColDef(
              DragHandleColumnId,
              header = "",
              cell = cell =>
                <.span(SequenceStyles.DragHandleCell)(
                  SequenceIcons.GripDotsVertical.unless(cell.row.original.isLocked)
                ),
              size = 25.toPx,
              enableResizing = false
            ),
            ColDef(
              StatusColumnId,
              header = "Status",
              cell = cell =>
                cell.row.original match
                  case DaytimeQueueRow.Obs(obs)    =>
                    Tag(
                      value = obs.status.shortName,
                      severity = statusSeverity(obs.status).orUndefined
                    )
                  case DaytimeQueueRow.Pause(_, _) =>
                    <.div(ObserveStyles.DaytimeQueuePauseDivider)(Icons.Pause, "Pause")
              ,
              size = 100.toPx
            ),
            ColDef(
              ControlsColumnId,
              header = "",
              cell = cell =>
                cell.table.options.meta.map: meta =>
                  obsCell(cell.row.original)(obsControls(_, meta)),
              size = 90.toPx,
              enableResizing = false
            ),
            ColDef(
              ObsColumnId,
              header = "Observation",
              cell = cell =>
                obsCell(cell.row.original): obs =>
                  <.span(
                    obs.summary.fold(obs.obsId.shortName)(_.refAndId),
                    (cell.table.options.meta, obs.summary).mapN: (meta, summary) =>
                      meta.linkToExploreObs:
                        summary.obsReference.toRight((summary.programId, summary.obsId))
                  ),
              size = 320.toPx
            ),
            ColDef(
              InstrumentColumnId,
              header = "Instrument",
              cell = cell =>
                obsCell(cell.row.original): obs =>
                  val instrument: Instrument = obs.item.instrument
                  // The instrument tab only exists while the observation is loaded.
                  cell.table.options.meta
                    .filter(_.loadedObsIds.contains(obs.obsId))
                    .fold[VdomNode](instrument.shortName): meta =>
                      <.a(
                        ^.href := meta.instrumentTabUrl(instrument),
                        ^.onClick ==> (e =>
                          e.preventDefaultCB >> meta.openInstrumentTab(instrument)
                        )
                      )(instrument.shortName)
              ,
              size = 110.toPx
            ),
            ColDef(
              SubsystemsColumnId,
              header = "Subsystems",
              cell = cell =>
                obsCell(cell.row.original): obs =>
                  // As the sequence shows them: observatory subsystems, then the instrument.
                  subsystemNames(Enumerated[Subsystem].all.filter(obs.item.subsystems.contains)),
              size = 160.toPx
            ),
            ColDef(
              EstimateColumnId,
              header = "Remaining",
              cell = cell =>
                obsCell(cell.row.original): obs =>
                  // Once it has run, what is left of its total; before that, only the estimate.
                  obs.progress.fold(DurationSpacedFormatter(obs.item.estimate.toDuration)):
                    progress =>
                      s"${DurationSpacedFormatter(progress.remaining.toDuration)} of " +
                        DurationSpacedFormatter(progress.total.toDuration)
              ,
              size = 170.toPx
            ),
            ColDef(
              ProgressColumnId,
              header = "Progress",
              cell = cell => obsCell(cell.row.original)(_.progress.map(progressBar)),
              size = 160.toPx
            ),
            ColDef(
              NoteColumnId,
              header = "Note",
              cell = cell => obsCell(cell.row.original)(_.note.map(_.text(subsystemName))),
              size = 260.toPx
            ),
            ColDef(
              ActionsColumnId,
              header = "",
              cell = cell =>
                cell.table.options.meta.map: meta =>
                  cell.row.original match
                    case DaytimeQueueRow.Obs(obs)          =>
                      removeButton(
                        "Remove from the queue",
                        obs.canRemove,
                        meta.remove(QueueItemId.Obs(obs.obsId))
                      )
                    case row @ DaytimeQueueRow.Pause(_, _) =>
                      removeButton(
                        "Remove this pause",
                        true,
                        meta.remove(row.itemId)
                      )
              ,
              size = 45.toPx,
              enableResizing = false
            )
          )

      for
        ctx         <- useContext(AppContext.ctx)
        queueApi    <- useContext(DaytimeQueueApi.ctx)
        sequenceApi <- useContext(SequenceApi.ctx)
        table       <-
          useReactTable:
            import ctx.given

            TableOptions(
              columns,
              Reusable.implicitly(props.rows),
              enableSorting = false,
              enableColumnResizing = true,
              columnResizeMode = ColumnResizeMode.OnChange,
              getRowId = (row, _, _) =>
                row.itemId match
                  case QueueItemId.Obs(obsId)     => RowId(obsId.toString)
                  case QueueItemId.Pause(pauseId) => RowId(pauseId.value.toString)
              ,
              meta = TableMeta(
                runObs = obsId => queueApi.runObs(obsId).runAsync,
                pauseObs = obsId => sequenceApi.requestSequenceHold(obsId).runAsync,
                cancelPauseObs = obsId => sequenceApi.cancelSequenceHoldRequest(obsId).runAsync,
                holdRequestedIds = props.holdRequestedIds,
                abortObs = obsId => queueApi.abortObs(obsId).runAsync,
                retryObs = obsId => queueApi.retryObs(obsId).runAsync,
                remove = id => queueApi.remove(id).runAsync,
                linkToExploreObs = obsIdOrRef =>
                  props.linkToExploreObs.map(_.value(obsIdOrRef)).getOrElse(EmptyVdom),
                loadedObsIds = props.loadedObsIds,
                instrumentTabUrl = instrument => ctx.pageUrl(AppTab.LoadedObs(instrument)),
                openInstrumentTab = instrument => ctx.pushPage(AppTab.LoadedObs(instrument))
              )
            )
        // The drag handlers are registered once, so they read the current rows through a ref.
        rowsRef     <- useShadowRef(props.rows)
        moveRef     <- useShadowRef(props.move)
        tableDnd    <-
          useTableDragAndDrop(
            table,
            DragHandleColumnId,
            getData = _.original.itemId,
            canDrag =
              (id, _) => rowsRef.get.runNow().exists(row => row.itemId === id && !row.isLocked),
            onDrop = (source, target) =>
              // The server takes the index in the queue without the moved item.
              rowsRef.get.flatMap: (rows: List[DaytimeQueueRow]) =>
                target
                  .flatMap: t =>
                    computeIndexInList[QueueItemId](_ === t.data, t.edge)(
                      rows.map(_.itemId).filterNot(_ === source)
                    )
                  .foldMap(toIndex => moveRef.get.flatMap(_(source, toIndex)))
          )
      yield tableDnd.context(
        PrimeTable(
          table,
          hoverableRows = true,
          tableMod = ObserveStyles.ObserveTable |+| ObserveStyles.DaytimeQueueTable,
          rowMod = tableDnd.rowMod((row, _) => rowClass(row.original)),
          cellMod = (cell, context, render) =>
            cell.row.original match
              case DaytimeQueueRow.Pause(_, _) if PauseSpannedColumnIds.contains(cell.column.id) =>
                EmptyVdom
              case DaytimeQueueRow.Pause(_, _) if cell.column.id == StatusColumnId               =>
                tableDnd.cellMod()(
                  cell,
                  context,
                  render(^.colSpan := PauseColSpan, ^.width := "auto")
                )
              case _                                                                             =>
                tableDnd.cellMod()(cell, context, render)
          ,
          emptyMessage = <.div(ObserveStyles.DaytimeQueueEmpty)(
            "The daytime queue is empty. Add daytime observations from the Observations tab."
          )
        )
      )
    )

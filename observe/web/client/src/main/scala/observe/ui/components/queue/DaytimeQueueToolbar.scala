// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.components.queue

import cats.syntax.all.*
import crystal.react.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.syntax.display.*
import lucuma.react.common.*
import lucuma.react.primereact.Button
import lucuma.react.primereact.ConfirmDialog
import lucuma.react.primereact.DialogPosition
import lucuma.react.primereact.ProgressBar
import lucuma.react.primereact.Tag
import lucuma.react.primereact.Tooltip
import lucuma.react.primereact.TooltipOptions
import lucuma.react.primereact.tooltip.*
import lucuma.refined.*
import lucuma.ui.format.DurationLongWithSecondsFormatter
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import observe.model.DaytimeQueue
import observe.model.enums.QueueDispatchState
import observe.model.enums.QueueOrderingMode
import observe.ui.Icons
import observe.ui.ObserveStyles
import observe.ui.display.given
import observe.ui.model.AppContext
import observe.ui.model.DaytimeQueuePrediction
import observe.ui.model.DaytimeQueueRows
import observe.ui.services.DaytimeQueueApi

import java.time.ZoneId
import java.time.format.DateTimeFormatter
import scala.scalajs.js.JSConverters.*

case class DaytimeQueueToolbar(
  queue:      DaytimeQueue,
  prediction: Option[DaytimeQueuePrediction],
  zone:       ZoneId
) extends ReactFnProps(DaytimeQueueToolbar)

object DaytimeQueueToolbar
    extends ReactFnComponent[DaytimeQueueToolbar](props =>
      val FinishTimeFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("HH:mm")

      val tooltipOptions: TooltipOptions =
        TooltipOptions(position = Tooltip.Position.Top, showDelay = 100, showOnDisabled = true)

      def dispatchSeverity(state: QueueDispatchState): Option[Tag.Severity] =
        state match
          case QueueDispatchState.Idle    => none
          case QueueDispatchState.Running => Tag.Severity.Success.some
          case QueueDispatchState.Halted  => Tag.Severity.Warning.some

      for
        ctx      <- useContext(AppContext.ctx)
        queueApi <- useContext(DaytimeQueueApi.ctx)
      yield
        import ctx.given

        val queue: DaytimeQueue = props.queue
        val hasRunning: Boolean = DaytimeQueueRows.hasRunning(queue)

        val newPauseIndex: Option[Int] = DaytimeQueueRows.newPauseIndex(queue)

        // The server owns the mode: a change is sent, and comes back with the queue.
        val orderingMode: View[QueueOrderingMode] =
          View(
            queue.orderingMode,
            (mod, cb) =>
              val newMode: QueueOrderingMode = mod(queue.orderingMode)
              queueApi.setOrderingMode(newMode).runAsync >> cb(queue.orderingMode, newMode)
          )

        def modeTooltip(mode: QueueOrderingMode): String = mode match
          case QueueOrderingMode.Strict    =>
            "Observations start in queue order: one that cannot start holds back those below it"
          case QueueOrderingMode.JumpAhead =>
            "An observation that cannot start is skipped: those below it that can run start"

        val abortAll: Callback =
          ConfirmDialog.confirmDialog(
            message = <.div("Every running observation of the queue will be aborted. Abort all?"),
            header = "Abort all",
            acceptLabel = "Yes, abort all",
            position = DialogPosition.Top,
            accept = queueApi.abortAll.runAsync,
            icon = Icons.TriangleExclamation
          )

        <.div(ObserveStyles.DaytimeQueueToolbar)(
          <.div(ObserveStyles.DaytimeQueueToolbarGroup)(
            Button(
              label = "Run Queue",
              icon = Icons.Play,
              severity = Button.Severity.Success,
              disabled = !DaytimeQueueRows.canRunQueue(queue),
              tooltip = "Start every observation that can run, in queue order",
              tooltipOptions = tooltipOptions,
              onClick = queueApi.run.runAsync
            ).small.compact,
            Button(
              label = "Pause after current step",
              icon = Icons.Pause,
              clazz = ObserveStyles.DaytimeQueueHoldButton,
              disabled = !hasRunning,
              tooltip = "Running observations pause at the end of their current step",
              tooltipOptions = tooltipOptions,
              onClick = queueApi.holdAfterCurrentSteps.runAsync
            ).small.compact,
            Button(
              label = "Pause after current observations",
              icon = Icons.PlayPause,
              clazz = ObserveStyles.DaytimeQueueHoldButton,
              disabled = !DaytimeQueueRows.canHalt(queue),
              tooltip = "Running observations finish, and no other observation starts",
              tooltipOptions = tooltipOptions,
              onClick = queueApi.holdAfterCurrentObservations.runAsync
            ).small.compact,
            Button(
              label = "Abort all",
              icon = Icons.Stop,
              severity = Button.Severity.Danger,
              disabled = !hasRunning,
              onClick = abortAll
            ).small.compact,
            <.label(^.htmlFor := "daytime-queue-ordering-mode")("Mode:"),
            SelectButtonEnumView(
              "daytime-queue-ordering-mode".refined,
              orderingMode,
              groupClass = LucumaPrimeStyles.Small |+| LucumaPrimeStyles.Compact,
              itemTemplate = item =>
                <.span(ObserveStyles.DaytimeQueueModeItem)(item.value.shortName).withTooltip(
                  content = modeTooltip(item.value),
                  position = Tooltip.Position.Top,
                  showDelay = 100
                )
            ),
            Button(
              label = "Add pause",
              icon = Icons.Plus,
              severity = Button.Severity.Secondary,
              disabled = newPauseIndex.isEmpty,
              tooltip =
                if newPauseIndex.isEmpty then
                  "A pause goes between two observations, below the running ones"
                else "Add a pause; drag it to its place",
              tooltipOptions = tooltipOptions,
              onClick = newPauseIndex.foldMap(index => queueApi.addPause(index).runAsync)
            ).small.compact,
            Button(
              label = "Clear completed",
              icon = Icons.Broom,
              severity = Button.Severity.Secondary,
              disabled = !DaytimeQueueRows.hasCompleted(queue),
              onClick = queueApi.clearCompleted.runAsync
            ).small.compact
          ),
          <.div(ObserveStyles.DaytimeQueueToolbarGroup)(
            props.prediction.map: prediction =>
              <.div(ObserveStyles.DaytimeQueuePrediction)(
                ProgressBar(
                  id = "daytime-queue-progress",
                  value = prediction.fraction * 100.0,
                  showValue = false,
                  clazz = ObserveStyles.DaytimeQueuePredictionBar
                ),
                <.span(
                  s"${DurationLongWithSecondsFormatter(prediction.remaining.toDuration)} left, ends at " +
                    FinishTimeFormatter.format(prediction.finishAt.toInstant.atZone(props.zone))
                )
              ),
            Tag(
              value = queue.dispatchState.shortName,
              severity = dispatchSeverity(queue.dispatchState).orUndefined
            )
          )
        )
    )

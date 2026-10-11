// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.components.sequence

import cats.syntax.all.*
import crystal.*
import crystal.react.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.*
import lucuma.react.fa.IconSize
import lucuma.react.primereact.Button
import lucuma.react.primereact.Tooltip
import lucuma.react.primereact.TooltipOptions
import lucuma.ui.LucumaIcons
import observe.model.Observation
import observe.model.SequenceStatus
import observe.model.enums.RunOverride
import observe.ui.Icons
import observe.ui.ObserveStyles
import observe.ui.model.AppContext
import observe.ui.model.ObservationRequests
import observe.ui.model.enums.OperationRequest
import observe.ui.services.SequenceApi

case class SeqControlButtons(
  obsId:            Observation.Id,
  refreshing:       Pot[View[Boolean]],
  sequenceStatus:   SequenceStatus,
  isObserveStarted: Boolean,
  requests:         ObservationRequests
) extends ReactFnProps(SeqControlButtons):
  val isSequenceHoldRequested: Boolean      = sequenceStatus.isSequenceHoldRequested
  val isSequenceHoldInFlight: Boolean       = requests.sequenceHold === OperationRequest.InFlight
  val isRewindInFlight: Boolean             = requests.rewind === OperationRequest.InFlight
  val isRewindRequested: Boolean            = sequenceStatus.isRewindRequested
  val isCancelRewindInFlight: Boolean       = requests.cancelRewind === OperationRequest.InFlight
  val isCancelSequenceHoldInFlight: Boolean =
    requests.cancelSequenceHold === OperationRequest.InFlight
  val isRunning: Boolean                    = sequenceStatus.isRunning
  val isWaitingUserPrompt: Boolean          = sequenceStatus.isWaitingUserPrompt
  val isRefreshing: Boolean                 = refreshing.exists(_.get)
  val isCompleted: Boolean                  = sequenceStatus.isCompleted

object SeqControlButtons
    extends ReactFnComponent[SeqControlButtons](props =>
      val tooltipOptions =
        TooltipOptions(position = Tooltip.Position.Top, showDelay = 100, showOnDisabled = true)

      for
        ctx         <- useContext(AppContext.ctx)
        sequenceApi <- useContext(SequenceApi.ctx)
      yield
        import ctx.given

        <.div(ObserveStyles.SeqControlButtons)(
          Button(
            clazz = ObserveStyles.PlayButton |+| ObserveStyles.ObsSummaryButton,
            loading = props.isRefreshing,
            icon = Icons.Play.withFixedWidth().withSize(IconSize.LG),
            loadingIcon = LucumaIcons.CircleNotch.withFixedWidth().withSize(IconSize.LG).withSpin(),
            tooltip = "Start/Resume sequence",
            tooltipOptions = tooltipOptions,
            onClick = props.refreshing.toOption.foldMap(_.set(true)) >>
              sequenceApi.startSequence(props.obsId, RunOverride.Override).runAsync,
            disabled = props.isRefreshing || props.isCompleted
          ).when(!props.isRunning),
          Button(
            clazz = ObserveStyles.SequenceHoldButton |+| ObserveStyles.ObsSummaryButton,
            icon = Icons.PlayPause.withFixedWidth().withSize(IconSize.LG),
            tooltip = "Hold sequence after current step",
            tooltipOptions = tooltipOptions,
            onClick = sequenceApi.requestSequenceHold(props.obsId).runAsync,
            disabled = props.isSequenceHoldInFlight || props.isWaitingUserPrompt ||
              props.isRewindRequested
          ).when(props.isRunning && !props.isSequenceHoldRequested),
          Button(
            clazz = ObserveStyles.CancelSequenceHoldButton |+| ObserveStyles.ObsSummaryButton,
            icon = Icons.CancelSequenceHold.withFixedWidth().withSize(IconSize.LG),
            tooltip = "Cancel sequence hold",
            tooltipOptions = tooltipOptions,
            onClick = sequenceApi.cancelSequenceHoldRequest(props.obsId).runAsync,
            disabled = props.isCancelSequenceHoldInFlight || props.isWaitingUserPrompt
          ).when(props.isRunning && props.isSequenceHoldRequested),
          Button(
            clazz = ObserveStyles.RewindButton |+| ObserveStyles.ObsSummaryButton,
            icon = Icons.BackwardStep.withFixedWidth().withSize(IconSize.LG),
            tooltip = "Rewind step: stop before the exposure and go idle. Run configures again.",
            tooltipOptions = tooltipOptions,
            onClick = sequenceApi.rewindStep(props.obsId).runAsync,
            disabled = !props.isRunning || props.isObserveStarted || props.isRewindInFlight ||
              props.isWaitingUserPrompt
          ).when(!props.isRewindRequested),
          Button(
            clazz = ObserveStyles.CancelRewindButton |+| ObserveStyles.ObsSummaryButton,
            icon = Icons.CancelRewind.withFixedWidth().withSize(IconSize.LG),
            tooltip = "Cancel rewind request",
            tooltipOptions = tooltipOptions,
            onClick = sequenceApi.cancelRewindRequest(props.obsId).runAsync,
            disabled = props.isCancelRewindInFlight || props.isWaitingUserPrompt
          ).when(props.isRewindRequested)
        )
    )

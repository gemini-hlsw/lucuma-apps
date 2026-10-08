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
  val isRewindRequested: Boolean            = sequenceStatus.isStepInterruptRequested
  val isCancelSequenceHoldInFlight: Boolean =
    requests.cancelSequenceHold === OperationRequest.InFlight
  val isRunning: Boolean                    = sequenceStatus.isRunning
  val isWaitingUserPrompt: Boolean          = sequenceStatus.isWaitingUserPrompt
  val isRefreshing: Boolean                 = refreshing.exists(_.get)
  val isCompleted: Boolean                  = sequenceStatus.isCompleted

object SeqControlButtons
    extends ReactFnComponent[SeqControlButtons](props =>
      val tooltipOptions =
        TooltipOptions(position = Tooltip.Position.Top, showDelay = 100)

      for
        ctx         <- useContext(AppContext.ctx)
        sequenceApi <- useContext(SequenceApi.ctx)
      yield
        import ctx.given

        <.span(
          // Button(
          //   clazz = ObserveStyles.PlayButton |+| ObserveStyles.ObsSummaryButton,
          //   loading = props.loadedObsId.exists(_.isPending),
          //   icon = Icons.FileArrowUp.withFixedWidth().withSize(IconSize.LG),
          //   loadingIcon = LucumaIcons.CircleNotch.withFixedWidth().withSize(IconSize.LG),
          //   tooltip = "Load sequence",
          //   tooltipOptions = tooltipOptions,
          //   onClick = props.loadObs(props.obsId),
          //   disabled = props.isReady
          // ).when(!selectedObsIsLoaded),
          Button(
            clazz = ObserveStyles.PlayButton |+| ObserveStyles.ObsSummaryButton,
            loading = props.isRefreshing,
            icon = Icons.Play.withFixedWidth().withSize(IconSize.LG),
            tooltip = "Start/Resume sequence",
            tooltipOptions = tooltipOptions,
            onClick = props.refreshing.toOption.foldMap(_.set(true)) >>
              sequenceApi.startSequence(props.obsId, RunOverride.Override).runAsync,
            disabled = props.isRefreshing || props.isCompleted
          ).when(!props.isRunning),
          Button(
            clazz = ObserveStyles.RewindButton |+| ObserveStyles.ObsSummaryButton,
            icon = Icons.BackwardStep.withFixedWidth().withSize(IconSize.LG),
            tooltip = "Rewind step: stop before the exposure and go idle. Run configures again.",
            tooltipOptions = tooltipOptions,
            onClick = sequenceApi.rewindStep(props.obsId).runAsync,
            disabled =
              props.isRewindInFlight || props.isRewindRequested || props.isWaitingUserPrompt
          ).when(props.isRunning && !props.isObserveStarted),
          Button(
            clazz = ObserveStyles.SequenceHoldButton |+| ObserveStyles.ObsSummaryButton,
            icon = Icons.PlayPause.withFixedWidth().withSize(IconSize.LG),
            tooltip = "Hold sequence after current step",
            tooltipOptions = tooltipOptions,
            onClick = sequenceApi.requestSequenceHold(props.obsId).runAsync,
            disabled = props.isSequenceHoldInFlight || props.isWaitingUserPrompt
          ).when(props.isRunning && props.isObserveStarted && !props.isSequenceHoldRequested),
          Button(
            clazz = ObserveStyles.CancelSequenceHoldButton |+| ObserveStyles.ObsSummaryButton,
            icon = Icons.CancelSequenceHold.withFixedWidth().withSize(IconSize.LG),
            tooltip = "Cancel sequence hold",
            tooltipOptions = tooltipOptions,
            onClick = sequenceApi.cancelSequenceHoldRequest(props.obsId).runAsync,
            disabled = props.isCancelSequenceHoldInFlight || props.isWaitingUserPrompt
          ).when(props.isRunning && props.isObserveStarted && props.isSequenceHoldRequested)
          // Button(
          //   clazz = ObserveStyles.ReloadButton |+| ObserveStyles.ObsSummaryButton,
          //   loading = props.isRefreshing,
          //   icon = Icons.ArrowsRotate.withFixedWidth().withSize(IconSize.LG),
          //   loadingIcon = Icons.ArrowsRotate.withFixedWidth().withSize(IconSize.LG).withSpin(),
          //   tooltip = "Reload sequence from ODB",
          //   tooltipOptions = tooltipOptions,
          //   onClick = props.refreshing.toOption.foldMap(_.set(true)) >>
          //     sequenceApi.loadObservation(props.obsId, props.instrument).runAsync,
          //   disabled = props.loadedObsId.exists(_.isPending) || props.isRunning
          // ).when(selectedObsIsLoaded)
        )
    )

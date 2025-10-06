// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.components.sequence

import cats.syntax.all.*
import crystal.*
import crystal.react.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Instrument
import lucuma.react.common.*
import lucuma.react.fa.IconSize
import lucuma.react.primereact.Button
import lucuma.react.primereact.Tooltip
import lucuma.react.primereact.TooltipOptions
import observe.model.Observation
import observe.model.SequenceState
import observe.model.enums.RunOverride
import observe.ui.Icons
import observe.ui.ObserveStyles
import observe.ui.model.AppContext
import observe.ui.model.ObservationRequests
import observe.ui.model.enums.OperationRequest
import observe.ui.services.SequenceApi

case class SeqControlButtons(
  obsId:         Observation.Id,
  loadedObsId:   Option[Pot[Observation.Id]],
  refreshing:    Pot[View[Boolean]],
  sequenceState: SequenceState,
  requests:      ObservationRequests,
  instrument:    Instrument
) extends ReactFnProps(SeqControlButtons):
  val isLoading: Boolean             = props.loadedObsId.exists(_.isPending)
  // val isReady: Boolean               = props.loadedObsId.exists(_.isReady)
  val isUserStopRequested: Boolean   = sequenceState.isUserStopRequested
  val isPauseInFlight: Boolean       = requests.pause === OperationRequest.InFlight
  val isCancelPauseInFlight: Boolean = requests.cancelPause === OperationRequest.InFlight
  val isRunning: Boolean             = sequenceState.isRunning
  val isWaitingUserPrompt: Boolean   = sequenceState.isWaitingUserPrompt
  val isRefreshing: Boolean          = props.refreshing.exists(_.get)
  val isCompleted: Boolean           = sequenceState.isCompleted

object SeqControlButtons
    extends ReactFnComponent[SeqControlButtons](props =>
      val tooltipOptions =
        TooltipOptions(position = Tooltip.Position.Top, showDelay = 100)

      for
        ctx         <- useContext(AppContext.ctx)
        sequenceApi <- useContext(SequenceApi.ctx)
      yield
        import ctx.given

        // TODO Remove this logic? Do we ever display sequences that are not loaded now?
        val selectedObsIsLoaded: Boolean = props.loadedObsId.contains_(props.obsId.ready)

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
            icon = Icons.Play.withFixedWidth().withSize(IconSize.LG),
            tooltip = "Start/Resume sequence",
            tooltipOptions = tooltipOptions,
            onClick = sequenceApi.start(props.obsId, RunOverride.Override).runAsync,
            disabled = props.isLoading || props.isRefreshing || props.isCompleted
          ).when(selectedObsIsLoaded && !props.isRunning),
          Button(
            clazz = ObserveStyles.PauseButton |+| ObserveStyles.ObsSummaryButton,
            icon = Icons.Pause.withFixedWidth().withSize(IconSize.LG),
            tooltip = "Pause sequence after current exposure",
            tooltipOptions = tooltipOptions,
            onClick = sequenceApi.pause(props.obsId).runAsync,
            disabled = props.isPauseInFlight || props.isWaitingUserPrompt
          ).when(selectedObsIsLoaded && props.isRunning && !props.isUserStopRequested),
          Button(
            clazz = ObserveStyles.CancelPauseButton |+| ObserveStyles.ObsSummaryButton,
            icon = Icons.CancelPause.withFixedWidth().withSize(IconSize.LG),
            tooltip = "Cancel process to pause the sequence",
            tooltipOptions = tooltipOptions,
            onClick = sequenceApi.cancelPause(props.obsId).runAsync,
            disabled = props.isCancelPauseInFlight || props.isWaitingUserPrompt
          ).when(selectedObsIsLoaded && props.isRunning && props.isUserStopRequested)
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

// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.components.sequence.steps

import crystal.react.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Instrument
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Step
import lucuma.react.common.*
import lucuma.react.fa.IconSize
import lucuma.react.primereact.*
import lucuma.typed.primereact.components.ButtonGroup
import lucuma.ui.sequence.SequenceIcons
import observe.model.SequenceStatus
import observe.model.operations.*
import observe.model.operations.Operations.*
import observe.ui.Icons
import observe.ui.ObserveStyles
import observe.ui.components.DefaultTooltipOptions
import observe.ui.model.AppContext
import observe.ui.model.ObservationRequests
import observe.ui.services.SequenceApi

/**
 * Contains a set of control buttons like stop/abort
 */
case class ExposureControlButtons(
  obsId:          Observation.Id,
  instrument:     Instrument,
  sequenceStatus: SequenceStatus,
  stepId:         Step.Id,
  isPausedInStep: Boolean,
  isExposure:     Boolean,
  isMultiLevel:   Boolean,
  requests:       ObservationRequests
) extends ReactFnProps(ExposureControlButtons):
  val operations: List[Operations] =
    instrument.operations(OperationLevel.Exposure, isPausedInStep, isMultiLevel)

  val isRunning: Boolean = sequenceStatus.isRunning

  // Only a stop/abort/pause-exposure already in progress (step interrupt requested) locks these
  // buttons. A pending "Hold sequence" request (sequence hold requested) lets the exposure run to
  // completion, so it must still be possible to stop or abort it.
  val isStepInterruptRequested: Boolean = sequenceStatus.isStepInterruptRequested

  val requestInFlight: Boolean = requests.stepRequestInFlight

object ExposureControlButtons
    extends ReactFnComponent[ExposureControlButtons](props =>
      for
        ctx         <- useContext(AppContext.ctx)
        sequenceApi <- useContext(SequenceApi.ctx)
      yield
        import ctx.given

        // val pauseGracefullyIcon: VdomNode =
        //   props.nsPendingObserveCmd match
        //     case Some(NodAndShuffleStep.PendingObserveCmd.PauseGracefully) => requestedIcon(Icons.Pause)
        //     case _                                                         => Icons.Pause

        // val stopGracefullyIcon: VdomNode =
        //   props.nsPendingObserveCmd match
        //     case Some(NodAndShuffleStep.PendingObserveCmd.StopGracefully) => requestedIcon(Icons.Stop)
        //     case _                                                        => Icons.Stop

        if (props.operations.nonEmpty)
          ButtonGroup(ObserveStyles.ControlButtonStrip)(
            // ObserveStyles.notInMobile,
            TagMod.when(props.isRunning):
              props.operations
                .map[VdomNode]:
                  case ResumeExposure =>
                    Button(
                      clazz = ObserveStyles.PlayButton,
                      icon = Icons.Play.withFixedWidth(),
                      tooltip = "Resume the current exposure",
                      tooltipOptions = DefaultTooltipOptions,
                      disabled = props.requestInFlight || !props.isPausedInStep,
                      onClickE =
                        _.stopPropagationCB >> sequenceApi.resumeExposure(props.obsId).runAsync
                    )
                  case PauseExposure  =>
                    Button(
                      clazz = ObserveStyles.PauseButton,
                      icon = Icons.Pause.withFixedWidth(),
                      tooltip = "Pause the current exposure",
                      tooltipOptions = DefaultTooltipOptions,
                      disabled =
                        props.requestInFlight || props.isPausedInStep || !props.isExposure || props.isStepInterruptRequested,
                      onClickE =
                        _.stopPropagationCB >> sequenceApi.pauseExposure(props.obsId).runAsync
                    )
                  case StopExposure   =>
                    Button(
                      clazz = ObserveStyles.StopButton,
                      icon = Icons.Stop.withFixedWidth().withSize(IconSize.LG),
                      tooltip = "Stop the current exposure early",
                      tooltipOptions = DefaultTooltipOptions,
                      disabled =
                        props.requestInFlight || !props.isExposure || props.isStepInterruptRequested,
                      onClickE =
                        _.stopPropagationCB >> sequenceApi.stopExposure(props.obsId).runAsync
                    )
                  case AbortExposure  =>
                    Button(
                      clazz = ObserveStyles.AbortButton,
                      icon = SequenceIcons.XMark.withFixedWidth().withSize(IconSize.LG),
                      tooltip = "Abort the current exposure",
                      tooltipOptions = DefaultTooltipOptions,
                      disabled =
                        props.requestInFlight || !props.isExposure || props.isStepInterruptRequested,
                      onClickE =
                        _.stopPropagationCB >> sequenceApi.abortExposure(props.obsId).runAsync
                    )
                  // // N&S operations
                  // case PauseExposureImmediately =>
                  //   Popup(
                  //     position = PopupPosition.TopRight,
                  //     trigger = Button(
                  //       icon = true,
                  //       color = Teal,
                  //       basic = true,
                  //       onClick = requestObsPause(p.obsId, p.stepId),
                  //       disabled = p.requestInFlight || p.isObservePaused || isReadingOut
                  //     )(IconPause)
                  //   )("Pause the current exposure immediately")
                  // case PauseExposureGracefully  =>
                  //   Popup(
                  //     position = PopupPosition.TopRight,
                  //     trigger = Button(
                  //       icon = true,
                  //       color = Teal,
                  //       onClick = requestGracefulObsPause(p.obsId, p.stepId),
                  //       disabled =
                  //         p.requestInFlight || p.isObservePaused || p.nsPendingObserveCmd.isDefined || isReadingOut
                  //     )(pauseGracefullyIcon)
                  //   )("Pause the current exposure at the end of the cycle")
                  // case StopExposureImmediately  =>
                  //   Popup(
                  //     position = PopupPosition.TopRight,
                  //     trigger = Button(
                  //       icon = true,
                  //       color = Orange,
                  //       basic = true,
                  //       onClick = requestStop(p.obsId, p.stepId),
                  //       disabled = p.requestInFlight || isReadingOut
                  //     )(IconStop)
                  //   )("Stop the current exposure immediately")
                  // case StopExposureGracefully   =>
                  //   Popup(
                  //     position = PopupPosition.TopRight,
                  //     trigger = Button(
                  //       icon = true,
                  //       color = Orange,
                  //       onClick = requestGracefulStop(p.obsId, p.stepId),
                  //       disabled =
                  //         p.requestInFlight || p.isObservePaused || p.nsPendingObserveCmd.isDefined || isReadingOut
                  //     )(stopGracefullyIcon)
                  //   )("Stop the current exposure at the end of the cycle")
                  case _              => EmptyVdom
                .toTagMod
          )
        else EmptyVdom
    )

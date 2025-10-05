// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.components.sequence

import cats.syntax.all.*
import crystal.*
import crystal.react.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Observation
import lucuma.core.model.ObservationReference
import lucuma.core.model.Program
import lucuma.react.common.*
import lucuma.react.fa.IconSize
import lucuma.react.primereact.Button
import lucuma.react.primereact.Tooltip
import lucuma.react.primereact.TooltipOptions
import lucuma.ui.LucumaIcons
import observe.model.*
import observe.ui.Icons
import observe.ui.ObserveStyles
import observe.ui.components.ConfigPanel
import observe.ui.model.ObsSummary
import observe.ui.model.ObservationRequests

case class ObsHeader(
  observation:      ObsSummary,
  loadedObsId:      Option[Pot[Observation.Id]],
  refreshing:       Pot[View[Boolean]],
  sequenceState:    SequenceState,
  requests:         ObservationRequests,
  overrides:        Option[View[SystemOverrides]],
  observer:         View[Option[Observer]],
  operator:         View[Option[Operator]],
  conditions:       View[Conditions],
  // openObsTable:     Callback,
  linkToExploreObs: Either[(Program.Id, Observation.Id), ObservationReference] => VdomNode
) extends ReactFnProps(ObsHeader)

object ObsHeader
    extends ReactFnComponent[ObsHeader](props =>
      <.div(ObserveStyles.ObsHeader)(
        <.div(ObserveStyles.ObsSummary)(
          <.div(ObserveStyles.ObsSummaryTitle)(
            SeqControlButtons(
              props.observation.obsId,
              props.loadedObsId,
              props.refreshing,
              props.sequenceState,
              props.requests,
              props.observation.instrument
            ),
            s"${props.observation.title} [${props.observation.refAndId}]",
            props.linkToExploreObs:
              props.observation.obsReference
                .toRight((props.observation.programId, props.observation.obsId))
          ),
          <.div(ObserveStyles.ObsSummaryDetails)(
            <.span(props.observation.instrumentConfigurationSummary),
            <.span(props.observation.constraintsSummary),
            props.overrides
              .map: overrides =>
                SubsystemOverrides(props.observation.obsId, props.observation.instrument, overrides)
                  .when(props.loadedObsId.contains_(props.observation.obsId.ready))
              .whenDefined
          )
        ),
        ConfigPanel(
          props.loadedObsId.flatMap(_.toOption),
          props.observer,
          props.operator,
          props.conditions
        )
        // <.div(ObserveStyles.ObsLoadSection)(
        //   Button(
        //     clazz = ObserveStyles.PlayButton |+| ObserveStyles.ObsSummaryButton,
        //     loading = props.loadedObsId.exists(_.isPending),
        //     icon = Icons.FileArrowUp.withFixedWidth().withSize(IconSize.LG),
        //     loadingIcon = LucumaIcons.CircleNotch.withFixedWidth().withSize(IconSize.LG),
        //     tooltip = "Load another sequence",
        //     tooltipOptions = TooltipOptions(position = Tooltip.Position.Top, showDelay = 100),
        //     onClick = props.openObsTable,
        //     disabled = !props.sequenceState.canUnload
        //   )
        // )
      )
    )

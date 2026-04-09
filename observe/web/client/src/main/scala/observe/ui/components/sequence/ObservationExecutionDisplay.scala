// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.components.sequence

import cats.syntax.all.*
import crystal.Pot
import crystal.react.*
import crystal.syntax.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Observation
import lucuma.core.model.ObservationReference
import lucuma.core.model.Program
import lucuma.react.common.*
import lucuma.schemas.model.ExecutionVisits
import lucuma.ui.DefaultErrorRender
import lucuma.ui.sequence.SelectedRowId
import lucuma.ui.sequence.SequenceData
import lucuma.ui.syntax.all.*
import observe.model.ExecutionState
import observe.model.SequenceStatus
import observe.model.StepProgress
import observe.model.Subsystem
import observe.model.enums.ControlStrategy
import observe.model.odb.RecordedVisit
import observe.ui.ObserveStyles
import observe.ui.components.ObservationSequence
import observe.ui.model.*

case class ObservationExecutionDisplay(
  selectedObs:                ObsSummary,
  rootModelData:              View[RootModelData],
  subsystemControlStrategies: Map[Subsystem, ControlStrategy],
  linkToExploreObs:           Either[(Program.Id, Observation.Id), ObservationReference] => VdomNode
) extends ReactFnProps(ObservationExecutionDisplay)

object ObservationExecutionDisplay
    extends ReactFnComponent[ObservationExecutionDisplay](props =>
      val selectedObsId: Observation.Id = props.selectedObs.obsId

      val rootModelData: RootModelData = props.rootModelData.get

      val executionStateOpt: ViewOpt[ExecutionState] =
        props.rootModelData
          .zoom(RootModelData.executionState.index(selectedObsId))

      val loadedObsViewPot: Pot[View[LoadedObservation]] =
        props.rootModelData
          .zoom:
            RootModelData.loadedObservations
              .andThen(LoadedObservations.Value)
              .index(selectedObsId)
              .andThen(Pot.readyPrism)
          .toOptionView
          .toPot

      val visitsViewPot: Pot[View[Option[ExecutionVisits]]] =
        loadedObsViewPot
          .flatMap: loView =>
            loView.zoom(LoadedObservation.visits).toPotView

      val executionStateAndConfig: Option[
        Pot[
          (Observation.Id, SequenceData, View[Option[ExecutionVisits]], View[ExecutionState])
        ]
      ] =
        rootModelData.loadedObservations.value
          .get(selectedObsId)
          .map: lo =>
            (lo.as(selectedObsId),
             lo.flatMap(_.sequenceData),
             visitsViewPot,
             executionStateOpt.toOptionView.toPot
            ).tupled

      val currentRecordedVisit: Option[RecordedVisit] =
        rootModelData.recordedIds.value.get(selectedObsId)

      <.div(ObserveStyles.ObservationArea, ^.key := selectedObsId.toString)(
        ObsHeader(
          props.selectedObs,
          executionStateAndConfig.map(_.map(_._1)),
          loadedObsViewPot.map(_.zoom(LoadedObservation.isRefreshing)),
          executionStateOpt.get.map(_.sequenceStatus).getOrElse(SequenceStatus.Idle),
          rootModelData.obsRequests.getOrElse(
            selectedObsId,
            ObservationRequests.Idle
          ),
          executionStateAndConfig
            .flatMap(_.toOption.map(_._4.zoom(ExecutionState.systemOverrides))),
          props.rootModelData.zoom(RootModelData.observer),
          props.rootModelData.zoom(RootModelData.operator),
          props.rootModelData.zoom(RootModelData.conditions),
          props.subsystemControlStrategies,
          props.linkToExploreObs
        ),
        executionStateAndConfig.map(
          _.renderPot(
            { (loadedObsId, sequenceData, visits, executionState) =>
              val progress: Option[StepProgress] =
                rootModelData.obsProgress.get(loadedObsId)

              val requests: ObservationRequests =
                rootModelData.obsRequests.getOrElse(loadedObsId, ObservationRequests.Idle)

              val selectedRowId: Option[SelectedRowId] =
                rootModelData.obsSelectedRow(loadedObsId)

              val setSelectedRowId: SelectedRowId => Callback = rowId =>
                props.rootModelData
                  .zoom(RootModelData.userSelectedRow.at(loadedObsId))
                  .mod: oldRowId =>
                    if (oldRowId.contains_(rowId)) none else rowId.some

              ObservationSequence(
                loadedObsId,
                sequenceData,
                visits,
                executionState,
                currentRecordedVisit,
                progress,
                requests,
                selectedRowId,
                setSelectedRowId,
                rootModelData.clientMode
              )
            },
            errorRender = t =>
              <.div(ObserveStyles.ObservationAreaError)(
                DefaultErrorRender(t)
              )
          )
        )
      )
    )

// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.components.obsList

import cats.effect.IO
import cats.syntax.all.given
import crystal.react.*
import japgolly.scalajs.react.*
import lucuma.react.common.*
import lucuma.react.primereact.Message
import lucuma.ui.syntax.all.*
import observe.model.Observation
import observe.ui.components.*
import observe.ui.model.AppContext
import observe.ui.model.RootModel
import observe.ui.model.RootModelData
import observe.ui.model.enums.AppTab
import observe.ui.model.reusability.given
import observe.ui.services.DaytimeQueueApi

final case class ObsListTab(rootModel: RootModel) extends ReactFnProps(ObsListTab)

private def queuedObsIds(rootModel: RootModel): Set[Observation.Id] =
  rootModel.data.get.daytimeQueue.queue.obsItems.map(_.obsId).toSet

object ObsListTab
    extends ReactFnComponent[ObsListTab](props =>
      for
        ctx         <- useContext(AppContext.ctx)
        sequenceApi <- useContext(observe.ui.services.SequenceApi.ctx)
        queueApi    <- useContext(DaytimeQueueApi.ctx)
        _           <- useEffectWithDeps(queuedObsIds(props.rootModel)): queued =>
                         val pendingAdds: View[Set[Observation.Id]] =
                           props.rootModel.data.zoom(RootModelData.pendingQueueAdds)
                         val added: Set[Observation.Id]             = pendingAdds.get.intersect(queued)
                         val toasts: Callback                       =
                           added.toList.foldLeft(Callback.empty): (shown, obsId) =>
                             shown >> ctx.toastRef.show(
                               s"$obsId added to the daytime queue",
                               Message.Severity.Success
                             )
                         Callback.when(added.nonEmpty)(toasts >> pendingAdds.mod(_ -- added))
      yield
        import ctx.given

        val rootModelDataView: View[RootModelData] = props.rootModel.data

        val rootModelData: RootModelData = rootModelDataView.get

        val loadObservation: Reusable[Observation.Id => Callback] =
          Reusable
            .implicitly(rootModelData.readyObservationsMap.keySet)
            .withValue: obsId =>
              rootModelData.readyObservationsMap
                .get(obsId)
                .foldMap: obsRow =>
                  rootModelDataView.mod(_.withLoadPendingObservation(obsId)) >>
                    sequenceApi.loadObservation(obsId, obsRow.instrument).runAsync

        val openQueue: Callback = ctx.pushPage(AppTab.DaytimeQueue)

        // Added from here, with no answer from the server yet.
        val pendingAdds: View[Set[Observation.Id]] =
          rootModelDataView.zoom(RootModelData.pendingQueueAdds)

        // A request that did not get there is not pending any longer.
        def add(obsId: Observation.Id): IO[Unit] =
          queueApi
            .add(obsId)
            .handleErrorWith(e => pendingAdds.mod(_ - obsId).to[IO] >> IO.raiseError(e))

        val queueObs: Observation.Id => Callback =
          obsId => pendingAdds.mod(_ + obsId) >> add(obsId).runAsync

        // The queue tab shows the outcome, so nothing is left to announce here.
        val queueAndOpenObs: Observation.Id => Callback =
          obsId =>
            pendingAdds.mod(_ + obsId) >>
              (add(obsId) >> pendingAdds.mod(_ - obsId).to[IO] >> openQueue.to[IO]).runAsync

        (rootModelData.obsList, props.rootModel.renderExploreLinkToObs).tupled
          .renderPot: (observations, renderExploreLinkToObs) =>
            ObsList(
              observations,
              rootModelData.executionState,
              rootModelData.loadedObservations,
              rootModelDataView.zoom(RootModelData.obsListGlobalFilter),
              rootModelDataView.zoom(RootModelData.obsListColumnFilters),
              loadObservation,
              queuedObsIds(props.rootModel),
              rootModelData.pendingQueueAdds,
              queueObs,
              queueAndOpenObs,
              openQueue,
              renderExploreLinkToObs
            )
    )

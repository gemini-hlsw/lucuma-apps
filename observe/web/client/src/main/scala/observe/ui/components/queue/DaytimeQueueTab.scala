// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.components.queue

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import fs2.Stream
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.util.Timestamp
import lucuma.react.common.*
import lucuma.react.primereact.ConfirmDialog
import lucuma.react.primereact.Message
import lucuma.ui.syntax.all.*
import observe.model.DaytimeQueue
import observe.model.DaytimeQueueView
import observe.model.QueueItemId
import observe.ui.ObserveStyles
import observe.ui.components.*
import observe.ui.model.AppContext
import observe.ui.model.DaytimeQueueRows
import observe.ui.model.RootModel
import observe.ui.model.RootModelData
import observe.ui.services.DaytimeQueueApi

import java.time.Instant
import java.time.ZoneId
import java.time.ZoneOffset
import scala.concurrent.duration.*

case class DaytimeQueueTab(rootModel: RootModel) extends ReactFnProps(DaytimeQueueTab)

object DaytimeQueueTab
    extends ReactFnComponent[DaytimeQueueTab](props =>
      val ClockPeriod: FiniteDuration = 1.second

      val currentTime: IO[Timestamp] =
        IO.realTime.map: sinceEpoch =>
          Timestamp.fromInstantTruncatedAndBounded(Instant.ofEpochMilli(sinceEpoch.toMillis))

      for
        ctx      <- useContext(AppContext.ctx)
        queueApi <- useContext(DaytimeQueueApi.ctx)
        nowPot   <- useStreamOnMount(
                      Stream.eval(currentTime) ++
                        Stream.awakeEvery[IO](ClockPeriod).evalMap(_ => currentTime)
                    )
      yield
        import ctx.given

        val queueView: View[DaytimeQueue] =
          props.rootModel.data.zoom(RootModelData.daytimeQueue.andThen(DaytimeQueueView.queue))

        val move: (QueueItemId, Int) => Callback =
          (id, toIndex) =>
            val before: DaytimeQueue = queueView.get
            val moved: DaytimeQueue  = before.move(id, toIndex).settlePauses
            if DaytimeQueueRows.pauseMoveHalts(before, id, toIndex) then
              ctx.toastRef.show(
                "A pause cannot go above everything that is left to run",
                Message.Severity.Warning
              )
            else
              // Moved here at once, so the row does not show in its old place meanwhile.
              queueView.set(moved) >>
                queueApi
                  .move(id, toIndex)
                  // The request did not get there: back, unless the server sent a newer queue.
                  .handleErrorWith: e =>
                    queueView.mod(q => if q === moved then before else q).to[IO] >>
                      IO.raiseError(e)
                  .runAsync

        val data: RootModelData    = props.rootModel.data.get
        val view: DaytimeQueueView = data.daytimeQueue
        val zone: ZoneId           =
          props.rootModel.clientConfig.toOption.fold[ZoneId](ZoneOffset.UTC)(_.site.timezone)

        nowPot.toOption.map: now =>
          <.div(ObserveStyles.DaytimeQueueTab)(
            DaytimeQueueToolbar(view.queue, DaytimeQueueRows.prediction(view.queue, now), zone),
            DaytimeQueueLamps(
              DaytimeQueueRows.lampsOffIn(view.queue, now),
              view.queue.lampsOffError
            ),
            DaytimeQueueTable(
              DaytimeQueueRows.rows(view, data.readyObservationsMap, now),
              data.executionState.keySet,
              data.executionState
                .collect:
                  case (obsId, state) if state.sequenceStatus.isSequenceHoldRequested => obsId
                .toSet,
              props.rootModel.renderExploreLinkToObs.toOption,
              move
            ),
            ConfirmDialog()
          )
    )

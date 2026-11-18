// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.services

import cats.effect.IO
import lucuma.core.model.Observation
import observe.model.Observer
import observe.model.QueueItemId
import observe.model.enums.QueueOrderingMode
import org.http4s.Uri

case class DaytimeQueueApiImpl(client: ApiClient, observer: Observer) extends DaytimeQueueApi[IO]:
  private val Root: Uri.Path = Uri.Path.empty / "queue"

  private val clientId: String = client.clientId.value.toString

  private def itemPath(id: QueueItemId): Uri.Path =
    id match
      case QueueItemId.Obs(obsId)     => Uri.Path.empty / "obs" / obsId.toString
      case QueueItemId.Pause(pauseId) => Uri.Path.empty / "pause" / pauseId.value.toString

  private def obsPath(obsId: Observation.Id): Uri.Path =
    Root / "obs" / obsId.toString / clientId

  override def add(obsId: Observation.Id): IO[Unit] =
    client.postNoData(Root / "add" / obsId.toString / clientId)

  override def remove(id: QueueItemId): IO[Unit] =
    client.postNoData((Root / "remove").concat(itemPath(id)) / clientId)

  override def move(id: QueueItemId, toIndex: Int): IO[Unit] =
    client.postNoData((Root / "move").concat(itemPath(id)) / toIndex.toString / clientId)

  override def addPause(atIndex: Int): IO[Unit] =
    client.postNoData(Root / "pause" / atIndex.toString / clientId)

  override def continuePastPause: IO[Unit] =
    client.postNoData(Root / "continue" / clientId)

  override def setOrderingMode(mode: QueueOrderingMode): IO[Unit] =
    client.postNoData(Root / "orderingMode" / mode.tag / clientId)

  override def run: IO[Unit] =
    client.postNoData(Root / "run" / clientId / observer.toString)

  override def holdAfterCurrentSteps: IO[Unit] =
    client.postNoData(Root / "holdAfterSteps" / clientId)

  override def holdAfterCurrentObservations: IO[Unit] =
    client.postNoData(Root / "holdAfterObservations" / clientId)

  override def abortAll: IO[Unit] =
    client.postNoData(Root / "abortAll" / clientId / observer.toString)

  override def runObs(obsId: Observation.Id): IO[Unit] =
    client.postNoData(obsPath(obsId) / "run" / observer.toString)

  override def abortObs(obsId: Observation.Id): IO[Unit] =
    client.postNoData(obsPath(obsId) / "abort" / observer.toString)

  override def retryObs(obsId: Observation.Id): IO[Unit] =
    client.postNoData(obsPath(obsId) / "retry")

  override def clearCompleted: IO[Unit] =
    client.postNoData(Root / "clearCompleted" / clientId)

  override def cancelLampsOff: IO[Unit] =
    client.postNoData(Root / "lampsOff" / "cancel" / clientId)

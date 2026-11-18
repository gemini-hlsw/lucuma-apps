// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.services

import cats.MonadThrow
import cats.effect.IO
import japgolly.scalajs.react.React
import japgolly.scalajs.react.feature.Context
import lucuma.core.model.Observation
import observe.model.QueueItemId
import observe.model.enums.QueueOrderingMode

import scala.annotation.unused

trait DaytimeQueueApi[F[_]: MonadThrow]:
  /** Add an observation at the end of the queue. It is not loaded. */
  def add(@unused obsId: Observation.Id): F[Unit] = NotAuthorized

  /** Remove an item that is not running */
  def remove(@unused id: QueueItemId): F[Unit] = NotAuthorized

  /** Move an item that is not running, so that it ends at the given index */
  def move(@unused id: QueueItemId, @unused toIndex: Int): F[Unit] = NotAuthorized

  /** Insert a pause at the given index */
  def addPause(@unused atIndex: Int): F[Unit] = NotAuthorized

  /** Remove the first pause */
  def continuePastPause: F[Unit] = NotAuthorized

  def setOrderingMode(@unused mode: QueueOrderingMode): F[Unit] = NotAuthorized

  /** Start the dispatcher */
  def run: F[Unit] = NotAuthorized

  /** Hold every running observation after its current step, and halt the dispatcher */
  def holdAfterCurrentSteps: F[Unit] = NotAuthorized

  /** Let the running observations finish, and halt the dispatcher */
  def holdAfterCurrentObservations: F[Unit] = NotAuthorized

  /** Abort every running observation, and halt the dispatcher */
  def abortAll: F[Unit] = NotAuthorized

  /** Start one observation now, whatever its position */
  def runObs(@unused obsId: Observation.Id): F[Unit] = NotAuthorized

  def abortObs(@unused obsId: Observation.Id): F[Unit] = NotAuthorized

  /** Set a failed or aborted observation back to waiting */
  def retryObs(@unused obsId: Observation.Id): F[Unit] = NotAuthorized

  /** Remove every completed observation */
  def clearCompleted: F[Unit] = NotAuthorized

  /** Cancel the countdown that turns the GCAL lamps off */
  def cancelLampsOff: F[Unit] = NotAuthorized

object DaytimeQueueApi:
  // Default value is NotAuthorized implementations
  val ctx: Context[DaytimeQueueApi[IO]] = React.createContext(new DaytimeQueueApi[IO] {})

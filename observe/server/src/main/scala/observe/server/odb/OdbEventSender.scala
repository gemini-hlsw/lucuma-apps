// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.odb

import cats.effect.Concurrent
import cats.effect.Deferred
import cats.effect.Ref
import cats.effect.Resource
import cats.effect.std.Supervisor
import cats.effect.syntax.all.*
import cats.syntax.all.*
import lucuma.core.model.Observation
import org.typelevel.log4cats.Logger

/**
 * Sends ODB events in the background, so that the sequence doesn't block waiting for each
 * acknowledgement.
 *
 * Every event carries the time it happened and the ODB accepts them in any order, so they all go
 * out concurrently: a step's worth of events costs one round trip instead of one per event.
 *
 * Send failures are held until the next `flush` for that observation, where they are raised.
 */
trait OdbEventSender[F[_]] private[odb] ():

  /** Submits an event to be sent in the background. Returns as soon as it is queued. */
  def submit(obsId: Observation.Id, description: String, send: F[Unit]): F[Unit]

  /**
   * Awaits acknowledgement of every event submitted so far for the given observation, raising the
   * first failure among them, if any.
   */
  def flush(obsId: Observation.Id): F[Unit]

object OdbEventSender:

  /**
   * The sends in flight for one observation. `idle` completes when `outstanding` reaches zero, and
   * a fresh one is installed whenever the observation goes from idle back to busy.
   */
  private case class ObsState[F[_]](
    outstanding: Int,
    idle:        Deferred[F, Unit],
    failure:     Option[Throwable]
  )

  private class Impl[F[_]: Logger](
    supervisor: Supervisor[F],
    obsStates:  Ref[F, Map[Observation.Id, ObsState[F]]]
  )(using F: Concurrent[F])
      extends OdbEventSender[F]:

    override def submit(obsId: Observation.Id, description: String, send: F[Unit]): F[Unit] =
      Deferred[F, Unit].flatMap: freshIdle =>
        obsStates.update { states =>
          val current: Option[ObsState[F]] = states.get(obsId)
          states.updated(
            obsId,
            ObsState(
              current.foldMap(_.outstanding) + 1,
              current.filter(_.outstanding > 0).fold(freshIdle)(_.idle),
              current.flatMap(_.failure)
            )
          )
        } >>
          supervisor
            .supervise:
              send
                .handleErrorWith(recordFailure(obsId, description))
                .guarantee(sendSettled(obsId))
            .void

    private def recordFailure(obsId: Observation.Id, description: String)(t: Throwable): F[Unit] =
      Logger[F].error(t)(s"Error sending ODB event $description for obsId: $obsId") >>
        obsStates.update:
          _.updatedWith(obsId)(_.map(s => s.copy(failure = s.failure.orElse(t.some))))

    private def sendSettled(obsId: Observation.Id): F[Unit] =
      obsStates
        .modify: states =>
          states
            .get(obsId)
            .fold((states, none[Deferred[F, Unit]])): s =>
              val remaining: Int = s.outstanding - 1
              (
                states.updated(obsId, s.copy(outstanding = remaining)),
                Option.when(remaining <= 0)(s.idle)
              )
        .flatMap(_.traverse_(_.complete(()).void))

    override def flush(obsId: Observation.Id): F[Unit] =
      obsStates.get
        .flatMap(_.get(obsId).filter(_.outstanding > 0).fold(F.unit)(_.idle.get)) >>
        obsStates
          .modify: states =>
            (
              states.updatedWith(obsId):
                _.flatMap(s => Option.when(s.outstanding > 0)(s.copy(failure = none)))
              ,
              states.get(obsId).flatMap(_.failure)
            )
          .flatMap(_.fold(F.unit)(F.raiseError))

  def apply[F[_]: {Concurrent, Logger}]: Resource[F, OdbEventSender[F]] =
    for
      // `await = true` so that pending events are still sent when the server shuts down.
      supervisor <- Supervisor[F](await = true)
      obsStates  <- Resource.eval(Ref.of[F, Map[Observation.Id, ObsState[F]]](Map.empty))
    yield Impl(supervisor, obsStates)

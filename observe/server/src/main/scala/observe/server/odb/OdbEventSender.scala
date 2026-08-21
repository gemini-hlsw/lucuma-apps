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
 * Events submitted for the same observation are sent in submission order; different observations
 * proceed independently. A failure is held until the next `flush` for that observation, where it is
 * raised.
 */
trait OdbEventSender[F[_]] private[odb] ():

  /** Submits an event to be sent in the background. Returns as soon as the event is queued. */
  def submit(obsId: Observation.Id, description: String, send: F[Unit]): F[Unit]

  /**
   * Awaits acknowledgement of every event submitted so far for the given observation, raising the
   * first failure among them, if any.
   */
  def flush(obsId: Observation.Id): F[Unit]

object OdbEventSender:

  /**
   * State of the events in flight for one observation. `sent` completes once the last event
   * submitted has been acknowledged; since each event awaits its predecessor, that implies all the
   * previous ones were too.
   */
  private case class Pending[F[_]](outstanding: Int, sent: F[Unit], failure: Option[Throwable])

  private class Impl[F[_]: Logger](
    supervisor: Supervisor[F],
    state:      Ref[F, Map[Observation.Id, Pending[F]]]
  )(using F: Concurrent[F])
      extends OdbEventSender[F]:

    override def submit(obsId: Observation.Id, description: String, send: F[Unit]): F[Unit] =
      Deferred[F, Unit].flatMap: sent =>
        state
          .modify: pendings =>
            val previous: Option[Pending[F]] = pendings.get(obsId)
            (
              pendings.updated(
                obsId,
                Pending(previous.foldMap(_.outstanding) + 1, sent.get, previous.flatMap(_.failure))
              ),
              previous.fold(F.unit)(_.sent)
            )
          .flatMap: awaitPrevious =>
            supervisor
              .supervise:
                (awaitPrevious >> send.handleErrorWith(recordFailure(obsId, description)))
                  .guarantee(sent.complete(()) >> eventSettled(obsId))
              .void

    private def recordFailure(obsId: Observation.Id, description: String)(t: Throwable): F[Unit] =
      Logger[F].error(t)(s"Error sending ODB event $description for obsId: $obsId") >>
        state.update:
          _.updatedWith(obsId)(_.map(p => p.copy(failure = p.failure.orElse(t.some))))

    // Drops the observation's entry once nothing is in flight and there's no failure left to report.
    private def eventSettled(obsId: Observation.Id): F[Unit] =
      state.update:
        _.updatedWith(obsId):
          _.flatMap: p =>
            val remaining: Int = p.outstanding - 1
            Option.when(remaining > 0 || p.failure.isDefined)(p.copy(outstanding = remaining))

    override def flush(obsId: Observation.Id): F[Unit] =
      state.get.flatMap(_.get(obsId).fold(F.unit)(_.sent)) >>
        state
          .modify: pendings =>
            (
              pendings.updatedWith(obsId):
                _.flatMap(p => Option.when(p.outstanding > 0)(p.copy(failure = none)))
              ,
              pendings.get(obsId).flatMap(_.failure)
            )
          .flatMap(_.fold(F.unit)(F.raiseError))

  def apply[F[_]: {Concurrent, Logger}]: Resource[F, OdbEventSender[F]] =
    for
      // `await = true` so that pending events are still sent when the server shuts down.
      supervisor <- Supervisor[F](await = true)
      state      <- Resource.eval(Ref.of[F, Map[Observation.Id, Pending[F]]](Map.empty))
    yield Impl(supervisor, state)

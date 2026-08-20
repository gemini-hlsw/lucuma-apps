// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.odb

import cats.Endo
import cats.effect.Async
import cats.effect.Ref
import cats.effect.std.Mutex
import cats.effect.std.UUIDGen
import cats.syntax.all.*
import clue.FetchClientWithPars
import clue.ResponseException
import clue.syntax.*
import lucuma.core.model.Observation
import lucuma.core.util.IdempotencyKey
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.AddEventBatchEntryInput
import lucuma.schemas.ObservationDB.Types.AddEventBatchInput
import observe.common.EventsGQL.AddEventBatchMutation
import org.http4s.Request
import org.http4s.headers.`Idempotency-Key`
import org.typelevel.log4cats.Logger
import retry.*

import scala.concurrent.duration.*

type PendingEvents = Map[Observation.Id, Vector[AddEventBatchEntryInput]]

/**
 * Local buffer of execution events awaiting transmission as one atomic `addEventBatch`. Buffered
 * events are not yet part of the ODB's permanent record: every entry must carry its own
 * `clientTime` and `idempotencyKey`, captured at emission.
 */
trait OdbEventBufferOps[F[_]: {Logger as L, Async, UUIDGen as U}](
  buffer:     Ref[F, PendingEvents],
  flushMutex: Mutex[F]
)(using FetchClientWithPars[F, Request[F], ObservationDB]):

  private val FlushRetryPolicy: RetryPolicy[F, Throwable] =
    RetryPolicies
      .limitRetries[F](4)
      .join(RetryPolicies.capDelay(30.seconds, RetryPolicies.exponentialBackoff[F](1.second)))

  protected def appendEvent(obsId: Observation.Id, entry: AddEventBatchEntryInput): F[Unit] =
    buffer.update(b => b.updated(obsId, b.getOrElse(obsId, Vector.empty) :+ entry))

  protected def pendingEventCount(obsId: Observation.Id): F[Int] =
    buffer.get.map(_.get(obsId).foldMap(_.size))

  /**
   * Flush the observation's buffered events as one atomic batch. The mutex serializes flushes so
   * batches reach the ODB in the order their events were taken from the buffer.
   */
  protected def flushEvents(obsId: Observation.Id): F[Unit] =
    flushMutex.lock.surround:
      buffer
        .modify(b => (b - obsId, b.getOrElse(obsId, Vector.empty)))
        .flatMap: events =>
          sendBatch(obsId, events).whenA(events.nonEmpty)

  /** Best-effort flush of every observation's pending events, for graceful shutdown. */
  protected def flushAllPendingEvents: F[Unit] =
    flushMutex.lock.surround:
      buffer
        .getAndSet(Map.empty)
        .flatMap:
          _.toList.traverse_ : (obsId, events) =>
            AddEventBatchMutation[F]
              .execute(AddEventBatchInput(events.toList))
              .raiseGraphQLErrors
              .void
              .handleErrorWith: e =>
                L.error(e)(
                  s"Failed to flush ${events.size} pending ODB events for [$obsId] on shutdown"
                )

  private def sendBatch(obsId: Observation.Id, events: Vector[AddEventBatchEntryInput]): F[Unit] =
    U.randomUUID
      .map(IdempotencyKey(_))
      .flatMap: batchKey =>
        retryingOnErrors(
          AddEventBatchMutation[F]
            .execute(AddEventBatchInput(events.toList), batchIdempotencyKey(batchKey))
            .raiseGraphQLErrors
        )(
          policy = FlushRetryPolicy,
          errorHandler = ResultHandler.retryOnSomeErrors(
            {
              case _: ResponseException[?] => false // GraphQL rejection: deterministic, don't retry
              case _                       => true  // transport failure: retry with same payload
            },
            (e, details) =>
              L.warn(e)(
                s"ODB event batch flush for [$obsId] failed (${details.retriesSoFar} retries so far)"
              )
          )
        )
      .flatMap: result =>
        L.warn(s"ODB event batch response for [$obsId] was truncated (hasMore)")
          .whenA(result.addEventBatch.hasMore)
      .onError:
        case e: ResponseException[?] =>
          L.error(e)(s"ODB rejected event batch for [$obsId]; dropping ${events.size} events")
        case e                       =>
          L.error(e)(
            s"ODB event batch flush for [$obsId] exhausted retries; re-buffering ${events.size} events"
          ) *>
            buffer.update(b => b.updated(obsId, events ++ b.getOrElse(obsId, Vector.empty)))
      .void

  private def batchIdempotencyKey(key: IdempotencyKey): Endo[Request[F]] = req =>
    req.putHeaders(`Idempotency-Key`(key.toString))

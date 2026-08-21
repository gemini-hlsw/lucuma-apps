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
import lucuma.core.model.sequence.Step
import org.typelevel.log4cats.Logger

/**
 * Sends ODB events in the background, so that the sequence doesn't block waiting for each
 * acknowledgement.
 *
 * Every event carries the time it happened and the ODB accepts them in any order, so step and
 * sequence events go out concurrently: a burst costs one round trip instead of one per event. Two
 * constraints are enforced on top of that, both found by exercising the dev ODB:
 *
 *   - Dataset events are serialized per observation. Concurrent `addDatasetEvent` calls against the
 *     same dataset make the ODB return HTTP 500 (contention on the dataset row, roughly 2 in 5 with
 *     five in flight), and Ember only retries dead-pool connection errors, not 500s. Since an
 *     observation exposes one dataset at a time, serializing per observation costs nothing.
 *   - The ODB refuses to record a dataset for a step it has no event for yet, which
 *     `awaitStepRecorded` covers without waiting on anything else in flight.
 *
 * Send failures are held until the next `flush` for that observation, where they are raised.
 */
trait OdbEventSender[F[_]] private[odb] ():

  /** Submits an event to be sent in the background. Returns as soon as it is queued. */
  def submit(obsId: Observation.Id, description: String, send: F[Unit]): F[Unit]

  /** As `submit`, and additionally releases `awaitStepRecorded` for the step. */
  def submitStepEvent(
    obsId:       Observation.Id,
    stepId:      Step.Id,
    description: String,
    send:        F[Unit]
  ): F[Unit]

  /** As `submit`, but sent only once the observation's previous dataset event has been sent. */
  def submitDatasetEvent(obsId: Observation.Id, description: String, send: F[Unit]): F[Unit]

  /**
   * Waits until a step event for this step has been acknowledged, which is what the ODB requires
   * before it will record a dataset for it. Returns as soon as one has, whether it succeeded or
   * not: a failure is reported by `flush`, and letting the dataset attempt go ahead yields a real
   * error rather than a hang. Also returns immediately if no step event was ever submitted, for the
   * same reason.
   */
  def awaitStepRecorded(obsId: Observation.Id, stepId: Step.Id): F[Unit]

  /** Discards the bookkeeping of a step that has finished. */
  def forgetStep(obsId: Observation.Id, stepId: Step.Id): F[Unit]

  /**
   * Awaits acknowledgement of every event submitted so far for the given observation, raising the
   * first failure among them, if any.
   */
  def flush(obsId: Observation.Id): F[Unit]

object OdbEventSender:

  /**
   * The sends in flight for one observation. `idle` completes when `outstanding` reaches zero, and
   * a fresh one is installed whenever the observation goes from idle back to busy. `datasetChain`
   * completes when the last dataset event submitted has been sent, which is what the next one waits
   * on.
   */
  private case class ObsState[F[_]](
    outstanding:  Int,
    idle:         Deferred[F, Unit],
    datasetChain: F[Unit],
    failure:      Option[Throwable]
  )

  private type StepKey = (Observation.Id, Step.Id)

  private class Impl[F[_]: Logger](
    supervisor:   Supervisor[F],
    obsStates:    Ref[F, Map[Observation.Id, ObsState[F]]],
    stepRecorded: Ref[F, Map[StepKey, Deferred[F, Unit]]]
  )(using F: Concurrent[F])
      extends OdbEventSender[F]:

    override def submit(obsId: Observation.Id, description: String, send: F[Unit]): F[Unit] =
      start(obsId, description, send, none, serialized = false)

    override def submitStepEvent(
      obsId:       Observation.Id,
      stepId:      Step.Id,
      description: String,
      send:        F[Unit]
    ): F[Unit] =
      stepMarker(obsId, stepId).flatMap: marker =>
        start(obsId, description, send, marker.some, serialized = false)

    override def submitDatasetEvent(
      obsId:       Observation.Id,
      description: String,
      send:        F[Unit]
    ): F[Unit] =
      start(obsId, description, send, none, serialized = true)

    private def start(
      obsId:       Observation.Id,
      description: String,
      send:        F[Unit],
      recorded:    Option[Deferred[F, Unit]],
      serialized:  Boolean
    ): F[Unit] =
      (Deferred[F, Unit], Deferred[F, Unit]).tupled.flatMap: (freshIdle, thisSent) =>
        obsStates
          .modify: states =>
            val current: Option[ObsState[F]] = states.get(obsId)
            val previousChain: F[Unit]       = current.fold(F.unit)(_.datasetChain)
            (
              states.updated(
                obsId,
                ObsState(
                  current.foldMap(_.outstanding) + 1,
                  current.filter(_.outstanding > 0).fold(freshIdle)(_.idle),
                  if (serialized) thisSent.get else previousChain,
                  current.flatMap(_.failure)
                )
              ),
              if (serialized) previousChain else F.unit
            )
          .flatMap: awaitPrevious =>
            supervisor
              .supervise:
                (awaitPrevious >> send.handleErrorWith(recordFailure(obsId, description)))
                  .guarantee:
                    thisSent.complete(()).void >>
                      recorded.traverse_(_.complete(()).void) >>
                      sendSettled(obsId)
              .void

    /** The marker for a step, created by whichever step event is submitted first. */
    private def stepMarker(obsId: Observation.Id, stepId: Step.Id): F[Deferred[F, Unit]] =
      Deferred[F, Unit].flatMap: fresh =>
        stepRecorded.modify: markers =>
          markers
            .get((obsId, stepId))
            .fold((markers.updated((obsId, stepId), fresh), fresh))(existing => (markers, existing))

    override def awaitStepRecorded(obsId: Observation.Id, stepId: Step.Id): F[Unit] =
      stepRecorded.get.flatMap(_.get((obsId, stepId)).fold(F.unit)(_.get))

    override def forgetStep(obsId: Observation.Id, stepId: Step.Id): F[Unit] =
      stepRecorded.update(_ - ((obsId, stepId)))

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
      supervisor   <- Supervisor[F](await = true)
      obsStates    <- Resource.eval(Ref.of[F, Map[Observation.Id, ObsState[F]]](Map.empty))
      stepRecorded <- Resource.eval(Ref.of[F, Map[StepKey, Deferred[F, Unit]]](Map.empty))
    yield Impl(supervisor, obsStates, stepRecorded)

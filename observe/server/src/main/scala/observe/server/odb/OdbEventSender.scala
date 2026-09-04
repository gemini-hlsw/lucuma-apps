// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
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
 * Every event carries the time it happened and the ODB accepts them in any order, so they all go
 * out concurrently: a step's worth of events costs one round trip instead of one per event.
 *
 * The one ordering the ODB imposes is that a step must have had an event recorded before it will
 * record a dataset for it. Waiting for that here, rather than having the ODB create the step's
 * execution row itself, keeps the dataset path out of the observation-execution mutex and away from
 * the deadlock class the ODB fought through V1212/V1217/V1227.
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
   * a fresh one is installed whenever the observation goes from idle back to busy.
   */
  private case class ObsState[F[_]](
    outstanding: Int,
    idle:        Deferred[F, Unit],
    failure:     Option[Throwable]
  )

  private type StepKey = (Observation.Id, Step.Id)

  private class Impl[F[_]: Logger](
    supervisor:   Supervisor[F],
    obsStates:    Ref[F, Map[Observation.Id, ObsState[F]]],
    stepRecorded: Ref[F, Map[StepKey, Deferred[F, Unit]]]
  )(using F: Concurrent[F])
      extends OdbEventSender[F]:

    override def submit(obsId: Observation.Id, description: String, send: F[Unit]): F[Unit] =
      start(obsId, description, send, none)

    override def submitStepEvent(
      obsId:       Observation.Id,
      stepId:      Step.Id,
      description: String,
      send:        F[Unit]
    ): F[Unit] =
      stepMarker(obsId, stepId).flatMap(marker => start(obsId, description, send, marker.some))

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

    private def start(
      obsId:       Observation.Id,
      description: String,
      send:        F[Unit],
      recorded:    Option[Deferred[F, Unit]]
    ): F[Unit] =
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
                .guarantee(recorded.traverse_(_.complete(()).void) >> sendSettled(obsId))
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
      supervisor   <- Supervisor[F](await = true)
      obsStates    <- Resource.eval(Ref.of[F, Map[Observation.Id, ObsState[F]]](Map.empty))
      stepRecorded <- Resource.eval(Ref.of[F, Map[StepKey, Deferred[F, Unit]]](Map.empty))
    yield Impl(supervisor, obsStates, stepRecorded)

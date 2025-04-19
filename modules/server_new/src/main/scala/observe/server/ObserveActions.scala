// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import cats.*
import cats.effect.Concurrent
import cats.effect.Temporal
import cats.syntax.all.*
import fs2.Stream
import lucuma.core.util.TimeSpan
import observe.common.ObsQueriesGQL.RecordDatasetMutation.Data.RecordDataset.Dataset
import observe.engine.*
import observe.model.Observation
import observe.model.dhs.*
import observe.model.enums.ObserveCommandResult
import observe.server.InstrumentSystem.*
import org.typelevel.log4cats.Logger

import odb.OdbProxy

/**
 * Methods usedd to generate observation related actions
 */
trait ObserveActions {

  private def info[F[_]: Logger](msg: => String): F[Unit] = Logger[F].info(msg)

  /**
   * Actions to perform when an observe is aborted
   */
  def abortTail[F[_]: MonadThrow](
    odb:         OdbProxy[F],
    obsId:       Observation.Id,
    imageFileId: ImageFileId
  ): F[Result] =
    odb
      .stepAbort(obsId)
      .ensure(
        ObserveFailure.Unexpected("Unable to send ObservationAborted message to ODB.")
      )(identity)
      // TODO IS it OK to ignore the Boolean return value?
      .as(Result.OKAborted(Response.Aborted(imageFileId)))

  /**
   * Send the datasetStart command to the odb
   */
  private def sendDataStart[F[_]: MonadThrow](
    odb:    OdbProxy[F],
    obsId:  Observation.Id,
    fileId: ImageFileId
  ): F[Dataset] =
    odb
      .datasetStartExposure(obsId, fileId)

  /**
   * Send the datasetEnd command to the odb
   */
  private def sendDataEnd[F[_]: MonadThrow](
    odb:    OdbProxy[F],
    obsId:  Observation.Id,
    fileId: ImageFileId
  ): F[Unit] =
    // TODO: link dataset events to instrument statuses
    odb
      .datasetEndExposure(obsId, fileId)
      .ensure(
        ObserveFailure.Unexpected("Unable to send datasetEndExposure message to ODB.")
      )(identity) >>
      odb
        .datasetStartReadout(obsId, fileId)
        .ensure(
          ObserveFailure.Unexpected("Unable to send datasetStartReadout message to ODB.")
        )(identity) >>
      odb
        .datasetEndReadout(obsId, fileId)
        .ensure(
          ObserveFailure.Unexpected("Unable to send datasetEndReadout message to ODB.")
        )(identity) >>
      odb
        .datasetStartWrite(obsId, fileId)
        .ensure(
          ObserveFailure.Unexpected("Unable to send datasetStartWrite message to ODB.")
        )(identity) >>
      odb
        .datasetEndWrite(obsId, fileId)
        .ensure(
          ObserveFailure.Unexpected("Unable to send datasetEndWrite message to ODB.")
        )(identity)
        .void

  /**
   * Standard progress stream for an observation
   */
  def observationProgressStream[F[_]](
    env: ObserveEnvironment[F]
  ): Stream[F, Result] = env.inst
    .observeProgress(env.inst.calcObserveTime, ElapsedTime(TimeSpan.Zero))
    .map(Result.Partial(_))

  /**
   * Tell each subsystem that an observe will start
   */
  def notifyObserveStart[F[_]: Applicative](
    env: ObserveEnvironment[F]
  ): F[Unit] =
    env.otherSys.traverse_(_.notifyObserveStart)

  /**
   * Tell each subsystem that an observe will end Unlike observe start we also tell the instrumetn
   * about it
   */
  def notifyObserveEnd[F[_]: Applicative](env: ObserveEnvironment[F]): F[Unit] =
    (env.inst +: env.otherSys).traverse_(_.notifyObserveEnd)

  /**
   * Close the image, telling either DHS or GDS as it correspond
   */
  def closeImage[F[_]](id: ImageFileId, env: ObserveEnvironment[F]): F[Unit] =
    env.inst.keywordsClient.closeImage(id)

  /**
   * Preamble for observations. It tells the odb, the subsystems send the start headers and finally
   * sends an observe
   */
  def observePreamble[F[_]: Concurrent: Logger](
    fileId: ImageFileId,
    env:    ObserveEnvironment[F]
  ): F[ObserveCommandResult] =
    for {
      d <- sendDataStart(env.odb, env.obsId, fileId)
      _ <- notifyObserveStart(env)
      _ <- env.headers(env.ctx).traverse(_.sendBefore(env.obsId, fileId, d.reference))
      _ <-
        info(
          s"Start ${env.inst.resource.longName} observation ${env.obsId} with label $fileId"
        )
      r <- env.inst.observe(fileId)
      _ <-
        info(
          s"Completed ${env.inst.resource.longName} observation ${env.obsId} with label $fileId"
        )
    } yield r

  /**
   * End of an observation for a typical instrument It tells the odb and each subsystem and also
   * sends the end observation keywords
   */
  def okTail[F[_]: Concurrent](
    fileId:  ImageFileId,
    stopped: Boolean,
    env:     ObserveEnvironment[F]
  ): F[Result] =
    for {
      _ <- notifyObserveEnd(env)
      _ <- env.headers(env.ctx).reverseIterator.toList.traverse(_.sendAfter(fileId))
      _ <- closeImage(fileId, env)
      _ <- sendDataEnd(env.odb, env.obsId, fileId)
    } yield
      if (stopped) Result.OKStopped(Response.Observed(fileId))
      else Result.OK(Response.Observed(fileId))

  /**
   * Method to process observe results and act accordingly to the response
   */
  private def observeTail[F[_]: Temporal](
    fileId: ImageFileId,
    env:    ObserveEnvironment[F]
  )(r: ObserveCommandResult): Stream[F, Result] =
    Stream.eval(r match {
      case ObserveCommandResult.Success =>
        okTail(fileId, stopped = false, env)
      case ObserveCommandResult.Stopped =>
        okTail(fileId, stopped = true, env)
      case ObserveCommandResult.Aborted =>
        abortTail(env.odb, env.obsId, fileId)
      case ObserveCommandResult.Paused  =>
        val totalTime = env.inst.calcObserveTime
        env.inst.observeControl match {
          case c: CompleteControl[F] =>
            val resumePaused: TimeSpan => Stream[F, Result] =
              (remaining: TimeSpan) =>
                Stream
                  .eval(c.continue.self(remaining))
                  .flatMap(observeTail(fileId, env))
            val progress: ElapsedTime => Stream[F, Result]  =
              (elapsed: ElapsedTime) =>
                env.inst
                  .observeProgress(totalTime, elapsed)
                  .map(Result.Partial(_))
                  .widen[Result]
            val stopPaused: Stream[F, Result]               =
              Stream
                .eval {
                  c.stopPaused.self
                }
                .flatMap(observeTail(fileId, env))
            val abortPaused: Stream[F, Result]              =
              Stream
                .eval {
                  c.abortPaused.self
                }
                .flatMap(observeTail(fileId, env))

            Result
              .Paused(
                ObserveContext[F](
                  resumePaused,
                  progress,
                  stopPaused,
                  abortPaused,
                  totalTime
                )
              )
              .pure[F]
              .widen[Result]
          case _                     =>
            ObserveFailure
              .Execution("Observation paused for an instrument that does not support pause")
              .raiseError[F, Result]
        }
    })

  /**
   * Observe for a typical instrument
   */
  def stdObserve[F[_]: Temporal: Logger](
    fileId: ImageFileId,
    env:    ObserveEnvironment[F]
  ): Stream[F, Result] =
    for {
      result <- Stream.eval(observePreamble(fileId, env))
      ret    <- observeTail(fileId, env)(result)
    } yield ret

}

object ObserveActions extends ObserveActions

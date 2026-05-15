// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import cats.Applicative
import cats.effect.Sync
import cats.syntax.all.*
import edu.gemini.aspen.giapi.commands.HandlerResponse
import giapi.client.GiapiClient
import giapi.client.GiapiConfig
import giapi.client.commands.CommandCallResult
import giapi.client.commands.CommandResult
import giapi.client.commands.CommandResultException
import giapi.client.commands.Configuration
import lucuma.core.util.TimeSpan
import observe.model.dhs.ImageFileId
import observe.server.ObserveFailure
import observe.server.ObserveFailure.Execution
import observe.server.ObserveFailure.ObserveException
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.syntax.*

import scala.concurrent.duration.*

trait GiapiInstrumentController[F[_], CFG] {
  def applyConfig(config: CFG): F[Unit]
  def observe(fileId:     ImageFileId, expTime: TimeSpan): F[ImageFileId]
  def endObserve: F[Unit]
}

extension (ts: TimeSpan) {
  def toDuration: FiniteDuration =
    ts.toMicroseconds.microseconds
}

given GiapiConfig[ImageFileId] = _.value

/**
 * Superclass for all GIAPI instrument controllers.
 */
private[server] abstract class AbstractGiapiInstrumentController[F[_]: Sync, CFG, C <: GiapiClient[
  F
]](client: C, configureTimeout: TimeSpan)(using L: Logger[F])
    extends GiapiInstrumentController[F, CFG] {

  def name: String
  def configuration(config: CFG): F[Configuration]

  private def adaptGiapiError: PartialFunction[Throwable, ObserveFailure] = {
    // The GMP sends these cryptic messages but we can do better
    case CommandResultException(_, "Message cannot be null") =>
      Execution("Unhandled Apply command")
    case CommandResultException(_, m)                        => Execution(m)
    case f                                                   => ObserveException(f)
  }

  private def configure(configF: F[Configuration]): F[CommandCallResult] = configF
    .flatMap { cfg =>
      if (cfg.config.isEmpty)
        CommandResult(HandlerResponse.Response.ACCEPTED).pure[F].widen[CommandCallResult]
      else
        client.genericApply(cfg, configureTimeout.toDuration)
    }
    .adaptError(adaptGiapiError)

  override def applyConfig(config: CFG): F[Unit] =
    doApplyConfig(configuration(config))

  protected def doApplyConfig(confF: F[Configuration]): F[Unit] =
    debug"Start $name configuration" *>
      configure(confF) *>
      debug"Configuration for $name sent"

  override def observe(fileId: ImageFileId, expTime: TimeSpan): F[ImageFileId] = (
    debug"Send observe to $name, file id $fileId and $expTime" *>
      client.observe(fileId, expTime.toDuration) *>
      debug"Completed $name observe"
  )
    .as(fileId)
    .adaptError(adaptGiapiError)

  override def endObserve: F[Unit] =
    Applicative[F].unit
}


object GiapiInstrumentController {
  extension (c: Configuration) {
    def when(f: Configuration => Boolean): Configuration =
      if (f(c)) c else Configuration.Zero
  }
}

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observer.server

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
]](client: C, configureTimeout: TimeSpan)(implicit
  L: Logger[F]
) extends GiapiInstrumentController[F, CFG] {

  def name: String
  def configuration(config: CFG): F[Configuration]

  private def adaptGiapiError: PartialFunction[Throwable, ObserveFailure] = {
    // The GMP sends these cryptic messages but we can do better
    case CommandResultException(_, "Message cannot be null") =>
      Execution("Unhandled Apply command")
    case CommandResultException(_, m)                        => Execution(m)
    case f                                                   => ObserveException(f)
  }

  private def configure(config: CFG): F[CommandCallResult] = {
    val cfg: F[Configuration] = configuration(config)
    val isEmpty               = cfg.map(_.config.isEmpty)
    isEmpty.ifM(
      CommandResult(HandlerResponse.Response.ACCEPTED).pure[F].widen[CommandCallResult],
      cfg.flatMap(client.genericApply(_, configureTimeout.toDuration))
    )
  }.adaptError(adaptGiapiError)

  override def applyConfig(config: CFG): F[Unit] =
    L.debug(s"Start $name configuration") *>
      configure(config) *>
      L.debug(s"Configuration for $name sent")

  override def observe(fileId: ImageFileId, expTime: TimeSpan): F[ImageFileId] = (
    L.debug(s"Send observe to $name, file id $fileId and $expTime") *>
      client.observe(fileId, expTime.toDuration) *>
      L.debug(s"Completed $name observe")
  )
    .as(fileId)
    .adaptError(adaptGiapiError)

  override def endObserve: F[Unit] =
    Applicative[F].unit
}

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.ghost

import cats.Applicative
import cats.syntax.all.*
import lucuma.core.util.TimeSpan
import observe.model.CurrentConditions
import observe.model.dhs.ImageFileId
import observe.model.enums.ObserveCommandResult
import observe.server.keywords.GdsClient
import observe.server.overrideLogMessage
import org.typelevel.log4cats.Logger

class GhostControllerDisabled[F[_]: {Logger, Applicative}] extends GhostController[F] {
  private val name = "GHOST"

  override def gdsClient: GdsClient[F] = GdsClient.loggingClient(name)

  override def applyConfig(config: GhostConfig): F[Unit] = overrideLogMessage(name, "applyConfig")

  override def observe(fileId: ImageFileId, expTime: TimeSpan): F[ImageFileId] =
    overrideLogMessage(name, s"observe $fileId").as(fileId)

  override def endObserve: F[Unit] = overrideLogMessage(name, "endObserve")

  override def stopObserve: F[Unit] = Applicative[F].unit

  override def abortObserve: F[Unit] = Applicative[F].unit

  override def pauseObserve: F[Unit] = Applicative[F].unit

  override def resumePaused(expTime: TimeSpan): F[ObserveCommandResult] =
    ObserveCommandResult.Success.pure[F].widen

  override def stopPaused: F[ObserveCommandResult] = ObserveCommandResult.Stopped.pure[F].widen

  override def abortPaused: F[ObserveCommandResult] = ObserveCommandResult.Aborted.pure[F].widen

  override def applyConfig(cfg: GhostConfig, conds: CurrentConditions): F[Unit] =
    overrideLogMessage(name, "applyConfig")
}

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.gnirs

import cats.Applicative
import cats.syntax.all.*
import fs2.Stream
import lucuma.core.util.TimeSpan
import observe.model.dhs.ImageFileId
import observe.model.enums.ObserveCommandResult
import observe.server.Progress
import observe.server.gnirs.GnirsController.GnirsConfig
import observe.server.overrideLogMessage
import org.typelevel.log4cats.Logger

class GnirsControllerDisabled[F[_]: {Logger, Applicative}] extends GnirsController[F] {
  private val name: String = "GNIRS"

  override def applyConfig(config: GnirsConfig): F[Unit] =
    overrideLogMessage(name, "applyConfig")

  override def observe(fileId: ImageFileId, expTime: TimeSpan): F[ObserveCommandResult] =
    overrideLogMessage(name, s"observe $fileId").as(ObserveCommandResult.Success)

  override def endObserve: F[Unit] = overrideLogMessage(name, "endObserve")

  override def stopObserve: F[Unit] = overrideLogMessage(name, "stopObserve")

  override def abortObserve: F[Unit] = overrideLogMessage(name, "abortObserve")

  override def observeProgress(total: TimeSpan): Stream[F, Progress] = Stream.empty
}

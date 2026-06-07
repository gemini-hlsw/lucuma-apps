// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.gnirs

import cats.effect.Async
import cats.syntax.all.*
import fs2.Stream
import lucuma.core.util.TimeSpan
import observe.model.dhs.ImageFileId
import observe.model.enums.ObserveCommandResult
import observe.server.InstrumentControllerSim
import observe.server.InstrumentSystem.ElapsedTime
import observe.server.Progress
import observe.server.gnirs.GnirsController.GnirsConfig
import org.typelevel.log4cats.Logger

final case class GnirsControllerSim[F[_]] private (sim: InstrumentControllerSim[F])
    extends GnirsController[F] {

  override def observe(fileId: ImageFileId, expTime: TimeSpan): F[ObserveCommandResult] =
    sim.observe(fileId, expTime)

  override def applyConfig(config: GnirsConfig): F[Unit] =
    sim.applyConfig(config)

  override def endObserve: F[Unit] = sim.endObserve

  override def stopObserve: F[Unit] = sim.stopObserve

  override def abortObserve: F[Unit] = sim.abortObserve

  override def observeProgress(total: TimeSpan): Stream[F, Progress] =
    sim.observeCountdown(total, ElapsedTime(TimeSpan.Zero))
}

object GnirsControllerSim {
  def apply[F[_]: {Async, Logger}]: F[GnirsController[F]] =
    InstrumentControllerSim("GNIRS").map(GnirsControllerSim(_))
}

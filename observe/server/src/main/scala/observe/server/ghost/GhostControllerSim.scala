// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.ghost

import cats.effect.Async
import cats.effect.Ref
import cats.effect.std.MapRef
import cats.syntax.all.*
import lucuma.core.util.TimeSpan
import observe.model.CurrentConditions
import observe.model.dhs.ImageFileId
import observe.model.enums.ObserveCommandResult
import observe.server.InstrumentControllerSim
import observe.server.keywords.GdsClient
import observe.server.keywords.KeywordBag
import org.typelevel.log4cats.Logger

import java.time.temporal.ChronoUnit

final case class GhostControllerSim[F[_]: {Async, Logger}] private (
  sim:         InstrumentControllerSim[F],
  configRef:   Ref[F, Option[GhostConfig]],
  accumulator: MapRef[F, ImageFileId, Option[KeywordBag]]
) extends GhostController[F] {

  override def gdsClient: GdsClient[F] = GdsClient.simulatedClient("GHOST", accumulator)

  override def stopObserve: F[Unit] = sim.stopObserve

  override def abortObserve: F[Unit] = sim.abortObserve

  override def pauseObserve: F[Unit] = sim.pauseObserve

  override def resumePaused(expTime: TimeSpan): F[ObserveCommandResult] = sim.resumePaused

  override def stopPaused: F[ObserveCommandResult] = sim.stopPaused

  override def abortPaused: F[ObserveCommandResult] = sim.abortPaused

  override def applyConfig(config: GhostConfig): F[Unit] =
    configRef.set(config.some) *> sim.applyConfig(config.configuration.toString)

  override def observe(fileId: ImageFileId, expTime: TimeSpan): F[ImageFileId] =
    configRef.get.flatMap: cfg =>
      val simTime = cfg.map(c => c.totalObserveTime(c.blueConfig, c.redConfig)).getOrElse(expTime)
      sim.observe(fileId, simTime).as(fileId)

  override def endObserve: F[Unit] = sim.endObserve

  override def applyConfig(config: GhostConfig, conds: CurrentConditions): F[Unit] =
    configRef.set(config.some) *> sim.applyConfig(config.configuration(conds).toString)
}

object GhostControllerSim {
  def apply[F[_]: {Async, Logger}]: F[GhostController[F]] =
    for {
      cfgRef <- Ref.of[F, Option[GhostConfig]](none)
      accRef <- MapRef.apply[F, ImageFileId, KeywordBag]
    } yield GhostControllerSim(
      InstrumentControllerSim.unsafeWithTimes(
        "GHOST",
        TimeSpan.Zero,
        TimeSpan.unsafeFromDuration(1500, ChronoUnit.MILLIS),
        TimeSpan.unsafeFromDuration(5, ChronoUnit.SECONDS)
      ),
      cfgRef,
      accRef
    )

}

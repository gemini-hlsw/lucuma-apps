// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.ghost

import cats.effect.Sync
import cats.syntax.all.*
import giapi.client.commands.Configuration
import giapi.client.ghost.GhostClient
import lucuma.core.util.TimeSpan
import observe.model.CurrentConditions
import observe.model.enums.ObserveCommandResult
import observe.server.AbstractGiapiInstrumentController
import observe.server.GiapiInstrumentController
import observe.server.keywords.GdsClient
import org.typelevel.log4cats.Logger

import java.time.temporal.ChronoUnit

trait GhostController[F[_]] extends GiapiInstrumentController[F, GhostConfig] {
  def gdsClient: GdsClient[F]

  def stopObserve: F[Unit]

  def abortObserve: F[Unit]

  def pauseObserve: F[Unit]

  def resumePaused(expTime: TimeSpan): F[ObserveCommandResult]

  def stopPaused: F[ObserveCommandResult]

  def abortPaused: F[ObserveCommandResult]

  def applyConfig(cfg: GhostConfig, conds: CurrentConditions): F[Unit]

}

object GhostController {

  val ConfigureTimeout: TimeSpan = TimeSpan.unsafeFromDuration(90, ChronoUnit.SECONDS)

  def apply[F[_]: {Sync, Logger}](client: GhostClient[F], gds: GdsClient[F]): GhostController[F] =
    new AbstractGiapiInstrumentController[F, GhostConfig, GhostClient[F]](client, ConfigureTimeout)
      with GhostController[F] {

      override val gdsClient: GdsClient[F] = gds

      override val name = "GHOST"

      private def isAGIdle: F[Boolean] =
        // 2 is idle, and 1 is guiding. We have more intermediate states but we'll
        // assume we don't want to move focus
        client.guidingState.map(_.exists(_ === 2))

      def configuration(config: GhostConfig, conds: CurrentConditions): F[Configuration] =
        for {
          value      <- client.guidingState
          idle       <- isAGIdle
          baseConfig  = config.configuration(conds)
          finalConfig = baseConfig |+| config.moveIFUToFocus.when(_ => idle)
          _          <- Logger[F].debug(s"Guiding check with value: $value isGuiding off: $idle")
          _          <- Logger[F].debug(pprint.apply(finalConfig).toString)
        } yield finalConfig

      override def applyConfig(cfg: GhostConfig, conds: CurrentConditions): F[Unit] = doApplyConfig(
        configuration(cfg, conds)
      )

      override def applyConfig(config: GhostConfig): F[Unit] =
        Logger[F].warn("Invalid call to GhostController.applyConfig(GhostConfig).")

      override def stopObserve: F[Unit] =
        client.stop.void

      override def abortObserve: F[Unit] =
        client.abort.void

      override def pauseObserve: F[Unit] =
        client.pause.void

      override def resumePaused(expTime: TimeSpan): F[ObserveCommandResult] =
        client.continue.map(_ => ObserveCommandResult.Success)

      override def stopPaused: F[ObserveCommandResult] =
        client.stop.map(_ => ObserveCommandResult.Stopped)

      override def abortPaused: F[ObserveCommandResult] =
        client.abort.map(_ => ObserveCommandResult.Aborted)

      override def configuration(config: GhostConfig): F[Configuration] = Configuration.Zero.pure[F]
    }
}

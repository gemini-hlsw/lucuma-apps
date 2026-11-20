// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import cats.Parallel
import cats.effect.Async
import cats.effect.Ref
import cats.effect.syntax.all.*
import cats.syntax.all.*
import lucuma.core.enums.AltairMode
import lucuma.odb.data.AltairConfiguration
import mouse.boolean.*
import navigate.epics.VerifiedEpics.*
import navigate.epics.given
import navigate.server.ApplyCommandResult
import navigate.server.ConnectionTimeout
import navigate.server.acm.GeminiApplyCommand
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.*

/**
 * Operations on Altair, at the level the telescope controller needs them. The details of how Altair
 * is driven (which records are written, in which order, and what has to be checked first) stay
 * inside the implementation.
 *
 * The parts of the Altair configuration that go in the TCS slew or configuration command (the AO
 * flag, the AO guide star and the field lens) are not here: they have to be in the same TCS apply
 * as the target, so the TCS controller adds them itself.
 */
trait AltairController[F[_]] {

  /**
   * Set Altair up for the given configuration: natural or laser guide star sensor, and neutral
   * density filter. If there is no Altair configuration, it is remembered that Altair is not in
   * use.
   */
  def configure(altair: Option[AltairConfiguration]): F[Unit]

  /**
   * Make the AOWFS probe follow the guide star, or stop doing it.
   *
   * The probe must never be commanded in LGS with P1 (REL-4159), because the AOWFS has no target
   * there, so this fails in that mode. It also fails if Altair has not been configured since
   * Navigate started, because the mode is not known.
   */
  def follow(enable: Boolean): F[ApplyCommandResult]

  /**
   * Park Altair, as TCC does: stop following, wait for it to be idle, and tell the TCS to park it.
   * Only the natural guide star mode is supported for now.
   */
  def park: F[ApplyCommandResult]

}

object AltairController {

  /** A request that Altair cannot, or must not, execute in its current situation. */
  final class AltairException(msg: String) extends GeminiApplyCommand.ApplyCommandException(msg)

  // Maximum time to wait for the changes between the natural and laser sensors, as in TCC
  private val UseLaserWfsTimeout = FiniteDuration(90, SECONDS)
  private val InitWfsTimeout     = FiniteDuration(20, SECONDS)
  private val NdFilterTimeout    = FiniteDuration(60, SECONDS)

  // How long, and how often, to check that the probe and Altair are idle when parking, as in TCC
  private val IdleTimeout       = FiniteDuration(10, SECONDS)
  private val IdleCheckInterval = FiniteDuration(200, MILLISECONDS)

  /**
   * @param tcs
   *   the TCS, which is the one that executes the commands for the AOWFS probe
   * @param tcsTimeout
   *   time to wait for the TCS commands
   */
  def fromEpics[F[_]: {Async, Parallel, Logger}](
    sys:        AltairEpicsSystem[F],
    tcs:        TcsEpicsSystem[F],
    tcsTimeout: FiniteDuration
  ): F[AltairController[F]] =
    Ref.of[F, Option[AltairMode]](none).map { modeRef =>
      new AltairController[F] {

        /**
         * Altair is set up for natural or laser guide stars with the same checks that TCC does. The
         * sensor is changed only if it is not already as needed: that is the case if the laser
         * state is wrong, or if any of the LGS mechanisms are not as they should be in LGS (the U
         * stage and the zoom) or are not at rest in NGS.
         */
        private def selectWfs(laser: Boolean): F[Unit] = for {
          usingLaser <- sys.status.usingLaserWfs.verifiedRun(ConnectionTimeout)
          uStageIn   <- sys.status.uStageIn.verifiedRun(ConnectionTimeout)
          zoomTrack  <- sys.status.zoomTracking.verifiedRun(ConnectionTimeout)
          zoomBusy   <- sys.status.zoomBusy.verifiedRun(ConnectionTimeout)
          zoomInPos  <- sys.status.zoomInPosition.verifiedRun(ConnectionTimeout)
          mustChange  = if (laser) !usingLaser || !uStageIn || !zoomTrack || !zoomBusy || !zoomInPos
                        else usingLaser || uStageIn || zoomBusy
          _          <- (Logger[F].debug(s"Switching Altair to ${laser.fold("LGS", "NGS")}") *>
                          sys.commands
                            .useLaserWfs(laser, UseLaserWfsTimeout)
                            .verifiedRun(ConnectionTimeout) *>
                          sys.commands
                            .initWfs(laser, InitWfsTimeout)
                            .verifiedRun(ConnectionTimeout)).whenA(mustChange)
        } yield ()

        private def selectNdFilter(altair: AltairConfiguration): F[Unit] =
          (if (altair.mode.usesLaser) sys.commands.lgsNdFilter(altair.ndFilter, NdFilterTimeout)
           else sys.commands.ngsNdFilter(altair.ndFilter, NdFilterTimeout))
            .verifiedRun(ConnectionTimeout)
            .void

        override def configure(altair: Option[AltairConfiguration]): F[Unit] =
          altair.fold(modeRef.set(none)) { a =>
            selectWfs(a.mode.usesLaser) *> selectNdFilter(a) *> modeRef.set(a.mode.some)
          }

        private def fail[A](msg: String): F[A] = new AltairException(msg).raiseError[F, A]

        private def currentMode: F[AltairMode] =
          modeRef.get.flatMap(
            _.fold(
              fail[AltairMode](
                "Altair has not been configured since Navigate started, so its mode is not known"
              )
            )(_.pure[F])
          )

        /**
         * Commanding the AOWFS probe in LGS with P1 made the Altair stage hit its limit and damaged
         * the hardware (REL-4159), because there is no AOWFS target in that mode. Seqexec does not
         * command it in either direction in that mode, so neither does Navigate.
         */
        private def followAllowed(mode: AltairMode): Boolean = mode =!= AltairMode.LgsP1

        private def tcsFollow(enable: Boolean): F[ApplyCommandResult] =
          tcs
            .startCommand(tcsTimeout)
            .aoProbeCommands
            .follow(enable)
            .post
            .verifiedRun(ConnectionTimeout)

        override def follow(enable: Boolean): F[ApplyCommandResult] =
          currentMode.flatMap { mode =>
            if (followAllowed(mode)) tcsFollow(enable)
            else fail(s"The AOWFS probe cannot be commanded in Altair mode $mode")
          }

        /** Wait, as long as TCC does, while `busy` is true. */
        private def waitWhileBusy(what: String, busy: F[Boolean]): F[Unit] =
          Async[F]
            .tailRecM(())(_ =>
              busy.flatMap(
                _.fold(Async[F].sleep(IdleCheckInterval).as(().asLeft[Unit]),
                       ().asRight[Unit].pure[F]
                )
              )
            )
            .timeoutTo(IdleTimeout, fail[Unit](s"Timed out waiting for $what"))

        private val waitForProbeToStop: F[Unit] =
          waitWhileBusy("Altair to stop following",
                        sys.status.wfsPositionBusy.verifiedRun(ConnectionTimeout)
          )

        private val waitForAltairIdle: F[Unit] =
          waitWhileBusy("Altair to go idle", sys.status.busy.verifiedRun(ConnectionTimeout))

        private val tcsPark: F[ApplyCommandResult] =
          tcs
            .startCommand(tcsTimeout)
            .aoProbeCommands
            .park
            .post
            .verifiedRun(ConnectionTimeout)

        override def park: F[ApplyCommandResult] =
          currentMode.flatMap {
            case AltairMode.Ngs =>
              // TCC repeats the follow command because "the first attempt often fails"
              tcsFollow(false) *> waitForProbeToStop *>
                tcsFollow(false) *> waitForProbeToStop *>
                waitForAltairIdle *>
                tcsPark
            case mode           =>
              fail(s"Parking Altair is not supported yet in mode $mode")
          }
      }
    }

}

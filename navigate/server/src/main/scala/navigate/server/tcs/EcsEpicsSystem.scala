// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import cats.MonadThrow
import cats.effect.kernel.Resource
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.IntPercent
import navigate.epics.EpicsService
import navigate.epics.VerifiedEpics
import navigate.epics.VerifiedEpics.*
import navigate.server.acm.CadDirective
import navigate.server.tcs.EcsEpicsSystem.EcsCommands
import navigate.server.tcs.EcsEpicsSystem.EcsStatus

trait EcsEpicsSystem[F[_]] {
  val status: EcsStatus[F]
  val commands: EcsCommands[F]
}

object EcsEpicsSystem {
  trait EcsStatus[F[_]] {
    def eastVentGatePos: VerifiedEpics[F, F, IntPercent]
    def westVentGatePos: VerifiedEpics[F, F, IntPercent]
  }

  /**
   * The close commands are triggered by writing a 1 to a PROC channel. There is no CAR to monitor,
   * so they are fire and forget.
   */
  trait EcsCommands[F[_]] {
    // CAD record: it is triggered by writing MARK and then START to its directive.
    def stopShutters: VerifiedEpics[F, F, Unit]
    def closeShutters: VerifiedEpics[F, F, Unit]
    def closeEastVentGate: VerifiedEpics[F, F, Unit]
    def closeWestVentGate: VerifiedEpics[F, F, Unit]
  }

  private[tcs] def buildSystem[F[_]: MonadThrow](
    ch: EcsChannels[F]
  ): EcsEpicsSystem[F] = new {
    override val status: EcsStatus[F] = new {
      override def eastVentGatePos: VerifiedEpics[F, F, IntPercent] = VerifiedEpics
        .readChannel(ch.telltale, ch.eastVentGateAperture)
        .map(_.map(v => IntPercent.from(v.toInt).getOrElse(ventGateClosePos)))

      override def westVentGatePos: VerifiedEpics[F, F, IntPercent] = VerifiedEpics
        .readChannel(ch.telltale, ch.westVentGateAperture)
        .map(_.map(v => IntPercent.from(v.toInt).getOrElse(ventGateClosePos)))
    }

    override val commands: EcsCommands[F] = new {
      override def stopShutters: VerifiedEpics[F, F, Unit] =
        VerifiedEpics.writeChannel(ch.telltale, ch.stopShuttersDir)(CadDirective.MARK.pure[F]) *>
          VerifiedEpics.writeChannel(ch.telltale, ch.stopShuttersDir)(CadDirective.START.pure[F])

      override def closeShutters: VerifiedEpics[F, F, Unit] =
        VerifiedEpics.writeChannel(ch.telltale, ch.closeShutters)(1.pure[F])

      override def closeEastVentGate: VerifiedEpics[F, F, Unit] =
        VerifiedEpics.writeChannel(ch.telltale, ch.closeEastVentGate)(1.pure[F])

      override def closeWestVentGate: VerifiedEpics[F, F, Unit] =
        VerifiedEpics.writeChannel(ch.telltale, ch.closeWestVentGate)(1.pure[F])
    }
  }

  def build[F[_]: MonadThrow](
    service: EpicsService[F],
    top:     NonEmptyString
  ): Resource[F, EcsEpicsSystem[F]] =
    EcsChannels.build(service, top).map(buildSystem)

  val ventGateClosePos: IntPercent = IntPercent.unsafeFrom(0)

}

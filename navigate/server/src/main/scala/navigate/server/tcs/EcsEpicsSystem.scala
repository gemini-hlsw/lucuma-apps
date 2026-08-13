// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import cats.MonadThrow
import cats.effect.kernel.Resource
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.IntPercent
import navigate.epics.EpicsService
import navigate.epics.VerifiedEpics
import navigate.epics.VerifiedEpics.VerifiedEpics
import navigate.server.tcs.EcsEpicsSystem.EcsStatus

trait EcsEpicsSystem[F[_]] {
  val status: EcsStatus[F]
}

object EcsEpicsSystem {
  trait EcsStatus[F[_]] {
    def eastVentGatePos: VerifiedEpics[F, F, IntPercent]
    def westVentGatePos: VerifiedEpics[F, F, IntPercent]
  }

  private[tcs] def buildSystem[F[_]: MonadThrow](
    ch: EcsChannels[F]
  ): EcsEpicsSystem[F] = new {
    override val status: EcsStatus[F] = new {
      override def eastVentGatePos: VerifiedEpics[F, F, IntPercent] = VerifiedEpics
        .readChannel(ch.telltale, ch.eastVentGateAperture)
        .map(_.map(v => IntPercent.from((v * 100.0).toInt).getOrElse(IntPercent.unsafeFrom(0))))

      override def westVentGatePos: VerifiedEpics[F, F, IntPercent] = VerifiedEpics
        .readChannel(ch.telltale, ch.westVentGateAperture)
        .map(_.map(v => IntPercent.from((v * 100.0).toInt).getOrElse(IntPercent.unsafeFrom(0))))
    }
  }

  def build[F[_]: MonadThrow](
    service: EpicsService[F],
    top:     NonEmptyString
  ): Resource[F, EcsEpicsSystem[F]] =
    EcsChannels.build(service, top).map(buildSystem)

}

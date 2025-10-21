// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import cats.Applicative
import cats.effect.Resource
import cats.effect.Temporal
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import navigate.epics.*
import navigate.epics.VerifiedEpics.VerifiedEpics
import navigate.model.enums.CentralBafflePosition
import navigate.model.enums.DeployableBafflePosition
import navigate.server.tcs.FollowStatus.*

import encoders.*

trait ScsEpicsSystem[F[_]] {
  def getFollowingState: VerifiedEpics[F, F, FollowStatus]
  def getCentralBaffleState: VerifiedEpics[F, F, CentralBafflePosition]
  def getDeployableBaffleState: VerifiedEpics[F, F, DeployableBafflePosition]
}

object ScsEpicsSystem {
  private[tcs] def buildSystem[F[_]: Applicative](channels: ScsChannels[F]): ScsEpicsSystem[F] =
    new ScsEpicsSystem[F] {
      override def getFollowingState: VerifiedEpics[F, F, FollowStatus] =
        VerifiedEpics
          .readChannel(channels.telltale, channels.follow)
          .map {
            _.map {
              case "YES" => Following
              case _     => NotFollowing
            }
          }

      override def getCentralBaffleState: VerifiedEpics[F, F, CentralBafflePosition] = VerifiedEpics
        .readChannel(channels.telltale, channels.centralBaffle)
        .map(
          _.map(
            _.decode[CentralBafflePosition].getOrElse(CentralBafflePosition.Open)
          )
        )

      override def getDeployableBaffleState: VerifiedEpics[F, F, DeployableBafflePosition] =
        VerifiedEpics
          .readChannel(channels.telltale, channels.deployableBaffle)
          .map(
            _.map(
              _.decode[DeployableBafflePosition].getOrElse(DeployableBafflePosition.Visible)
            )
          )
    }

  def build[F[_]: Temporal](
    service: EpicsService[F],
    top:     NonEmptyString
  ): Resource[F, ScsEpicsSystem[F]] =
    ScsChannels.build(service, top).map(buildSystem)
}

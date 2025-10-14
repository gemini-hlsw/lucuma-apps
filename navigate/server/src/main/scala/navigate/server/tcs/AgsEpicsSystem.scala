// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import cats.Applicative
import cats.effect.Resource
import cats.effect.Temporal
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.math.Angle
import lucuma.core.util.Enumerated
import mouse.boolean.*
import navigate.epics.*
import navigate.epics.EpicsSystem.TelltaleChannel
import navigate.epics.VerifiedEpics.*
import navigate.model.enums.PwfsFieldStop
import navigate.model.enums.PwfsFilter
import navigate.server.acm.Decoder.*
import navigate.server.epicsdata.AgMechPosition
import navigate.server.tcs.AgsEpicsSystem.AgsStatus

import encoders.{*, given}
import FollowStatus.*
import ParkStatus.*
import ScienceFoldPositionCodex.given

trait AgsEpicsSystem[F[_]] {
  val status: AgsStatus[F]
}

object AgsEpicsSystem {

  trait AgsStatus[F[_]] {
    def inPosition: VerifiedEpics[F, F, Boolean]
    def sfParked: VerifiedEpics[F, F, ParkStatus]
    def aoParked: VerifiedEpics[F, F, ParkStatus]
    def hwParked: VerifiedEpics[F, F, ParkStatus]
    def p1Parked: VerifiedEpics[F, F, ParkStatus]
    def p1Follow: VerifiedEpics[F, F, FollowStatus]
    def p2Parked: VerifiedEpics[F, F, ParkStatus]
    def p2Follow: VerifiedEpics[F, F, FollowStatus]
    def oiParked: VerifiedEpics[F, F, ParkStatus]
    def oiFollow: VerifiedEpics[F, F, FollowStatus]
    def flamingos2Port: VerifiedEpics[F, F, Int]
    def ghostPort: VerifiedEpics[F, F, Int]
    def gmosPort: VerifiedEpics[F, F, Int]
    def gnirsPort: VerifiedEpics[F, F, Int]
    def gpiPort: VerifiedEpics[F, F, Int]
    def gsaoiPort: VerifiedEpics[F, F, Int]
    def igrins2Port: VerifiedEpics[F, F, Int]
    def nifsPort: VerifiedEpics[F, F, Int]
    def niriPort: VerifiedEpics[F, F, Int]
    def aoName: VerifiedEpics[F, F, AgMechPosition]
    def hwName: VerifiedEpics[F, F, AgMechPosition]
    def sfName: VerifiedEpics[F, F, ScienceFold]
    def pwfs1Angles: PwfsAngles[F]
    def pwfs2Angles: PwfsAngles[F]
    def pwfs1Mechs: PwfsMechs[F]
    def pwfs2Mechs: PwfsMechs[F]
  }

  private[tcs] def buildSystem[F[_]: Applicative](
    channels: AgsChannels[F]
  ): AgsEpicsSystem[F] =
    new AgsEpicsSystem[F] {
      override val status: AgsStatus[F] = new AgsStatus[F] {

        override def inPosition: VerifiedEpics[F, F, Boolean] =
          VerifiedEpics.readChannel(channels.telltale, channels.inPosition).map(_.map(a => a =!= 0))

        override def sfParked: VerifiedEpics[F, F, ParkStatus] = VerifiedEpics
          .readChannel(channels.telltale, channels.sfParked)
          .map(_.map(a => (a =!= 0).fold(Parked, NotParked)))

        override def aoParked: VerifiedEpics[F, F, ParkStatus] = VerifiedEpics
          .readChannel(channels.telltale, channels.aoParked)
          .map(_.map(a => (a =!= 0).fold(Parked, NotParked)))

        override def hwParked: VerifiedEpics[F, F, ParkStatus] = VerifiedEpics
          .readChannel(channels.telltale, channels.hwParked)
          .map(_.map(a => (a =!= 0).fold(Parked, NotParked)))

        override def p1Parked: VerifiedEpics[F, F, ParkStatus] = VerifiedEpics
          .readChannel(channels.telltale, channels.p1Parked)
          .map(_.map(a => (a =!= 0).fold(Parked, NotParked)))

        override def p1Follow: VerifiedEpics[F, F, FollowStatus] =
          VerifiedEpics.readChannel(channels.telltale, channels.p1Follow).map {
            _.map(decodeFollow)
          }

        override def p2Parked: VerifiedEpics[F, F, ParkStatus] = VerifiedEpics
          .readChannel(channels.telltale, channels.p2Parked)
          .map(_.map(a => (a =!= 0).fold(Parked, NotParked)))

        override def p2Follow: VerifiedEpics[F, F, FollowStatus] =
          VerifiedEpics.readChannel(channels.telltale, channels.p2Follow).map {
            _.map(decodeFollow)
          }

        override def oiParked: VerifiedEpics[F, F, ParkStatus] = VerifiedEpics
          .readChannel(channels.telltale, channels.oiParked)
          .map(_.map(a => (a =!= 0).fold(Parked, NotParked)))

        override def oiFollow: VerifiedEpics[F, F, FollowStatus] =
          VerifiedEpics.readChannel(channels.telltale, channels.oiFollow).map {
            _.map(decodeFollow)
          }

        private def decodeFollow(str: String): FollowStatus = str.toUpperCase.trim match {
          case "ON" => Following
          case _    => NotFollowing
        }

        override def flamingos2Port: VerifiedEpics[F, F, Int] =
          VerifiedEpics.readChannel(channels.telltale, channels.instrumentPorts.f2)

        override def ghostPort: VerifiedEpics[F, F, Int] =
          VerifiedEpics.readChannel(channels.telltale, channels.instrumentPorts.ghost)

        override def gmosPort: VerifiedEpics[F, F, Int] =
          VerifiedEpics.readChannel(channels.telltale, channels.instrumentPorts.gmos)

        override def gnirsPort: VerifiedEpics[F, F, Int] =
          VerifiedEpics.readChannel(channels.telltale, channels.instrumentPorts.gnirs)

        override def gpiPort: VerifiedEpics[F, F, Int] =
          VerifiedEpics.readChannel(channels.telltale, channels.instrumentPorts.gpi)

        override def gsaoiPort: VerifiedEpics[F, F, Int] =
          VerifiedEpics.readChannel(channels.telltale, channels.instrumentPorts.gsaoi)

        override def igrins2Port: VerifiedEpics[F, F, Int] =
          VerifiedEpics.readChannel(channels.telltale, channels.instrumentPorts.igrins2)

        override def nifsPort: VerifiedEpics[F, F, Int] =
          VerifiedEpics.readChannel(channels.telltale, channels.instrumentPorts.nifs)

        override def niriPort: VerifiedEpics[F, F, Int] =
          VerifiedEpics.readChannel(channels.telltale, channels.instrumentPorts.niri)

        override def aoName: VerifiedEpics[F, F, AgMechPosition] =
          VerifiedEpics
            .readChannel(channels.telltale, channels.aoName)
            .map(_.map(Enumerated[AgMechPosition].fromTag(_).getOrElse(AgMechPosition.Unknown)))

        override def hwName: VerifiedEpics[F, F, AgMechPosition] =
          VerifiedEpics
            .readChannel(channels.telltale, channels.hwName)
            .map(_.map(Enumerated[AgMechPosition].fromTag(_).getOrElse(AgMechPosition.Unknown)))

        override def sfName: VerifiedEpics[F, F, ScienceFold] =
          VerifiedEpics.readChannel(channels.telltale, channels.sfName).map(_.map(_.decode))

        override def pwfs1Angles: PwfsAngles[F] =
          PwfsAngles.build(channels.telltale, channels.p1Angles)

        override def pwfs2Angles: PwfsAngles[F] =
          PwfsAngles.build(channels.telltale, channels.p2Angles)

        override def pwfs1Mechs: PwfsMechs[F] = PwfsMechs.build(channels.telltale, channels.p1Mechs)

        override def pwfs2Mechs: PwfsMechs[F] = PwfsMechs.build(channels.telltale, channels.p2Mechs)
      }
    }

  trait PwfsAngles[F[_]] {
    val tableAngle: VerifiedEpics[F, F, Angle]
    val armAngle: VerifiedEpics[F, F, Angle]
  }

  object PwfsAngles {
    def build[F[_]: Applicative](
      tt:  TelltaleChannel[F],
      chs: AgsChannels.PwfsAnglesChannels[F]
    ): PwfsAngles[F] =
      new PwfsAngles[F] {
        override val tableAngle: VerifiedEpics[F, F, Angle] = VerifiedEpics
          .readChannel[F, Double](tt, chs.tableAngle)
          .map(_.map(Angle.fromDoubleDegrees))
        override val armAngle: VerifiedEpics[F, F, Angle]   =
          VerifiedEpics.readChannel[F, Double](tt, chs.armAngle).map(_.map(Angle.fromDoubleDegrees))
      }
  }

  trait PwfsMechs[F[_]] {
    val colFilter: VerifiedEpics[F, F, Option[PwfsFilter]]
    val fieldStop: VerifiedEpics[F, F, Option[PwfsFieldStop]]
  }

  object PwfsMechs {
    def build[F[_]: Applicative](
      tt:  TelltaleChannel[F],
      chs: AgsChannels.PwfsMechsChannels[F]
    ): PwfsMechs[F] = new PwfsMechs {
      override val colFilter: VerifiedEpics[F, F, Option[PwfsFilter]]    = VerifiedEpics
        .readChannel[F, String](tt, chs.colFilter)
        .map(_.map(_.decode))
      override val fieldStop: VerifiedEpics[F, F, Option[PwfsFieldStop]] = VerifiedEpics
        .readChannel[F, String](tt, chs.fieldStop)
        .map(_.map(_.decode))
    }
  }

  def build[F[_]: Temporal](
    service: EpicsService[F],
    top:     NonEmptyString
  ): Resource[F, AgsEpicsSystem[F]] = AgsChannels.build[F](service, top).map(buildSystem)
}

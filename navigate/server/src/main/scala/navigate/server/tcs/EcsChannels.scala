// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import cats.effect.Resource
import eu.timepit.refined.types.string.NonEmptyString
import navigate.epics.Channel
import navigate.epics.EpicsService
import navigate.epics.EpicsSystem.TelltaleChannel
import navigate.epics.given

case class EcsChannels[F[_]](
  telltale:             TelltaleChannel[F],
  eastVentGateAperture: Channel[F, Double],
  westVentGateAperture: Channel[F, Double],
  closeShutters:        Channel[F, Int],
  closeEastVentGate:    Channel[F, Int],
  closeWestVentGate:    Channel[F, Int]
)

object EcsChannels {
  val sysName: String = "ECS"

  def build[F[_]](
    service: EpicsService[F],
    top:     NonEmptyString
  ): Resource[F, EcsChannels[F]] = for {
    tt  <- service.getChannel[String](top, "health.VAL").map(TelltaleChannel(sysName, _))
    evg <- service.getChannel[Double](top, "sad:eastVentGatePos.VAL")
    wvg <- service.getChannel[Double](top, "sad:westVentGatePos.VAL")
    csh <- service.getChannel[Int](top, "closeSH.PROC")
    cev <- service.getChannel[Int](top, "closeEVG.PROC")
    cwv <- service.getChannel[Int](top, "closeWVG.PROC")
  } yield EcsChannels(tt, evg, wvg, csh, cev, cwv)

}

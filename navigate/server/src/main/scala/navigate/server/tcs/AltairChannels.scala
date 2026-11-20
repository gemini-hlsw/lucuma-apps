// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import cats.effect.Resource
import eu.timepit.refined.types.string.NonEmptyString
import navigate.epics.Channel
import navigate.epics.EpicsService
import navigate.epics.EpicsSystem.TelltaleChannel
import navigate.epics.given
import navigate.server.acm.CarState

/**
 * Channels of the Altair control system. `useLwfs` and `wfsInit` are taskControl records, and the
 * ND filters are assemblyControl records, both with their parameters. The others are statuses.
 */
case class AltairChannels[F[_]](
  telltale:          TelltaleChannel[F],
  useLwfsParam:      Channel[F, String],
  wfsInitParam:      Channel[F, String],
  ngsNdFilterParams: AltairChannels.NdFilterParamChannels[F],
  lgsNdFilterParams: AltairChannels.NdFilterParamChannels[F],
  useLwfs:           Channel[F, String],
  uStage:            Channel[F, String],
  zoomMode:          Channel[F, String],
  zoomBusy:          Channel[F, String],
  zoomInPosition:    Channel[F, Int],
  wfsPositionBusy:   Channel[F, CarState],
  gaosBusy:          Channel[F, CarState]
)

object AltairChannels {
  val sysName: String = "ALTAIR"

  // Names of the taskControl records, relative to the Altair top
  val UseLwfsRecord: String = "useLwfs"
  val WfsInitRecord: String = "wfcs:wfsInit"

  // Names of the ND filter assemblyControl records, relative to the Altair top
  val NgsNdFilterRecord: String = "cc:ngsNDfiltAssembly"
  val LgsNdFilterRecord: String = "cc:lgsNDfiltAssembly"

  case class NdFilterParamChannels[F[_]](
    position: Channel[F, String],
    mode:     Channel[F, String]
  )

  object NdFilterParamChannels {
    def build[F[_]](
      service: EpicsService[F],
      top:     NonEmptyString,
      record:  String
    ): Resource[F, NdFilterParamChannels[F]] = for {
      p <- service.getChannel[String](top, s"$record.A")
      m <- service.getChannel[String](top, s"$record.MODE")
    } yield NdFilterParamChannels(p, m)
  }

  def build[F[_]](
    service: EpicsService[F],
    top:     NonEmptyString
  ): Resource[F, AltairChannels[F]] = for {
    t   <- service.getChannel[String](top, "followS").map(TelltaleChannel(sysName, _))
    ulp <- service.getChannel[String](top, s"$UseLwfsRecord.A")
    wip <- service.getChannel[String](top, s"$WfsInitRecord.A")
    ngs <- NdFilterParamChannels.build(service, top, NgsNdFilterRecord)
    lgs <- NdFilterParamChannels.build(service, top, LgsNdFilterRecord)
    ul  <- service.getChannel[String](top, s"$UseLwfsRecord.VALA")
    us  <- service.getChannel[String](top, "cc:lgsustAssembly.VALA")
    zm  <- service.getChannel[String](top, "cc:lgszoomAssembly.MODE")
    zb  <- service.getChannel[String](top, "cc:lgszoomAssembly.BUSY")
    zp  <- service.getChannel[Int](top, "cc:lgszoomPosition.VALA")
    wpb <- service.getChannel[CarState](top, "cc:wfsposAssembly.BUSY")
    gb  <- service.getChannel[CarState](top, "WfcsCcC.VAL")
  } yield AltairChannels(t, ulp, wip, ngs, lgs, ul, us, zm, zb, zp, wpb, gb)
}

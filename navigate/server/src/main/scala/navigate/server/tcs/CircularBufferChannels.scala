// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import cats.effect.Resource
import eu.timepit.refined.types.string.NonEmptyString
import navigate.epics.Channel
import navigate.epics.EpicsService
import navigate.epics.EpicsSystem.TelltaleChannel
import navigate.epics.given
import navigate.server.acm.CadDirective

case class CircularBufferChannels[F[_]](
  telltale:  TelltaleChannel[F],
  dir:       Channel[F, CadDirective],
  enableIm:  Channel[F, String],
  enableAo:  Channel[F, String],
  enableFg:  Channel[F, String],
  imEnabled: Channel[F, String],
  aoEnabled: Channel[F, String],
  fgEnabled: Channel[F, String]
)

object CircularBufferChannels {

  def build[F[_]](
    server:   EpicsService[F],
    top:      NonEmptyString,
    telltale: Either[String, TelltaleChannel[F]]
  ): Resource[F, CircularBufferChannels[F]] = for {
    tt  <- telltale.fold(
             sysName =>
               server.getChannel[String](top, "health.VAL").map(TelltaleChannel(sysName, _)),
             Resource.pure
           )
    dir <- server.getChannel[CadDirective](top, ":dc:detSigSaveCb.DIR")
    imp <- server.getChannel[String](top, ":dc:detSigSaveCb.A")
    aop <- server.getChannel[String](top, ":dc:detSigSaveCb.B")
    fgp <- server.getChannel[String](top, ":dc:detSigSaveCb.C")
    ims <- server.getChannel[String](top, ":dc:aoSaveCbIm.VAL")
    aos <- server.getChannel[String](top, ":dc:aoSaveCbAoCtrl.VAL")
    fgs <- server.getChannel[String](top, ":dc:aoSaveCbFgCtrl.VAL")
  } yield CircularBufferChannels(tt, dir, imp, aop, fgp, ims, aos, fgs)
}

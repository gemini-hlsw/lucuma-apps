// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import cats.Parallel
import cats.effect.Async
import cats.effect.Ref
import cats.effect.Resource
import cats.syntax.all.*
import fs2.Stream
import lucuma.core.enums.Instrument
import lucuma.core.enums.Site
import navigate.epics.given
import navigate.model.WfsConfiguration
import navigate.model.enums.HrwfsPickupPosition
import navigate.server.ApplyCommandResult
import navigate.server.ConnectionTimeout
import navigate.server.tcs.TcsEpicsSystem.TcsCommands
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.FiniteDuration

import EpicsSystems.*

class TcsNorthControllerEpics[F[_]: {Async, Parallel, Logger}](
  sys:      EpicsSystemsNorth[F],
  timeout:  FiniteDuration,
  stateRef: Ref[F, TcsBaseControllerEpics.State]
) extends TcsBaseControllerEpics[F](
      sys,
      timeout,
      stateRef
    )
    with TcsNorthController[F] {

  override val acInstrument: Instrument = Instrument.AcqCamNorth

  override val site: Site = Site.GN

  override def getInstrumentPort(instrument: Instrument): F[Option[Int]] = (instrument match {
    case Instrument.AcqCamNorth  => 1.pure[F]
    case Instrument.AcqCamSouth  => 0.pure[F]
    case Instrument.Alopeke      => 2.pure[F]
    case Instrument.Flamingos2   => 0.pure[F]
    case Instrument.Ghost        => 0.pure[F]
    case Instrument.GmosNorth    => sys.ags.status.gmosPort.verifiedRun(ConnectionTimeout)
    case Instrument.GmosSouth    => 0.pure[F]
    case Instrument.Gnirs        => sys.ags.status.gnirsPort.verifiedRun(ConnectionTimeout)
    case Instrument.Gpi          => sys.ags.status.gpiPort.verifiedRun(ConnectionTimeout)
    case Instrument.Gsaoi        => 0.pure[F]
    case Instrument.Igrins2      => sys.ags.status.igrins2Port.verifiedRun(ConnectionTimeout)
    case Instrument.MaroonX      => 1.pure[F]
    case Instrument.Niri         => sys.ags.status.niriPort.verifiedRun(ConnectionTimeout)
    case Instrument.Scorpio      => 0.pure[F]
    case Instrument.VisitorNorth => sys.ags.status.visitorPort.verifiedRun(ConnectionTimeout)
    case Instrument.VisitorSouth => 0.pure[F]
    case Instrument.Zorro        => 0.pure[F]
  }).map(_.some.filter(_ =!= 0))

  override def oiwfsCircularBuffer(enable: Boolean): F[ApplyCommandResult] =
    wfsCircularBuffer(sys.oiwfs, enable)

  override def getOiwfsConfig: F[WfsConfiguration] = getWfsConfig(sys.oiwfs, sys.oiwfs)

  override def oiwfsConfigStream: Resource[F, Stream[F, WfsConfiguration]] =
    wfsConfigStream(sys.oiwfs, sys.oiwfs)

  // At GN, pickoff mirror is moved to Out position instead of parked
  override def takeHrOut(cmds: TcsCommands[F]): TcsCommands[F] =
    cmds.hrwfsCommands.move.setPosition(HrwfsPickupPosition.Out)

}

object TcsNorthControllerEpics {

  def build[F[_]: {Async, Parallel, Logger}](
    sys:     EpicsSystemsNorth[F],
    timeout: FiniteDuration
  ): F[TcsNorthControllerEpics[F]] =
    Ref
      .of[F, TcsBaseControllerEpics.State](TcsBaseControllerEpics.State.default)
      .map(
        new TcsNorthControllerEpics(sys, timeout, _)
      )

}

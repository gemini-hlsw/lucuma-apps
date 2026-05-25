// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import cats.Parallel
import cats.effect.Async
import cats.effect.Ref
import cats.effect.Resource
import cats.syntax.all.*
import fs2.Stream
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

  override def getInstrumentPorts: F[InstrumentPorts] = (for {
    gmF    <- sys.ags.status.gmosPort
    gnF    <- sys.ags.status.gnirsPort
    gpF    <- sys.ags.status.gpiPort
    igF    <- sys.ags.status.igrins2Port
    nfF    <- sys.ags.status.nifsPort
    nrF    <- sys.ags.status.niriPort
    vsF    <- sys.ags.status.visitorPort
    port1F <- sys.ags.status.portLabel(1)
  } yield for {
    gm    <- gmF
    gn    <- gnF
    gp    <- gpF
    ig    <- igF
    nf    <- nfF
    nr    <- nrF
    vs    <- vsF
    port1 <- port1F
  } yield InstrumentPorts(
    flamingos2Port = 0,
    ghostPort = 0,
    gmosPort = gm,
    gnirsPort = gn,
    gpiPort = gp,
    gsaoiPort = 0,
    igrins2Port = ig,
    nifsPort = nf,
    niriPort = nr,
    visitorPort = vs
  )).verifiedRun(ConnectionTimeout)

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

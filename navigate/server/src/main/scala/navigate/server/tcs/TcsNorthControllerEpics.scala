// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import cats.Parallel
import cats.effect.Async
import cats.effect.Ref
import cats.effect.Resource
import cats.syntax.all.*
import fs2.Stream
import lucuma.core.enums.FieldLens
import lucuma.core.enums.Instrument
import lucuma.core.enums.Site
import lucuma.core.math.Angle
import lucuma.odb.data.AltairConfiguration
import lucuma.schemas.model.navigate.LightSource
import navigate.epics.VerifiedEpics.*
import navigate.epics.given
import navigate.model.Target
import navigate.model.TcsConfig
import navigate.model.WfsConfiguration
import navigate.model.enums.HrwfsPickupPosition
import navigate.server.ApplyCommandResult
import navigate.server.ConnectionTimeout
import navigate.server.tcs.TcsEpicsSystem.TcsCommands
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.*

import EpicsSystems.*

class TcsNorthControllerEpics[F[_]: {Async, Parallel, Logger}](
  sys:              EpicsSystemsNorth[F],
  altairController: AltairController[F],
  timeout:          FiniteDuration,
  stateRef:         Ref[F, TcsBaseControllerEpics.State]
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

  override protected val domeParkPosition: Angle = Angle.fromDoubleDegrees(90.0)

  override protected def usesAo(config: TcsConfig): Boolean = config.altair.isDefined

  override protected def altairTcsConfig(config: TcsConfig): TcsCommands[F] => TcsCommands[F] =
    config.altair.fold(identity[TcsCommands[F]]) { altair => (cmds: TcsCommands[F]) =>
      cmds.aoConfigCommand.fieldLens(fieldLens(config, altair))
    }

  /**
   * The field lens that will be used: the one the user chose, or if left on auto, the one that
   * corresponds to the position of the guide star. With no guide star (NGS), it is out.
   */
  private def fieldLens(config: TcsConfig, altair: AltairConfiguration): FieldLens =
    altair.fieldLens(guideStarSeparation(config)).getOrElse(FieldLens.Out)

  private def guideStarSeparation(config: TcsConfig): Option[Angle] =
    (config.aowfs.map(_.target), config.sourceATarget) match {
      case (Some(g: Target.SiderealTarget), s: Target.SiderealTarget) =>
        g.coordinates.angularDistance(s.coordinates).some
      case _                                                          => none
    }

  // With Altair, the light comes from the sky through the AO system
  override def altairFollow(enable: Boolean): F[ApplyCommandResult] =
    altairController.follow(enable)

  override def altairPark: F[ApplyCommandResult] = altairController.park

  override protected def defaultLightSource(config: TcsConfig): LightSource =
    config.altair.fold(LightSource.Sky)(_ => LightSource.AO)

  override protected def configureAltair(config: TcsConfig): F[Unit] =
    altairController.configure(config.altair)
}

object TcsNorthControllerEpics {

  def build[F[_]: {Async, Parallel, Logger}](
    sys:     EpicsSystemsNorth[F],
    timeout: FiniteDuration
  ): F[TcsNorthControllerEpics[F]] =
    for {
      altair <- AltairController.fromEpics(sys.altair, sys.tcsEpics, timeout)
      state  <- Ref.of[F, TcsBaseControllerEpics.State](TcsBaseControllerEpics.State.default)
    } yield new TcsNorthControllerEpics(sys, altair, timeout, state)

}

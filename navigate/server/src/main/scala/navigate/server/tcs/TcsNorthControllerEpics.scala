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
import mouse.boolean.*
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

  override protected val domeParkPosition: Angle = Angle.fromDoubleDegrees(90.0)

  // The TCS has to know if it is being used with Altair. Without it, the AO flag is cleared.
  override protected def altairTcsConfig(config: TcsConfig): TcsCommands[F] => TcsCommands[F] =
    config.altair.fold { (cmds: TcsCommands[F]) =>
      cmds.aoConfigCommand.useAo(false)
    } { altair => (cmds: TcsCommands[F]) =>
      cmds.aoConfigCommand
        .useAo(true)
        .aoConfigCommand
        .fieldLens(fieldLens(config, altair))
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

  // Maximum time to wait for the changes between the natural and laser sensors, as in TCC
  private val UseLaserWfsTimeout = FiniteDuration(90, SECONDS)
  private val InitWfsTimeout     = FiniteDuration(20, SECONDS)
  private val NdFilterTimeout    = FiniteDuration(60, SECONDS)

  /**
   * Altair is set up for natural or laser guide stars with the same checks that TCC does. The
   * sensor is changed only if it is not already as needed: that is the case if the laser state is
   * wrong, or if any of the LGS mechanisms are not as they should be in LGS (the U stage and the
   * zoom) or are not at rest in NGS.
   */
  private def selectWfs(laser: Boolean): F[Unit] = for {
    usingLaser <- sys.altair.status.usingLaserWfs.verifiedRun(ConnectionTimeout)
    uStageIn   <- sys.altair.status.uStageIn.verifiedRun(ConnectionTimeout)
    zoomTrack  <- sys.altair.status.zoomTracking.verifiedRun(ConnectionTimeout)
    zoomBusy   <- sys.altair.status.zoomBusy.verifiedRun(ConnectionTimeout)
    zoomInPos  <- sys.altair.status.zoomInPosition.verifiedRun(ConnectionTimeout)
    mustChange  = if (laser) !usingLaser || !uStageIn || !zoomTrack || !zoomBusy || !zoomInPos
                  else usingLaser || uStageIn || zoomBusy
    _          <- (Logger[F].debug(s"Switching Altair to ${laser.fold("LGS", "NGS")}") *>
                    sys.altair.commands
                      .useLaserWfs(laser, UseLaserWfsTimeout)
                      .verifiedRun(ConnectionTimeout) *>
                    sys.altair.commands
                      .initWfs(laser, InitWfsTimeout)
                      .verifiedRun(ConnectionTimeout)).whenA(mustChange)
  } yield ()

  private def selectNdFilter(altair: AltairConfiguration): F[ApplyCommandResult] =
    (if (altair.mode.usesLaser) sys.altair.commands.lgsNdFilter(altair.ndFilter, NdFilterTimeout)
     else sys.altair.commands.ngsNdFilter(altair.ndFilter, NdFilterTimeout))
      .verifiedRun(ConnectionTimeout)

  // With Altair, the light comes from the sky through the AO system
  override protected def defaultLightSource(config: TcsConfig): LightSource =
    config.altair.fold(LightSource.Sky)(_ => LightSource.AO)

  override protected def configureAltair(config: TcsConfig): F[Unit] =
    config.altair.traverse_ { altair =>
      selectWfs(altair.mode.usesLaser) *> selectNdFilter(altair).void
    }
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

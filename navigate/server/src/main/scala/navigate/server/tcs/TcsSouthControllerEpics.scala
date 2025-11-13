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
import lucuma.core.util.TimeSpan
import navigate.epics.VerifiedEpics.VerifiedEpics
import navigate.model.WfsConfiguration
import navigate.server.ApplyCommandResult
import navigate.server.ConnectionTimeout
import navigate.server.tcs.TcsSouthControllerEpics.CombinedOiwfsStatus
import navigate.server.tcs.TcsSouthControllerEpics.F2oiSaving
import navigate.server.tcs.TcsSouthControllerEpics.GmoiSaving
import navigate.server.tcs.TcsSouthControllerEpics.OiExposureTime
import navigate.server.tcs.TcsSouthControllerEpics.OiName
import navigate.server.tcs.TcsSouthControllerEpics.OiStatusEvent
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.FiniteDuration

class TcsSouthControllerEpics[F[_]: {Async, Parallel, Logger}](
  sys:      EpicsSystems.EpicsSystemsSouth[F],
  timeout:  FiniteDuration,
  stateRef: Ref[F, TcsBaseControllerEpics.State]
) extends TcsBaseControllerEpics[F](
      sys,
      timeout,
      stateRef
    )
    with TcsSouthController[F] {

  override def getInstrumentPorts: F[InstrumentPorts] = (for {
    f2F <- sys.ags.status.flamingos2Port
    ghF <- sys.ags.status.ghostPort
    gmF <- sys.ags.status.gmosPort
    gsF <- sys.ags.status.gsaoiPort
  } yield for {
    f2 <- f2F
    gh <- ghF
    gm <- gmF
    gs <- gsF
  } yield InstrumentPorts(
    flamingos2Port = f2,
    ghostPort = gh,
    gmosPort = gm,
    gnirsPort = 0,
    gpiPort = 0,
    gsaoiPort = gs,
    igrins2Port = 0,
    nifsPort = 0,
    niriPort = 0
  )).verifiedRun(ConnectionTimeout)

  override def oiwfsDaytimeGains: VerifiedEpics[F, F, ApplyCommandResult] = sys.oiwfs
    .startGainCommand(timeout)
    .gains
    .setTipGain(0.0)
    .gains
    .setTiltGain(0.0)
    .gains
    .setFocusGain(0.0)
    .gains
    .setScaleGain(TcsSouthControllerEpics.DefaultOiwfsScaleGain)
    .post

  override def oiwfsCircularBuffer(enable: Boolean): F[ApplyCommandResult] =
    sys.ags.status.oiwfsName.verifiedRun(ConnectionTimeout).flatMap {
      case Some(Instrument.GmosSouth) | Some(Instrument.GmosNorth) =>
        wfsCircularBuffer(sys.gmosOiwfs, enable)
      case Some(Instrument.Flamingos2)                             => wfsCircularBuffer(sys.f2Oiwfs, enable)
      case _                                                       => ApplyCommandResult.Completed.pure[F]
    }

  override def getOiwfsConfig: F[WfsConfiguration] =
    sys.ags.status.oiwfsName.verifiedRun(ConnectionTimeout).flatMap {
      case Some(Instrument.GmosSouth) | Some(Instrument.GmosNorth) =>
        getWfsConfig(sys.oiwfs, sys.gmosOiwfs)
      case Some(Instrument.Flamingos2)                             => getWfsConfig(sys.oiwfs, sys.f2Oiwfs)
      case _                                                       => WfsConfiguration.default.pure[F]
    }

  override def oiwfsConfigStream: F[fs2.Stream[F, WfsConfiguration]] = {
    val startVal: VerifiedEpics[F, F, CombinedOiwfsStatus] = for {
      oiName  <- sys.ags.status.oiwfsName
      exp     <- sys.oiwfs.getIntegrationTime
      gmosSav <- sys.gmosOiwfs.circularBufferStatus.map(_.map(_.imageEnabled))
      f2Sav   <- sys.f2Oiwfs.circularBufferStatus.map(_.map(_.imageEnabled))
    } yield for {
      t  <- exp.attempt.map(_.toOption)
      oi <- oiName.attempt.map(_.toOption)
      gm <- gmosSav.attempt.map(_.toOption)
      f2 <- f2Sav.attempt.map(_.toOption)
    } yield CombinedOiwfsStatus(t, oi, gm, f2)

    val streamsRes: VerifiedEpics[
      F,
      Resource[F, *],
      (Stream[F, TimeSpan], Stream[F, Option[Instrument]], Stream[F, Boolean], Stream[F, Boolean])
    ] = for {
      expS    <- sys.oiwfs.integrationTimeStream
      oiNameS <- sys.ags.status.oiwfsNameStream
      gmSaveS <- sys.gmosOiwfs.imgCircularBufferStrean
      f2SaveS <- sys.f2Oiwfs.imgCircularBufferStrean
    } yield (expS, oiNameS, gmSaveS, f2SaveS).mapN((_, _, _, _))

    val streams: VerifiedEpics[F, Resource[F, *], Stream[F, OiStatusEvent]] =
      streamsRes.map(_.map { (a, b, c, d) =>
        Stream(a.map(OiExposureTime.apply),
               b.map(OiName.apply),
               c.map(GmoiSaving.apply),
               d.map(F2oiSaving.apply)
        ).parJoinUnbounded
      })

    (
      for {
        v0  <- startVal
        ssr <- streams
      } yield ssr.use(ss =>
        v0.map(
          ss.scan(_) { (current, update) =>
            update match {
              case OiExposureTime(t) => current.copy(expTime = t.some)
              case OiName(ins)       => current.copy(oiSel = ins.some)
              case GmoiSaving(en)    => current.copy(gmoiSaving = en.some)
              case F2oiSaving(en)    => current.copy(f2oiSaving = en.some)
            }
          }.map { x =>
            for {
              t   <- x.expTime
              sav <- x.oiSel.flatten.flatMap {
                       case Instrument.GmosSouth | Instrument.GmosNorth => x.gmoiSaving
                       case Instrument.Flamingos2                       => x.f2oiSaving
                       case _                                           => none
                     }
            } yield WfsConfiguration(t, sav)
          }.flattenOption
        )
      )
    ).verifiedRun(ConnectionTimeout)
  }
}

object TcsSouthControllerEpics {

  def build[F[_]: {Async, Parallel, Logger}](
    sys:     EpicsSystems.EpicsSystemsSouth[F],
    timeout: FiniteDuration
  ): F[TcsSouthControllerEpics[F]] =
    Ref
      .of[F, TcsBaseControllerEpics.State](TcsBaseControllerEpics.State.default)
      .map(
        new TcsSouthControllerEpics(sys, timeout, _)
      )

  val DefaultOiwfsScaleGain: Double = 0.0003

  case class CombinedOiwfsStatus(
    expTime:    Option[TimeSpan],
    oiSel:      Option[Option[Instrument]],
    gmoiSaving: Option[Boolean],
    f2oiSaving: Option[Boolean]
  )

  sealed trait OiStatusEvent
  case class OiExposureTime(t: TimeSpan)     extends OiStatusEvent
  case class OiName(ins: Option[Instrument]) extends OiStatusEvent
  case class GmoiSaving(enabled: Boolean)    extends OiStatusEvent
  case class F2oiSaving(enabled: Boolean)    extends OiStatusEvent

}

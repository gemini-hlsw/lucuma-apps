// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.igrins2

import cats.effect.Async
import cats.effect.Ref
import cats.syntax.all.*
import fs2.Stream
import lucuma.core.util.TimeSpan
import observe.model.Observation
import observe.model.dhs.ImageFileId
import observe.server.InstrumentControllerSim
import observe.server.keywords.GdsClient
import observe.server.keywords.KeywordBag
import observe.server.overrideLogMessage
import org.typelevel.log4cats.Logger

import java.time.temporal.ChronoUnit
import scala.concurrent.duration.*

final case class Igrins2ControllerSim[F[_]: {Async, Logger}] private (
  sim:       InstrumentControllerSim[F],
  configRef: Ref[F, Option[Igrins2Config]]
) extends Igrins2Controller[F]:

  override def gdsClient: GdsClient[F] = new GdsClient[F]:
    override def setKeywords(id: ImageFileId, ks: KeywordBag): F[Unit] =
      overrideLogMessage("IGRINS2", "setKeywords")

    override def openObservation(obsId: Observation.Id, id: ImageFileId, ks: KeywordBag): F[Unit] =
      overrideLogMessage("IGRINS2", "openObservation")

    override def closeObservation(id: ImageFileId): F[Unit] =
      overrideLogMessage("IGRINS2", "closeObservation")

  override def applyConfig(config: Igrins2Config): F[Unit] =
    configRef.set(config.some) *>
      sim.applyConfig(config.configuration.toString)

  override def observe(fileId: ImageFileId, expTime: TimeSpan): F[ImageFileId] =
    configRef.get.flatMap: cfg =>
      val simTime = cfg.map(c => c.exposureTime +| c.readoutTime).getOrElse(expTime)
      sim.observe(fileId, simTime).as(fileId)

  override def endObserve: F[Unit] = sim.endObserve

  override def abort: F[Unit] = sim.abortObserve

  override def exposureProgress: F[Stream[F, Int]] =
    configRef.get.map: cfg =>
      val totalSecs = cfg.map(_.exposureTime.toSeconds.toDouble.ceil.toInt).getOrElse(0)
      Stream
        .iterate(totalSecs)(_ - 1)
        .zipLeft(Stream.awakeEvery(1.second))
        .takeWhile(_ >= 0)

  override def sequenceComplete: F[Unit] = configRef.set(none)

  override def requestedTime: F[Option[BigDecimal]] =
    configRef.get.map(_.map(_.exposureTime.toSeconds))

  override def currentStatus: F[Igrins2ControllerState] =
    Igrins2ControllerState.Idle.pure[F].widen

  override def dcIsPreparing: F[Boolean] = false.pure[F]

  override def dcIsAcquiring: F[Boolean] = true.pure[F]

  override def dcIsReadingOut: F[Boolean] = false.pure[F]

  override def dcIsWritingMEF: F[Boolean] = false.pure[F]

object Igrins2ControllerSim:
  def apply[F[_]: {Async, Logger}]: F[Igrins2Controller[F]] =
    Ref
      .of[F, Option[Igrins2Config]](none)
      .map: configRef =>
        Igrins2ControllerSim(
          InstrumentControllerSim.unsafeWithTimes(
            "IGRINS-2",
            TimeSpan.Zero,
            TimeSpan.unsafeFromDuration(1500, ChronoUnit.MILLIS),
            TimeSpan.unsafeFromDuration(5, ChronoUnit.SECONDS)
          ),
          configRef
        )

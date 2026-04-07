// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.igrins2

import cats.Applicative
import cats.implicits.*
import fs2.Stream
import lucuma.core.util.TimeSpan
import observe.model.dhs.ImageFileId
import observe.server.keywords.GdsClient
import observe.server.overrideLogMessage
import org.typelevel.log4cats.Logger

class Igrins2ControllerDisabled[F[_]: Logger: Applicative] extends Igrins2Controller[F]:
  private val name = "IGRINS2"

  override def exposureProgress: F[Stream[F, Int]] =
    Stream.emit[F, Int](0).pure[F]

  def requestedTime: F[Option[BigDecimal]] =
    none[BigDecimal].pure[F]

  def sequenceComplete: F[Unit] = Applicative[F].unit

  def currentStatus: F[Igrins2ControllerState] = Igrins2ControllerState.Idle.pure[F].widen

  def dcIsPreparing: F[Boolean] = false.pure[F]

  def dcIsAcquiring: F[Boolean] = false.pure[F]

  def dcIsReadingOut: F[Boolean] = false.pure[F]

  def dcIsWritingMEF: F[Boolean] = false.pure[F]

  override def gdsClient: GdsClient[F] = GdsClient.loggingClient(name)

  override def applyConfig(config: Igrins2Config): F[Unit] = overrideLogMessage(name, "applyConfig")

  override def observe(fileId: ImageFileId, expTime: TimeSpan): F[ImageFileId] =
    overrideLogMessage(name, s"observe $fileId").as(fileId)

  override def endObserve: F[Unit] = overrideLogMessage(name, "endObserve")

  override def abort: F[Unit] = overrideLogMessage(name, "abort")

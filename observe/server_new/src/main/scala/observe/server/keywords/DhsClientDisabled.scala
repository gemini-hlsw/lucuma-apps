// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.keywords

import cats.FlatMap
import cats.effect.Clock
import cats.syntax.all.*
import observe.model.dhs.ImageFileId
import observe.server.overrideLogMessage
import org.typelevel.log4cats.Logger

import java.time.Instant
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

class DhsClientDisabled[F[_]: {FlatMap, Clock, Logger}] extends DhsClient[F] {

  val format: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyyMMdd")

  override def createImage(p: DhsClient.ImageParameters): F[ImageFileId] =
    for {
      _    <- overrideLogMessage[F]("DHS", "setKeywords")
      date <- Clock[F].realTime
                .map(d => Instant.EPOCH.plusSeconds(d.toSeconds))
                .map(LocalDateTime.ofInstant(_, java.time.ZoneOffset.UTC))
    } yield ImageFileId(
      f"S${date.format(format)}S${date.getHour() * 360 + date.getMinute() * 6 + date.getSecond() / 10}%04d"
    )

  override def setKeywords(id: ImageFileId, keywords: KeywordBag, finalFlag: Boolean): F[Unit] =
    overrideLogMessage("DHS", "setKeywords")
}

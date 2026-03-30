// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.igrins2

import cats.MonadThrow
import cats.syntax.all.*
import lucuma.core.enums.ExecutionEnvironment
import observe.common.EventsGQL.RecordDatasetMutation.Data.RecordDataset.Dataset
import observe.model.Observation.Id
import observe.model.dhs.ImageFileId
import observe.model.enums.KeywordName
import observe.server.keywords.*
import observe.server.tcs.TcsKeywordsReader
import org.typelevel.log4cats.Logger

object Igrins2Header:

  def header[F[_]: MonadThrow: Logger](
    kwClient:          KeywordsClient[F],
    tcsKeywordsReader: TcsKeywordsReader[F]
  ): Header[F] =
    new Header[F]:
      override def sendBefore(
        obsId:       Id,
        id:          ImageFileId,
        dataset:     Option[Dataset.Reference],
        environment: ExecutionEnvironment
      ) =
        sendKeywords(
          id,
          kwClient,
          List(
            buildInt32(tcsKeywordsReader.igrins2InstPort, KeywordName.INPORT),
            buildString(tcsKeywordsReader.dateUT, KeywordName.DATE_OBS),
            buildString(s"$id.fits".pure[F], KeywordName.ORIGNAME)
          )
        )

      override def sendAfter(id: ImageFileId): F[Unit] =
        sendKeywords(
          id,
          kwClient,
          List(
            buildString(tcsKeywordsReader.hourAngle, KeywordName.HAEND),
            buildString(tcsKeywordsReader.date, KeywordName.DATEEND)
          )
        )

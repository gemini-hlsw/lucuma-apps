// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import boopickle.DefaultBasic.*
import cats.effect.Concurrent
import cats.syntax.all.*
import explore.model.ErrorMsgOr
import explore.model.boopickle.CatalogPicklers.given
import lucuma.catalog.BlindOffsetCandidate
import lucuma.catalog.BlindOffsets
import lucuma.catalog.clients.GaiaClient
import lucuma.core.geom.jts.interpreter.given
import lucuma.schemas.model.CoordinatesAt
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.syntax.*

object BlindOffsetRequestHandler:
  val cacheVersion = CacheVersion(1)

  def apply[F[_]: {Concurrent, Logger}](
    client:            GaiaClient[F],
    baseCoordinatesAt: CoordinatesAt,
    cache:             Cache[F],
    callback:          ErrorMsgOr[List[BlindOffsetCandidate]] => F[Unit]
  ): F[Unit] =
    def doRequest(coordsAt: CoordinatesAt): F[ErrorMsgOr[List[BlindOffsetCandidate]]] =
      BlindOffsets
        .runBlindOffsetAnalysis(
          client,
          coordsAt.coordinates,
          coordsAt.at
        )
        .map: list =>
          list.asRight
        .handleErrorWith: t =>
          error"Error getting blind offset candidates at ${coordsAt.at}: ${t.getMessage}" >>
            "Error getting blind offset candidates".asLeft.pure

    val cacheableRequest =
      Cacheable(
        CacheName("blindOffsetCandidates"),
        cacheVersion,
        doRequest,
        (_, result) => result.isRight
      )

    cache.eval(cacheableRequest).apply(baseCoordinatesAt).flatMap(callback)

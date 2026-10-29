// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import boopickle.DefaultBasic.*
import cats.effect.Concurrent
import cats.syntax.all.*
import explore.model.ErrorMsgOr
import explore.model.boopickle.CatalogPicklers.given
import lucuma.ags.GuideStarCandidate
import lucuma.catalog.clients.GaiaClient
import lucuma.catalog.votable.CatalogProblem
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.syntax.*

object GuideStarByIdRequestHandler:
  val cacheVersion = CacheVersion(2)

  def apply[F[_]: {Concurrent, Logger}](
    client:   GaiaClient[F],
    sourceId: Long,
    cache:    Cache[F],
    callback: ErrorMsgOr[Option[GuideStarCandidate]] => F[Unit]
  ): F[Unit] =
    def notFound(p: CatalogProblem): Boolean = p match
      case CatalogProblem.SourceIdNotFound(_) => true
      case _                                  => false

    val failed: ErrorMsgOr[Option[GuideStarCandidate]] =
      s"Error looking up guide star Gaia DR3 $sourceId".asLeft

    def doRequest(id: Long): F[ErrorMsgOr[Option[GuideStarCandidate]]] =
      client
        .queryByIdGuideStar(id)
        .flatMap:
          case Right(target)                               =>
            GuideStarCandidate.siderealTarget.get(target).some.asRight.pure
          case Left(problems) if problems.forall(notFound) =>
            none.asRight.pure
          case Left(problems)                              =>
            error"Error getting guide star $id: ${problems.map(_.displayValue).mkString_(", ")}"
              .as(failed)
        .handleErrorWith: t =>
          error"Error getting guide star $id: ${t.getMessage}".as(failed)

    // Gaia DR3 doesn't change, so a star that isn't there is cached too. Errors are retried.
    val cacheableRequest =
      Cacheable(CacheName("guideStarById"), cacheVersion, doRequest, (_, result) => result.isRight)

    cache.eval(cacheableRequest).apply(sourceId).flatMap(callback)

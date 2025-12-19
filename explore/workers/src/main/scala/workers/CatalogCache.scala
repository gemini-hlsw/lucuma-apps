// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import cats.Hash
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import explore.events.*
import japgolly.webapputil.indexeddb.*
import lucuma.ags
import lucuma.ags.GuideStarCandidate
import lucuma.catalog.clients.GaiaClient
import lucuma.catalog.votable.*
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.math.Coordinates
import lucuma.core.model.Target
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.syntax.*

import java.time.LocalDateTime
import java.time.ZoneOffset
import java.time.temporal.ChronoField
import java.time.temporal.ChronoUnit

trait CatalogQuerySettings {
  private val MaxTargets: Int   = 100
  private val CacheVersion: Int = 8

  protected given Hash[Coordinates] = Hash.fromUniversalHashCode
  protected given ADQLInterpreter   = ADQLInterpreter.nTarget(MaxTargets)

  protected def cacheQueryHash: Hash[ADQLQuery] =
    Hash.by(q => (MaxTargets, CacheVersion, q.toString))
}

/**
 * Handles the catalog cache, it tries to use the local db and if not it goes to gaia to get the
 * data
 */
trait CatalogCache extends CatalogIDB:

  /**
   * Try to read the gaia query from the cache or else get it from gaia
   */
  def readFromGaia(
    client:         GaiaClient[IO],
    idb:            Option[IndexedDb.Database],
    stores:         CacheIDBStores,
    request:        CatalogMessage.GSRequest,
    candidatesArea: ShapeExpression,
    respond:        List[GuideStarCandidate] => IO[Unit]
  )(using LF: LoggerFactory[IO]): IO[Unit] = {
    given Logger[IO] = LF.getLoggerFromName("catalog-cache")

    val CatalogMessage.GSRequest(tracking, obsTime, _) = request

    val brightnessConstraints = ags.widestConstraints

    val ldt   = LocalDateTime.ofInstant(obsTime, ZoneOffset.UTC)
    // We consider the query valid from the fist moment of the year to the end
    val start = ldt.`with`(ChronoField.DAY_OF_YEAR, 1L).`with`(ChronoField.NANO_OF_DAY, 0)
    val end   = start.plus(1, ChronoUnit.YEARS)

    // If tracking returns None at start and/or end, it's probably because a non-sidereal is
    // involved. In which case, we'll just use obs time.
    (tracking.at(start.toInstant(ZoneOffset.UTC)), tracking.at(end.toInstant(ZoneOffset.UTC)))
      .mapN(NonEmptyList.of(_, _))
      .orElse(tracking.at(obsTime).map(NonEmptyList.one))
      .map { coordsList =>
        // Make a query based on two coordinates of the base of an asterism over a year,
        // unless a non-sidereal is involved.
        val query: CoordinatesRangeQueryByADQL =
          CoordinatesRangeQueryByADQL(
            coordsList,
            candidatesArea,
            brightnessConstraints.some
          )

        info"requested catalog $query" *>
          // Try to find it in the db
          readGuideStarCandidatesFromCache(idb, stores, query)
            .toF[IO]
            .handleError(_ => none) // Try to find it in the db
            .flatMap(
              _.fold(
                // Not found in the db, re request
                client
                  .queryGuideStars(query)
                  .map(
                    _.collect { case Right(s) =>
                      GuideStarCandidate.siderealTarget.get(s)
                    }
                  )
                  .flatMap { candidates =>
                    info"Catalog results from remote catalog: ${candidates.length} candidates" *>
                      respond(candidates) *>
                      storeGuideStarCandidates(idb, stores, query, candidates)
                        .toF[IO]
                        .handleError(e => Logger[IO].error(e)("Error storing guidestar candidates"))
                  }
                  .void
              ) { c =>
                // Cache hit!
                info"Catalog results from cache: ${c.length} candidates" *>
                  respond(c)
              }
            )
      }
      .getOrElse(IO.unit)
  }

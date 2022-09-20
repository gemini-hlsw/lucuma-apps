// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import cats.Hash
import cats.data.EitherNec
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.effect.IO
import cats.syntax.all.*
import explore.events.*
import explore.model.Constants
import explore.model.boopickle.Boopickle.*
import explore.model.boopickle.*
import explore.model.boopickle.*
import fs2.text
import japgolly.webapputil.indexeddb.*
import lucuma.ags
import lucuma.ags.GuideStarCandidate
import lucuma.catalog.*
import lucuma.catalog.votable.*
import lucuma.core.enums.*
import lucuma.core.enums.*
import lucuma.core.geom.gmos.probeArm
import lucuma.core.geom.jts.interpreter.*
import lucuma.core.geom.jts.interpreter.*
import lucuma.core.math.Coordinates
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.Target
import org.http4s.Method.*
import org.http4s.Request
import org.http4s.client.Client
import org.http4s.syntax.all.*
import org.scalajs.dom
import org.typelevel.cats.time.*
import org.typelevel.log4cats.Logger
import spire.math.Bounded
import spire.math.Interval

import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneId
import java.time.temporal.ChronoField
import java.time.temporal.ChronoUnit

trait CatalogQuerySettings {
  val proxy = uri"https://cors-proxy.lucuma.xyz"

  val MaxTargets = 30000

  given Hash[Coordinates]            = Hash.fromUniversalHashCode
  given catalog: CatalogAdapter.Gaia = CatalogAdapter.Gaia3Lite
  given ci: ADQLInterpreter          = ADQLInterpreter.nTarget(MaxTargets)

  def cacheQueryHash: Hash[ADQLQuery] =
    Hash.by(q => (MaxTargets, catalog.gaiaDB, q.base, q.adqlGeom, q.adqlBrightness))

}

/**
 * Handles the catalog cache, it tries to use the local db and if not it goes to gaia to get the
 * data
 */
trait CatalogCache extends CatalogIDB {

  /**
   * Request and parse data from Gaia
   */
  def readFromGaia[F[_]: Concurrent](
    client: Client[F],
    query:  ADQLQuery
  ): F[List[EitherNec[CatalogProblem, Target.Sidereal]]] =
    val queryUri = CatalogSearch.gaiaSearchUri(query)
    val request  = Request[F](GET, queryUri)
    client
      .stream(request)
      .flatMap(
        _.body
          .through(text.utf8.decode)
          .through(CatalogSearch.guideStars[F](CatalogAdapter.Gaia3Lite))
      )
      .compile
      .toList

  /**
   * Try to read the gaia query from the cache or else get it from gaia
   */
  def readFromGaia(
    client:  Client[IO],
    idb:     Option[IndexedDb.Database],
    stores:  CacheIDBStores,
    request: CatalogMessage.GSRequest,
    respond: List[GuideStarCandidate] => IO[Unit]
  )(using Logger[IO]): IO[Unit] = {
    val CatalogMessage.GSRequest(tracking, obsTime) = request

    val brightnessConstraints = ags.widestConstraints

    val ldt   = LocalDateTime.ofInstant(obsTime, Constants.UTC)
    // We consider the query valid from the fist moment of the year to the end
    val start = ldt.`with`(ChronoField.DAY_OF_YEAR, 1L).`with`(ChronoField.NANO_OF_DAY, 0)
    val end   = start.plus(1, ChronoUnit.YEARS)

    (tracking.at(start.toInstant(Constants.UTCOffset)),
     tracking.at(end.toInstant(Constants.UTCOffset))
    ).mapN { (a, b) =>
      // Make a query based on two coordinates of the base of an asterism over a year
      val query = CoordinatesRangeQueryByADQL(
        NonEmptyList.of(a, b),
        probeArm.candidatesArea,
        brightnessConstraints.some,
        proxy.some
      )

      (Logger[IO].debug(s"Requested catalog $query ${cacheQueryHash.hash(query)}") *>
        // Try to find it in the db
        readGuideStarCandidates(idb, stores, query)
          .toF[IO]
          .handleError(_ => none)) // Try to find it in the db
        .flatMap(
          _.fold(
            // Not found in the db, re request
            readFromGaia[IO](client, query)
              .map(
                _.collect { case Right(s) =>
                  GuideStarCandidate.siderealTarget.get(s)
                }.map(gsc =>
                  // Do PM correction
                  gsc.at(request.vizTime)
                )
              )
              .flatMap { candidates =>
                Logger[IO].debug(
                  s"Catalog results from remote catalog: ${candidates.length} candidates"
                ) *>
                  respond(candidates) *>
                  storeGuideStarCandidates(idb, stores, query, candidates)
                    .toF[IO]
                    .handleError(e => Logger[IO].error(e)("Error storing guidestar candidates"))
              }
              .void
          ) { c =>
            // Cache hit!
            Logger[IO].debug(s"Catalog results from cache: ${c.length} candidates") *>
              respond(c)
          }
        )
    }.getOrElse(IO.unit)
  }
}

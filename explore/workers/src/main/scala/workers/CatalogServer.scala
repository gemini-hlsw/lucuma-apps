// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import boopickle.DefaultBasic.*
import cats.effect.IO
import cats.effect.Ref
import cats.effect.unsafe.implicits.*
import cats.syntax.all.*
import explore.events.CatalogMessage
import explore.events.CatalogMessage.BlindOffsetRequest
import explore.model.AppConfig
import explore.model.boopickle.CatalogPicklers.given
import japgolly.webapputil.indexeddb.IndexedDb
import lucuma.catalog.clients.GaiaClient
import lucuma.catalog.simbad.SEDDataLoader
import lucuma.catalog.simbad.SEDMatcher
import org.http4s.Uri
import org.http4s.client.middleware.RequestLogger
import org.http4s.dom.FetchClientBuilder
import org.scalajs.dom
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.otel4s.trace.Tracer
import org.typelevel.otel4s.trace.TracerProvider

import java.net.URLDecoder
import java.nio.charset.StandardCharsets
import java.time.Duration
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("CatalogServer", moduleID = "exploreworkers")
object CatalogServer extends WorkerServer[CatalogMessage.Request] with CatalogCache:
  @JSExport
  def runWorker(): Unit = run.unsafeRunAndForget()

  // Expire the data in 30 days
  private val Expiration: Duration           = Duration.ofDays(30)
  private val RequestTimeout: FiniteDuration = FiniteDuration(300, TimeUnit.SECONDS)

  protected def handler(
    config: Option[AppConfig]
  ): (LoggerFactory[IO], Tracer[IO], TracerProvider[IO]) ?=> IO[Invocation => IO[Unit]] =
    for {
      self      <- IO(dom.DedicatedWorkerGlobalScope.self)
      idb       <- IO(self.indexedDB.toOption)
      stores     = CacheIDBStores()
      cacheDb   <- idb.traverse(idb => stores.open(IndexedDb(idb)).toF[IO])
      cache     <- Cache.withIDB[IO](idb, "explore-gaia") // blind offsets and single guide stars
      rawClient  = FetchClientBuilder[IO].withRequestTimeout(RequestTimeout).create
      log       <- IO(LoggerFactory[IO].getLoggerFromName("catalog-server"))
      httpClient = RequestLogger(
                     logHeaders = true,
                     logBody = false,
                     logAction = Some((msg: String) =>
                       log.info(URLDecoder.decode(msg, StandardCharsets.UTF_8))
                     )
                   )(rawClient)
      client     = GaiaClient.build(httpClient)
      sedRef    <- Ref.of[IO, Option[SEDMatcher]](none)
      // Keep only a successful load so a failed fetch is retried on the next request
      sedMatcher = sedRef.get.flatMap(
                     _.fold(
                       SEDDataLoader
                         .loadMatcher[IO](httpClient, Uri.unsafeFromString(self.location.origin))
                         .flatTap(m => sedRef.set(m.some))
                     )(IO.pure)
                   )
    } yield invocation =>
      invocation.data match
        case CatalogMessage.CleanCache =>
          cacheDb.traverse(stores.clean(_).toF[IO]).void *> cache.clear

        case req @ CatalogMessage.GSRequest(_, _, guideProbe) =>
          readFromGaia(client, cacheDb, stores, req, c => invocation.respond(c)) *>
            expireGuideStarCandidates(cacheDb, stores, Expiration)
              .toF[IO]
              .handleErrorWith(e => log.error(e)("Error expiring guidestar candidates"))

        case CatalogMessage.GSByIdRequest(sourceId) =>
          given Logger[IO] = log
          GuideStarByIdRequestHandler(client, sourceId, cache, invocation.respond(_))

        case CatalogMessage.GSCacheCleanupRequest(expTime) =>
          expireGuideStarCandidates(cacheDb, stores, expTime)
            .toF[IO]
            .handleErrorWith(e => log.error(e)("Error expiring guidestar candidates"))

        case BlindOffsetRequest(baseCoordinatesAt, instrument) =>
          given Logger[IO] = log
          BlindOffsetRequestHandler(
            client,
            sedMatcher,
            baseCoordinatesAt,
            instrument,
            cache,
            invocation.respond(_)
          )

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import boopickle.DefaultBasic.*
import cats.effect.*
import cats.effect.unsafe.implicits.*
import explore.events.*
import explore.model.AppConfig
import explore.model.Constants.HorizonsProxyMod
import explore.model.boopickle.HorizonsPicklers
import lucuma.horizons.HorizonsClient
import org.http4s.client.Client
import org.http4s.dom.FetchClientBuilder
import org.scalajs.dom
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.otel4s.trace.Tracer
import org.typelevel.otel4s.trace.TracerProvider
import workers.horizons.HorizonsRequests

import java.time.Duration
import scala.concurrent.duration.*
import scala.scalajs.js

import js.annotation.*

/**
 * Web worker that can query horizons and store results locally
 */
@JSExportTopLevel("HorizonsServer", moduleID = "exploreworkers")
object HorizonsServer extends WorkerServer[HorizonsMessage.Request] with HorizonsPicklers:
  @JSExport
  def runWorker(): Unit = run.unsafeRunAndForget()

  private val CacheRetention: Duration = Duration.ofDays(30)

  // Deliberately untraced, as with the Gaia client in CatalogServer. The trace middleware injects
  // a `traceparent` header, which is not CORS-safelisted, so every request becomes a preflight
  // that gpp-horizons.noirlab.edu rejects (it does not list the header in its
  // Access-Control-Allow-Headers). Propagating it would buy nothing anyway: neither the proxy nor
  // JPL joins our traces, and the enclosing `worker.handle` span already times the call.
  private def createClient[F[_]: Async]: Client[F] =
    FetchClientBuilder[F].withRequestTimeout(10.seconds).create

  protected def handler(
    config: Option[AppConfig]
  ): (LoggerFactory[IO], Tracer[IO], TracerProvider[IO]) ?=> IO[Invocation => IO[Unit]] = {
    given Logger[IO] = LoggerFactory[IO].getLoggerFromName("horizons-server")

    for {
      self  <- IO(dom.DedicatedWorkerGlobalScope.self)
      cache <- Cache.withIDB[IO](self.indexedDB.toOption, "explore-horizons")
      _     <- cache.evict(CacheRetention).start
      client =
        HorizonsClient[IO](
          createClient[IO],
          modUri = HorizonsProxyMod
        )
    } yield { invocation =>
      invocation.data match
        case HorizonsMessage.CleanCache =>
          cache.clear *> invocation.respond(())

        case req: HorizonsMessage.EphemerisRequest =>
          HorizonsRequests.ephemerisRequest[IO](req, client, cache, invocation.respond(_))

        case req: HorizonsMessage.AlignedEphemerisRequest =>
          HorizonsRequests.alignedEphemerisRequest[IO](req, client, cache, invocation.respond(_))
    }
  }

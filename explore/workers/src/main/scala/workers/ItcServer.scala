// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import boopickle.DefaultBasic.*
import cats.effect.*
import cats.effect.unsafe.implicits.*
import cats.syntax.all.*
import clue.js.FetchJsBackend
import clue.js.FetchJsClient
import clue.js.given
import clue.otel4s.Otel4sMiddleware
import explore.events.*
import explore.itc.ITCGraphRequests
import explore.itc.ITCRequests
import explore.itc.ITCVersionsRequests
import explore.model.AppConfig
import explore.model.boopickle.ItcPicklers
import lucuma.itc.client.ItcClient
import org.http4s.Uri
import org.scalajs.dom
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.syntax.*
import org.typelevel.otel4s.trace.Tracer
import org.typelevel.otel4s.trace.TracerProvider

import java.time.Duration
import scala.concurrent.duration.*
import scala.scalajs.js

import js.annotation.*

/**
 * Web worker that can query gaia and store results locally
 */
@JSExportTopLevel("ItcServer", moduleID = "exploreworkers")
object ItcServer extends WorkerServer[ItcMessage.Request] with ItcPicklers {
  @JSExport
  def runWorker(): Unit = run.unsafeRunAndForget()

  private val CacheRetention: Duration = Duration.ofDays(30)

  private def createItcClient[F[_]: {Async, Logger, Tracer}](uri: Uri): F[ItcClient[F]] =
    given FetchJsBackend[F] = FetchJsBackend[F]()
    FetchJsClient
      .of[F, Unit](uri.toString, "ITC")
      .map(Otel4sMiddleware(_))
      .flatMap(client => ItcClient.create[F](client))

  protected def handler(
    config: Option[AppConfig]
  ): (LoggerFactory[IO], Tracer[IO], TracerProvider[IO]) ?=> IO[Invocation => IO[Unit]] = {
    given Logger[IO] = LoggerFactory[IO].getLoggerFromName("itc-server")

    for {
      self      <- IO(dom.DedicatedWorkerGlobalScope.self)
      cache     <- Cache.withIDB[IO](self.indexedDB.toOption, "explore-itc")
      _         <- cache.evict(CacheRetention).start
      itcClient <- Deferred[IO, ItcClient[IO]]
    } yield { invocation =>
      invocation.data match
        case ItcMessage.Initialize =>
          (for {
            itcURI <- IO.fromOption(config.map(_.itcURI))(
                        new Exception("Could not read the ITC URI from the configuration")
                      )
            client <- createItcClient[IO](itcURI)
            _      <- itcClient.complete(client).void
            _      <- ITCVersionsRequests.queryItc[IO](cache, client).andWait(1.hour).foreverM.start
          } yield ()).attempt.flatMap {
            case Right(_)  =>
              debug"ITC client initialized successfully" >>
                invocation.respond(none[String])
            case Left(err) =>
              error"Failed to initialize ITC client" >>
                invocation.respond(
                  Some(s"ITC initialization failed: ${err.getMessage}")
                )
          }

        case ItcMessage.CleanCache =>
          cache.clear *> invocation.respond(())

        case ItcMessage.ItcQuery(
              constraint,
              targets,
              customSedTimestamps,
              rows
            ) =>
          debug"ITC query ${rows.length}" >>
            itcClient.get >>= (implicit client =>
            ITCRequests
              .queryItc[IO](
                constraint,
                targets,
                customSedTimestamps,
                rows,
                cache,
                r => invocation.respond(r)
              )
          )

        case ItcMessage.ItcGraphQuery(
              constraint,
              targets,
              customSedTimestamps,
              mode
            ) =>
          debug"ITC graph query $mode" >>
            itcClient.get >>= (implicit client =>
            ITCGraphRequests
              .queryItc[IO](
                constraint,
                targets,
                customSedTimestamps,
                mode,
                cache,
                r => invocation.respond(r)
              )
          )
    }
  }
}

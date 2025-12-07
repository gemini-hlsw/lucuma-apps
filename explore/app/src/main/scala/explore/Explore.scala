// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.Async
import cats.effect.IO
import cats.effect.Resource
import cats.effect.Sync
import cats.effect.kernel.Deferred
import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import clue.js.FetchJsBackend
import clue.js.FetchMethod
import clue.js.WebSocketJsBackend
import clue.websocket.ReconnectionStrategy
import explore.components.ui.ExploreStyles
import explore.events.*
import explore.events.ItcMessage.given
import explore.model.AppConfig
import explore.model.AppContext
import explore.model.Focused
import explore.model.RootModel
import explore.model.RoutingInfo
import explore.model.WorkerClients
import explore.model.enums.AppTab
import explore.utils.ToastCtx
import fs2.dom.BroadcastChannel
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.*
import log4cats.loglevel.LogLevelLogger
import org.typelevel.log4cats.syntax.*
import lucuma.core.model.Program
import lucuma.react.primereact.Message
import lucuma.react.primereact.ToastRef
import lucuma.ui.sso.UserVault
import lucuma.ui.utils.showEnvironment
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.dom.FetchClientBuilder
import org.scalajs.dom
import org.scalajs.dom.Element
import org.typelevel.log4cats.Logger

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.*
import scala.scalajs.js

import js.annotation.*

@JSExportTopLevel("Explore", moduleID = "explore")
object ExploreMain {

  @JSExport
  def runIOApp(configJson: String): Unit =
    run(configJson).unsafeRunAndForget()

  def setupLogger[F[_]: Sync]: F[Logger[F]] = Sync[F].delay {
    LogLevelLogger.setLevel(LogLevelLogger.Level.INFO)
    LogLevelLogger.createForRoot[F]
  }

  private def buildNonCachingHttpClient[F[_]: Async]: Client[F] =
    FetchClientBuilder[F]
      .withRequestTimeout(4.seconds)
      .withCache(dom.RequestCache.`no-store`)
      .create

  def initialModel(vault: Option[UserVault]): RootModel =
    RootModel(vault = vault)

  def setupDOM[F[_]: Sync]: F[Element] = Sync[F].delay(
    Option(dom.document.getElementById("root")).getOrElse {
      val elem = dom.document.createElement("div")
      elem.id = "root"
      dom.document.body.appendChild(elem)
      elem
    }
  )

  def crash[F[_]: Sync](msg: String): F[Unit] =
    setupDOM[F].map { element =>
      (ExploreStyles.CrashMessage |+| ExploreStyles.ErrorLabel).value
        .foreach(element.classList.add)
      element.innerHTML = msg
    }

  def run(configJson: String): IO[Unit] = {

    val reconnectionStrategy: ReconnectionStrategy =
      (attempt, _) =>
        // Increase the delay to get exponential backoff with a minimum of 1s and a max of 1m
        // TODO If it's a Not authorized, do not backoff, retry on constant period.
        FiniteDuration(
          math.min(60.0, math.pow(2, attempt.toDouble - 1)).toLong,
          TimeUnit.SECONDS
        ).some

    def initializeItc(
      workerClients: WorkerClients[IO],
      itcURI:        Uri,
      toastCtx:      ToastCtx[IO]
    )(using Logger[IO]): IO[Unit] =
      workerClients.itc
        .requestSingle(ItcMessage.Initialize(itcURI))
        .flatMap:
          case Some(Some(error)) =>
            toastCtx.showToast(error, Message.Severity.Error, true)
          case Some(None)        =>
            info"ITC client initialized"
          case None              =>
            warn"ITC initialization: no response from worker"
        .start
        .void

    def buildPage(
      dispatcher:    Dispatcher[IO],
      workerClients: WorkerClients[IO],
      bc:            BroadcastChannel[IO, ExploreEvent],
      configJson:    String
    )(using Logger[IO]): IO[Unit] = {
      given FetchJsBackend[IO]     = FetchJsBackend[IO](FetchMethod.GET)
      given WebSocketJsBackend[IO] = WebSocketJsBackend[IO](dispatcher)

      val (router, routerCtl) =
        RouterWithProps.componentAndCtl(BaseUrl.fromWindowOrigin, Routing.config)

      def pageUrl(location: Option[(AppTab, Program.Id, Focused)]): String =
        routerCtl.urlFor(RoutingInfo.getPage(location)).value

      def setPageVia(
        location: Option[(AppTab, Program.Id, Focused)],
        via:      SetRouteVia
      ) =
        routerCtl.set(RoutingInfo.getPage(location), via)

      for {
        host                 <- IO(dom.window.location.host)
        appConfig             = AppConfig.parseConf(host, configJson)
        httpClient            = buildNonCachingHttpClient[IO]
        _                    <- info"Git Commit: [${utils.gitHash.getOrElse("NONE")}]"
        _                    <- info"Config: ${appConfig.show}"
        toastRef             <- Deferred[IO, ToastRef]
        ctx                  <- AppContext.from[IO](
                                  appConfig,
                                  reconnectionStrategy,
                                  pageUrl,
                                  setPageVia,
                                  workerClients,
                                  httpClient,
                                  bc,
                                  toastRef
                                )
        _                    <- initializeItc(workerClients, appConfig.itcURI, ctx.toastCtx)
        r                    <- (ctx.sso.whoami, setupDOM[IO], showEnvironment[IO](appConfig.environment)).parTupled
        (vault, container, _) = r
      } yield ReactDOMClient
        .createRoot(container)
        .render:
          RootComponent(ctx, router, initialModel(vault))

    }.void
      .handleErrorWith { t =>
        Logger[IO].error("Error initializing") >>
          crash[IO](s"There was an error initializing Explore:<br/>${t.getMessage}")
      }

    (for {
      dispatcher       <- Dispatcher.parallel[IO]
      given Logger[IO] <- Resource.eval(setupLogger[IO])
      workerClients    <- WorkerClients.build[IO](dispatcher)
      bc               <- BroadcastChannel[IO, ExploreEvent]("explore")
      _                <- Resource.eval(buildPage(dispatcher, workerClients, bc, configJson))
    } yield ()).useForever
  }

}

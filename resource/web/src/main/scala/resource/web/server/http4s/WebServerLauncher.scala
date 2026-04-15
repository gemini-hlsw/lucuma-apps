// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.web.server.http4s

import cats.effect.*
import cats.effect.std.Console
import cats.effect.syntax.all.*
import cats.syntax.all.*
import fs2.*
import fs2.compression.Compression
import fs2.io.file.Files
import fs2.io.net.*
import grackle.Schema
import grackle.skunk.SkunkMonitor
import natchez.Trace.Implicits.noop
import org.http4s.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.*
import org.http4s.server.websocket.WebSocketBuilder2
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import pureconfig.ConfigObjectSource
import pureconfig.ConfigSource
import resource.model.config.*
import resource.web.server.BuildInfo
import resource.web.server.config.*
import skunk.*

import java.nio.file.Files as JavaFiles

import ServerMiddleware.Middleware

object ResourceMain extends IOApp.Simple {

  override def run: IO[Unit] = resource[IO].useForever

  private def config[F[_]: {Sync, Logger}]: F[ConfigObjectSource] =
    for
      confDir    <- baseDir[F].map(_.resolve("conf"))
      secretsConf = confDir.resolve("local").resolve("secrets.conf")
      systemsConf = confDir.resolve("local").resolve("systems.conf")
      site        = sys.env.get("SITE").getOrElse(sys.error("SITE environment variable not set"))
      siteConf    = confDir.resolve(site).resolve("site.conf")
      _          <- Logger[F].info("Loading configuration:")
      _          <- Logger[F].info:
                      s" - $systemsConf (present: ${JavaFiles.exists(systemsConf)}), with fallback:"
      _          <- Logger[F].info:
                      s" - $secretsConf (present: ${JavaFiles.exists(secretsConf)}), with fallback:"
      _          <- Logger[F].info(s" - $siteConf (present: ${JavaFiles.exists(siteConf)}), with fallback:")
      _          <- Logger[F].info(s" - <resources>/base.conf")
    yield ConfigSource
      .file(systemsConf)
      .optional
      .withFallback:
        ConfigSource
          .file(secretsConf)
          .optional
          .withFallback:
            ConfigSource.file(siteConf).optional.withFallback(ConfigSource.resources("base.conf"))

  def databasePoolResource[F[_]: {Temporal, Console, Network}](
    config: DatabaseConfiguration
  ): Resource[F, Resource[F, Session[F]]] =
    Session.pooled[F](
      host = config.host,
      port = config.port,
      user = config.user,
      password = Some(config.password),
      database = config.database,
      ssl = SSL.Trusted.withFallback(true),
      max = config.maxConnections
    )

  private def printBanner[F[_]: Logger](conf: ResourceConfiguration): F[Unit] = {
    val runtime    = Runtime.getRuntime
    val memorySize = java.lang.management.ManagementFactory
      .getOperatingSystemMXBean()
      .asInstanceOf[com.sun.management.OperatingSystemMXBean]
      .getTotalMemorySize()

    val banner = """
▄▄▄▄▄▄▄                                             
███▀▀███▄                                           
███▄▄███▀ ▄█▀█▄ ▄█▀▀▀ ▄███▄ ██ ██ ████▄ ▄████ ▄█▀█▄ 
███▀▀██▄  ██▄█▀ ▀███▄ ██ ██ ██ ██ ██ ▀▀ ██    ██▄█▀ 
███  ▀███ ▀█▄▄▄ ▄▄▄█▀ ▀███▀ ▀██▀█ ██    ▀████ ▀█▄▄▄ 

"""

    val msg =
      s"""Starting Resource Server
          | Site               : ${conf.site}
          | Version            : ${BuildInfo.version}
          | Web server         : ${conf.webServer.host}:${conf.webServer.port}
          | External URL       : ${conf.webServer.externalBaseUrl}
          |
          | cores              : ${runtime.availableProcessors()}
          | current JVM memory : ${runtime.totalMemory() / 1024 / 1024} MB
          | maximum JVM memory : ${runtime.maxMemory() / 1024 / 1024} MB
          | total RAM          : ${memorySize / 1024 / 1024} MB
          | java version       : ${System.getProperty("java.version")}
          |""".stripMargin

    Logger[F].info(banner + msg)
  }

  def resource[F[_]: {Async, Console, Compression, Files, Network}]: Resource[F, Unit] =
    for
      given Logger[F] = Slf4jLogger.getLoggerFromName[F]("resource")
      conf           <- Resource.eval(config[F].flatMap(loadConfiguration[F]))
      _              <- Resource.eval(printBanner(conf))
      s              <- webServer(conf)
    yield ()

  def webServer[F[_]: {Async, Logger, Compression, Console, Files, Network}](
    conf: ResourceConfiguration
  ): Resource[F, Server] = {
    def routes(pool: Resource[F, Session[F]], monitor: SkunkMonitor[F], schema: Schema)(
      wsb: WebSocketBuilder2[F]
    ): HttpRoutes[F]         =
      Router[F](
        "/"         -> new StaticRoutes().service,
        "/resource" -> new GraphQlRoutes().service(wsb, pool, monitor, schema)
      )

    def server(
      app:        WebSocketBuilder2[F] => HttpRoutes[F],
      middleware: Middleware[F]
    ): Resource[F, Server] = EmberServerBuilder
      .default[F]
      .withHost(conf.webServer.host)
      .withPort(conf.webServer.port)
      .withHttpWebSocketApp(wsb => middleware(app(wsb)).orNotFound)
      .build

    for
      pool      <- databasePoolResource(conf.database)
      schema    <- GraphQlRoutes.loadSchema[F].toResource
      r          = routes(pool, SkunkMonitor.noopMonitor[F], schema)
      middleware = ServerMiddleware()
      s         <- server(r, middleware)
    yield s
  }
}

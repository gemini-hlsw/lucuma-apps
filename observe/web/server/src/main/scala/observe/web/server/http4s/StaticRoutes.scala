// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.web.server.http4s

import cats.data.NonEmptyList
import cats.data.OptionT
import cats.effect.Sync
import cats.syntax.all.*
import fs2.compression.Compression
import fs2.io.file.Files
import fs2.io.file.Path
import org.http4s.CacheDirective.*
import org.http4s.Header
import org.http4s.HttpRoutes
import org.http4s.Request
import org.http4s.Response
import org.http4s.StaticFile
import org.http4s.Uri
import org.http4s.headers.`Cache-Control`
import org.http4s.server.middleware.GZip

import scala.concurrent.duration.*

class StaticRoutes[F[_]: {Sync, Compression, Files}]:
  private val AppDir: String = "app"

  private val OneYear: Int = 365 * 24 * 60 * 60 // One year in seconds

  private val CacheHeaders: List[Header.ToRaw] = List(
    `Cache-Control`(NonEmptyList.of(`max-age`(OneYear.seconds)))
  )

  def localFile(path: String, req: Request[F]): OptionT[F, Response[F]] =
    OptionT
      .liftF(baseDir)
      .flatMap: dir =>
        StaticFile.fromPath(
          Path.fromNioPath(dir.resolve(AppDir).resolve(path.stripPrefix("/"))),
          req.some
        )

  extension (req: Request[F])
    def endsWith(exts: String*): Boolean = exts.exists(req.pathInfo.toString.endsWith)

    def serve(path: String): F[Response[F]] =
      localFile(path, req)
        .map(_.putHeaders(CacheHeaders*))
        .getOrElse(Response.notFound[F])

  private val supportedExtension = List(
    ".html",
    ".js",
    ".map",
    ".css",
    ".png",
    ".eot",
    ".svg",
    ".woff",
    ".woff2",
    ".ttf",
    ".mp3",
    ".ico",
    ".webm",
    ".json"
  )

  def service: HttpRoutes[F] = GZip:
    HttpRoutes.of[F]:
      case req if req.pathInfo === Uri.Path.Root       => req.serve("/index.html")
      case req if req.endsWith(supportedExtension*)    => req.serve(req.pathInfo.toString)
      // This maybe not desired in all cases but it helps to keep client side routing cleaner
      case req if !req.pathInfo.toString.contains(".") => req.serve("/index.html")

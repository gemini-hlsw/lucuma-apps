// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.web.server.http4s

import cats.data.NonEmptyList
import cats.data.OptionT
import cats.effect.Async
import cats.syntax.all.*
import fs2.compression.Compression
import fs2.io.file.Files
import org.http4s.CacheDirective
import org.http4s.CacheDirective.*
import org.http4s.Header
import org.http4s.HttpRoutes
import org.http4s.Request
import org.http4s.Response
import org.http4s.Status
import org.http4s.Uri
import org.http4s.dsl.io.*
import org.http4s.headers.`Cache-Control`
import org.http4s.server.middleware.GZip
import org.http4s.server.staticcontent.FileService
import org.http4s.server.staticcontent.fileService

import java.nio.file.Path
import scala.concurrent.duration.*

class StaticRoutes[F[_]: {Async, Compression, Files}](baseDir: Path):
  private val AppDir: String = "app"

  private val IndexPath: Uri.Path = Uri.Path.Root / "index.html"

  private val ApiPath: Uri.Path = Uri.Path.Root / "api"

  private val OneYear: FiniteDuration = 365.days

  private val CacheHeaders: List[Header.ToRaw] = List(
    `Cache-Control`(NonEmptyList.of(`max-age`(OneYear)))
  )

  // Cache index pages for a short time to avoid stale content
  private val IndexCacheHeaders: List[Header.ToRaw] = List(
    `Cache-Control`(NonEmptyList.of(`max-age`(6.hours), `must-revalidate`))
  )

  // /assets/* files are fingerprinted and can be cached for a long time
  private val immutable                             = CacheDirective("immutable")
  private val AssetCacheHeaders: List[Header.ToRaw] = List(
    `Cache-Control`(NonEmptyList.of(`public`, `max-age`(OneYear), immutable))
  )

  private val supportedExtension = List(
    ".avif",
    ".css",
    ".eot",
    ".html",
    ".ico",
    ".jpg",
    ".js",
    ".json",
    ".map",
    ".mp3",
    ".png",
    ".svg",
    ".ttf",
    ".txt",
    ".wasm",
    ".webm",
    ".webp",
    ".woff",
    ".woff2"
  )

  private val files: HttpRoutes[F] =
    fileService[F](FileService.Config(systemPath = baseDir.resolve(AppDir).toString))

  extension (req: Request[F])
    def isStaticAsset: Boolean = supportedExtension.exists(req.pathInfo.toString.endsWith)

  // Only files that were found get cache headers, not errors such as 400 or 416
  def serve(req: Request[F], headers: List[Header.ToRaw]): OptionT[F, Response[F]] =
    files(req).map: resp =>
      if resp.status === Status.Ok || resp.status === Status.NotModified then
        resp.putHeaders(headers*)
      else resp

  def service: HttpRoutes[F] = GZip:
    HttpRoutes[F]:
      case req @ (GET | HEAD) -> "assets" /: _ if req.isStaticAsset =>
        serve(req, AssetCacheHeaders)
      case req @ (GET | HEAD) -> _ if req.isStaticAsset             =>
        serve(req, CacheHeaders)
      // This maybe not desired in all cases but it helps to keep client side routing cleaner
      case req @ (GET | HEAD) -> path
          if !path.startsWith(ApiPath) && !path.toString.contains(".") =>
        serve(req.withPathInfo(IndexPath), IndexCacheHeaders)
      case _                                                        =>
        OptionT.none

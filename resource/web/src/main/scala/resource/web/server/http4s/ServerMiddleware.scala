// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.web.server.http4s
import cats.*
import cats.effect.*
import fs2.compression.Compression
import org.http4s.HttpRoutes
import org.http4s.server.middleware.ErrorAction
import org.http4s.server.middleware.GZip
import org.http4s.server.middleware.Logger as Http4sLogger
import org.typelevel.log4cats.Logger

object ServerMiddleware {
  type Middleware[F[_]] = Endo[HttpRoutes[F]]

  /**
   * Create a combined middleware to all server routes
   */
  def apply[F[_]: {Async, Compression, Logger}](): Middleware[F] =
    val logging = Http4sLogger.httpRoutes[F](
      logHeaders = true,
      logBody = false
    )

    val errorReporting: Middleware[F] = routes =>
      ErrorAction.httpRoutes.log(
        httpRoutes = routes,
        messageFailureLogAction = Logger[F].error(_)(_),
        serviceErrorLogAction = Logger[F].error(_)(_)
      )

    List[Middleware[F]](
      logging,
      errorReporting,
      GZip(_)
    ).reduce(_ andThen _) // N.B. the monoid for Endo uses `compose`
}

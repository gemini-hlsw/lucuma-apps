// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.web.server.http4s

import cats.effect.Async
import cats.syntax.all.*
import fs2.compression.Compression
import lucuma.graphql.routes.GraphQLService
import lucuma.graphql.routes.Routes
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.server.middleware.GZip
import org.http4s.server.websocket.WebSocketBuilder2
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.otel4s.trace.Tracer.Implicits.noop

class GraphQlRoutes[F[_]: {Async, Compression}](
  mappings: NavigateMappings[F]
) extends Http4sDsl[F] {

  private given Logger[F] =
    Slf4jLogger.getLoggerFromName[F]("navigate.web.server.http4s.GraphQlRoutes")

  def service(wsb: WebSocketBuilder2[F]): HttpRoutes[F] =
    GZip(
      Routes.forService(
        _ => GraphQLService[F](mappings).some.pure[F],
        wsb
      )
    )

}

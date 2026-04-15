// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.web.server.http4s

import _root_.skunk.Session
import cats.effect.*
import cats.syntax.all.*
import edu.gemini.schema.util.SchemaStitcher
import edu.gemini.schema.util.SourceResolver
import grackle.*
import grackle.skunk.SkunkMonitor
import lucuma.graphql.routes.GraphQLService
import lucuma.graphql.routes.Routes
import natchez.Trace
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.server.websocket.WebSocketBuilder2
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import resource.web.server.graphql.ResourceMapping

import java.nio.file.Path as JPath

class GraphQlRoutes[F[_]: {Async, Trace}](
) extends Http4sDsl[F] {

  private given Logger[F] = Slf4jLogger.getLogger[F]

  def service(
    wsb:     WebSocketBuilder2[F],
    pool:    Resource[F, Session[F]],
    monitor: SkunkMonitor[F],
    schema:  Schema
  ): HttpRoutes[F] =
    Routes.forService(
      _ =>
        GraphQLService[F](
          ResourceMapping(
            pool,
            monitor
          )(schema)
        ).some.pure[F],
      wsb
    )
}

object GraphQlRoutes:

  def loadSchema[F[_]: {Sync, Logger}]: F[Schema] =
    SchemaStitcher
      .apply[F](JPath.of("resource.graphql"), SourceResolver.fromResource(getClass.getClassLoader))
      .build
      .flatMap {
        case Result.Success(schema)           =>
          Logger[F].info("Loaded GraphQL schema") *>
            schema.pure[F]
        case Result.Warning(problems, schema) =>
          Logger[F]
            .warn(s"Loaded schema with problems: ${problems.map(_.message).toList.mkString(",")}")
            .as(schema)
        case Result.Failure(problems)         =>
          Sync[F].raiseError[Schema](
            new Throwable(
              s"Unable to load schema because: ${problems.map(_.message).toList.mkString(",")}"
            )
          )
        case Result.InternalError(error)      =>
          Sync[F].raiseError[Schema](
            new Throwable(s"Unable to load schema because: ${error.getMessage}")
          )
      }

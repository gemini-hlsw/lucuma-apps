// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.*
import cats.effect.*
import cats.effect.std.SecureRandom
import cats.syntax.all.*
import clue.StreamingClient
import clue.js.*
import clue.otel4s.Otel4sMiddleware
import clue.websocket.*
import io.circe.Json
import lucuma.schemas.*
import org.http4s.*
import org.typelevel.log4cats.Logger
import org.typelevel.otel4s.trace.Tracer
import queries.schemas.*

case class GraphQLClients[F[_]: {Async, Parallel}] protected (
  // Persistent clients kept for connect/disconnect lifecycle.
  odbPersistent:   WebSocketJsClient[F, ObservationDB],
  prefsPersistent: WebSocketJsClient[F, UserPreferencesDB],
  // Traced views exposed to query/subscription call sites.
  odb:             StreamingClient[F, ObservationDB],
  preferencesDB:   StreamingClient[F, UserPreferencesDB],
  sso:             FetchJsClient[F, SSO]
):
  def init(payload: F[Map[String, Json]]): F[Unit] =
    (prefsPersistent.connect(), odbPersistent.connect(payload)).parTupled.void

  def close(): F[Unit] =
    List(
      prefsPersistent.disconnect(CloseParams(code = 1000)),
      odbPersistent.disconnect(CloseParams(code = 1000))
    ).sequence.void

object GraphQLClients:
  def build[F[
    _
  ]: {Async, FetchJsBackend, WebSocketJsBackend, Parallel, Logger, SecureRandom, Tracer}](
    odbURI:               Uri,
    prefsURI:             Uri,
    ssoURI:               Uri,
    reconnectionStrategy: ReconnectionStrategy
  ): F[GraphQLClients[F]] =
    for {
      odbClient   <-
        WebSocketJsClient.of[F, ObservationDB](odbURI.toString, "ODB", reconnectionStrategy)
      prefsClient <-
        WebSocketJsClient.of[F, UserPreferencesDB](prefsURI.toString, "PREFS", reconnectionStrategy)
      ssoClient   <-
        FetchJsClient.of[F, SSO](s"${ssoURI.toString}/graphql", "SSO")
    } yield GraphQLClients(
      odbClient,
      prefsClient,
      Otel4sMiddleware(odbClient: StreamingClient[F, ObservationDB]),
      Otel4sMiddleware(prefsClient: StreamingClient[F, UserPreferencesDB]),
      Otel4sMiddleware(ssoClient)
    )

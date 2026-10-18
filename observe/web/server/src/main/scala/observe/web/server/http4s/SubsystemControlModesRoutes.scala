// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.web.server.http4s

import cats.effect.Async
import cats.effect.Ref
import cats.syntax.all.*
import fs2.concurrent.Topic
import lucuma.core.enums.Site
import lucuma.core.model.User
import lucuma.sso.client.SsoClient
import observe.model.ClientId
import observe.model.config.SubsystemControlModes
import observe.model.events.ClientEvent
import observe.server.ObserveEngine
import observe.web.server.config.SubsystemControlModesFile
import org.http4s.*
import org.http4s.circe.*
import org.http4s.dsl.*
import org.typelevel.log4cats.Logger

import java.nio.file.Path

/**
 * Endpoints, mounted at `/api/observe/subsystems`, to read and save the control mode of the
 * subsystems and to restart the server so that a saved configuration takes effect.
 */
class SubsystemControlModesRoutes[F[_]: {Async, Logger}](
  ssoClient:                  SsoClient[F, User],
  oe:                         ObserveEngine[F],
  site:                       Site,
  savedSubsystemControlModes: Ref[F, SubsystemControlModes],
  filePath:                   Path,
  events:                     Topic[F, (Option[ClientId], ClientEvent)],
  shutdown:                   F[Unit]
) extends Http4sDsl[F]:

  given EntityDecoder[F, SubsystemControlModes] = jsonOf[F, SubsystemControlModes]
  given EntityEncoder[F, SubsystemControlModes] =
    jsonEncoderOf[F, SubsystemControlModes]

  private def describeDiff(
    previous: SubsystemControlModes,
    current:  SubsystemControlModes
  ): String =
    val changes: List[String] =
      previous
        .diff(current, site)
        .map((field, from, to) => s"${field.label}: ${from.tag} -> ${to.tag}")
    if changes.isEmpty then "no changes" else changes.mkString(", ")

  val service: HttpRoutes[F] = HttpRoutes.of[F]:
    case req @ GET -> Root =>
      ssoClient.require(req): _ =>
        savedSubsystemControlModes.get.flatMap(Ok(_))

    case req @ POST -> Root =>
      ssoClient.require(req): user =>
        req.decode[SubsystemControlModes]: config =>
          for
            _        <- SubsystemControlModesFile.write[F](filePath, site, config, user.displayName)
            previous <- savedSubsystemControlModes.getAndSet(config)
            _        <- Logger[F].info(
                          s"${user.displayName} saved system control: ${describeDiff(previous, config)}"
                        )
            _        <- events.publish1((none, ClientEvent.SubsystemControlModesSaved(config)))
            resp     <- NoContent()
          yield resp

    case req @ POST -> Root / ClientIDVar(clientId) / "restart" =>
      ssoClient.require(req): user =>
        oe.requestRestart(user, clientId, shutdown) *> NoContent()

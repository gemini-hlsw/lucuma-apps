// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.web.server.http4s

import cats.effect.IO
import cats.effect.Ref
import cats.syntax.all.*
import fs2.concurrent.Topic
import io.circe.syntax.*
import lucuma.core.enums.Site
import lucuma.core.model.User
import lucuma.sso.client.SsoClient.AbstractSsoClient
import observe.model.ClientId
import observe.model.config.SubsystemControlModes
import observe.model.enums.SubsystemControlMode
import observe.model.events.ClientEvent
import org.http4s.*
import org.http4s.circe.*
import org.http4s.headers.Authorization
import org.http4s.implicits.*

import java.nio.file.Files
import java.nio.file.Path
import java.util.UUID
import scala.jdk.CollectionConverters.*

class SubsystemControlModesRoutesSuite extends munit.CatsEffectSuite with TestRoutes:
  private val clientId: ClientId = ClientId(UUID.randomUUID())

  private val Initial: SubsystemControlModes =
    SubsystemControlModes(
      SubsystemControlMode.Simulated,
      SubsystemControlMode.Simulated,
      SubsystemControlMode.Simulated,
      SubsystemControlMode.Simulated,
      SubsystemControlMode.Simulated,
      SubsystemControlMode.Simulated,
      SubsystemControlMode.Simulated,
      SubsystemControlMode.Simulated,
      SubsystemControlMode.Simulated,
      SubsystemControlMode.Simulated,
      SubsystemControlMode.Simulated,
      SubsystemControlMode.Simulated,
      SubsystemControlMode.Simulated,
      SubsystemControlMode.Simulated,
      SubsystemControlMode.Simulated
    )

  private val NoUser: AbstractSsoClient[IO, User] = new AbstractSsoClient[IO, User]:
    override def get(authorization: Authorization): IO[Either[String, User]] =
      IO.pure("not logged in".asLeft)
    override def find(req: Request[IO]): IO[Either[String, User]]            =
      IO.pure("not logged in".asLeft)

  private case class Env(
    routes: HttpRoutes[IO],
    saved:  Ref[IO, SubsystemControlModes],
    topic:  Topic[IO, (Option[ClientId], ClientEvent)],
    file:   Path,
    shut:   Ref[IO, Boolean]
  )

  private def env(sso: AbstractSsoClient[IO, User] = this.sso): IO[Env] =
    for
      engine <- TestObserveEngine.build[IO]
      saved  <- Ref.of[IO, SubsystemControlModes](Initial)
      topic  <- Topic[IO, (Option[ClientId], ClientEvent)]
      shut   <- Ref.of[IO, Boolean](false)
      dir    <- IO.delay(Files.createTempDirectory("systems-routes"))
      file    = dir.resolve("conf").resolve("local").resolve("subsystems.conf")
    yield Env(
      SubsystemControlModesRoutes[IO](sso,
                                      engine,
                                      Site.GS,
                                      saved,
                                      file,
                                      topic,
                                      shut.set(true)
      ).service,
      saved,
      topic,
      file,
      shut
    )

  test("all endpoints require a logged in user"):
    for
      e <- env(NoUser)
      g <- e.routes.orNotFound.run(Request[IO](Method.GET, uri"/"))
      p <- e.routes.orNotFound.run(
             Request[IO](Method.POST, uri"/").withEntity(Initial.asJson)
           )
      r <- e.routes.orNotFound.run(
             Request[IO](Method.POST, Uri.unsafeFromString(s"/${clientId.value}/restart"))
           )
      s <- e.saved.get
      d <- e.shut.get
    yield
      assertEquals(List(g, p, r).map(_.status.responseClass), List.fill(3)(Status.ClientError))
      assertEquals(s, Initial)
      assert(!d)
      assert(!Files.exists(e.file))

  test("GET returns the saved configuration"):
    for
      e <- env()
      g <- e.routes.orNotFound.run(Request[IO](Method.GET, uri"/"))
      c <- g.asJsonDecode[SubsystemControlModes]
    yield
      assertEquals(g.status, Status.Ok)
      assertEquals(c, Initial)

  test("POST writes the file, updates the ref and publishes SubsystemControlModesSaved"):
    val updated: SubsystemControlModes = Initial.copy(gmos = SubsystemControlMode.Full)
    for
      e      <- env()
      events <- e.topic.subscribe(10).take(1).compile.toList.start
      _      <- IO.sleep(scala.concurrent.duration.DurationInt(200).millis)
      p      <- e.routes.orNotFound.run(Request[IO](Method.POST, uri"/").withEntity(updated.asJson))
      s      <- e.saved.get
      ev     <- events.joinWithNever
    yield
      assertEquals(p.status, Status.NoContent)
      assertEquals(s, updated)
      assertEquals(ev, List((none, ClientEvent.SubsystemControlModesSaved(updated))))
      val content = Files.readAllLines(e.file).asScala.mkString("\n")
      assert(content.contains("gmos=full") || content.contains("gmos = full"), content)

  test("POST restart calls the engine"):
    for
      e <- env()
      r <- e.routes.orNotFound.run(
             Request[IO](Method.POST, Uri.unsafeFromString(s"/${clientId.value}/restart"))
           )
      d <- e.shut.get
    yield
      assertEquals(r.status, Status.NoContent)
      assert(d)

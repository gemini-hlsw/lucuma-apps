// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.web.server.http4s

import cats.effect.IO
import cats.syntax.all.*
import io.circe.refined.given
import io.circe.syntax.*
import lucuma.core.circe.coulomb.given
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ImageQuality
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Step
import observe.model.ClientId
import org.http4s.*
import org.http4s.circe.*
import org.http4s.implicits.*
import org.http4s.server.websocket.WebSocketBuilder2

import java.util.UUID

class ObserveCommandRoutesSuite extends munit.CatsEffectSuite with TestRoutes:
  val clientId = ClientId(UUID.randomUUID())
  val obsId    = Observation.Id.fromLong(1000).get
  val stepId   = Step.Id.fromUuid(UUID.randomUUID())

  test("reset conditions"):
    val r = for
      engine <- TestObserveEngine.build[IO]
      s      <- commandRoutes(engine)
      wsb    <- WebSocketBuilder2[IO]
      l      <- s(Request[IO](method = Method.POST, uri = uri"/resetconditions")).value
    yield l.map(_.status)
    assertIO(r, Some(Status.NoContent))

  test("update water vapor"):
    val r = for
      engine <- TestObserveEngine.build[IO]
      s      <- commandRoutes(engine)
      wsb    <- WebSocketBuilder2[IO]
      l      <- s(
                  Request[IO](method = Method.POST, uri = Uri.unsafeFromString(s"/${clientId.value}/wv"))
                    .withEntity((WaterVapor.Wet: WaterVapor).asJson)
                ).value
      b      <- l.traverse(_.as[String])
    yield (l.map(_.status), b)
    assertIO(r.map(_._1), Some(Status.NoContent)) *>
      assertIO(r.map(_._2), Some(s""))

  test("update image quality"):
    val r = for
      engine <- TestObserveEngine.build[IO]
      s      <- commandRoutes(engine)
      wsb    <- WebSocketBuilder2[IO]
      l      <- s(
                  Request[IO](method = Method.POST, uri = Uri.unsafeFromString(s"/${clientId.value}/iq"))
                    .withEntity(ImageQuality.Preset.PointThree.toImageQuality.asJson)
                ).value
      b      <- l.traverse(_.as[String])
    yield (l.map(_.status), b)
    assertIO(r.map(_._1), Some(Status.NoContent)) *>
      assertIO(r.map(_._2), Some(s""))

  test("update sky background"):
    val r = for
      engine <- TestObserveEngine.build[IO]
      s      <- commandRoutes(engine)
      wsb    <- WebSocketBuilder2[IO]
      l      <- s(
                  Request[IO](method = Method.POST, uri = Uri.unsafeFromString(s"/${clientId.value}/sb"))
                    .withEntity((SkyBackground.Darkest: SkyBackground).asJson)
                ).value
      b      <- l.traverse(_.as[String])
    yield (l.map(_.status), b)
    assertIO(r.map(_._1), Some(Status.NoContent)) *>
      assertIO(r.map(_._2), Some(s""))

  test("update cloud extinction"):
    val r = for
      engine <- TestObserveEngine.build[IO]
      s      <- commandRoutes(engine)
      wsb    <- WebSocketBuilder2[IO]
      l      <- s(
                  Request[IO](method = Method.POST, uri = Uri.unsafeFromString(s"/${clientId.value}/ce"))
                    .withEntity(CloudExtinction.Preset.PointFive.toCloudExtinction.asJson)
                ).value
      b      <- l.traverse(_.as[String])
    yield (l.map(_.status), b)
    assertIO(r.map(_._1), Some(Status.NoContent)) *>
      assertIO(r.map(_._2), Some(s""))

  test("load sequence"):
    val r = for
      engine <- TestObserveEngine.build[IO]
      s      <- commandRoutes(engine)
      wsb    <- WebSocketBuilder2[IO]
      l      <- s(
                  Request[IO](
                    method = Method.POST,
                    uri = Uri.unsafeFromString(
                      s"/load/GmosSouth/${obsId.show}/${clientId.value}/observer"
                    )
                  )
                ).value
    yield l.map(_.status)
    assertIO(r, Some(Status.NoContent))

  test("unload sequence"):
    val r = for
      engine <- TestObserveEngine.build[IO]
      s      <- commandRoutes(engine)
      wsb    <- WebSocketBuilder2[IO]
      l      <- s(
                  Request[IO](
                    method = Method.POST,
                    uri = Uri.unsafeFromString(s"/unload/${obsId.show}/${clientId.value}")
                  )
                ).value
    yield l.map(_.status)
    assertIO(r, Some(Status.NoContent))

  test("execute sequence step/subsystem"):
    val r = for
      engine <- TestObserveEngine.build[IO]
      s      <- commandRoutes(engine)
      wsb    <- WebSocketBuilder2[IO]
      l      <- s(
                  Request[IO](
                    method = Method.POST,
                    uri = Uri.unsafeFromString(
                      s"/${obsId.show}/${stepId.show}/${clientId.value}/execute/Tcs/observer"
                    )
                  )
                ).value
    yield l.map(_.status)
    assertIO(r, Some(Status.NoContent))

  test("disable tcs"):
    val r = for
      engine <- TestObserveEngine.build[IO]
      s      <- commandRoutes(engine)
      wsb    <- WebSocketBuilder2[IO]
      l      <- s(
                  Request[IO](
                    method = Method.POST,
                    uri = Uri.unsafeFromString(
                      s"/${obsId.show}/${clientId.value}/tcsEnabled/false"
                    )
                  )
                ).value
    yield l.map(_.status)
    assertIO(r, Some(Status.NoContent))

  test("disable gcal"):
    val r = for
      engine <- TestObserveEngine.build[IO]
      s      <- commandRoutes(engine)
      wsb    <- WebSocketBuilder2[IO]
      l      <- s(
                  Request[IO](
                    method = Method.POST,
                    uri = Uri.unsafeFromString(
                      s"/${obsId.show}/${clientId.value}/gcalEnabled/false"
                    )
                  )
                ).value
    yield l.map(_.status)
    assertIO(r, Some(Status.NoContent))

  test("disable instrument"):
    val r = for
      engine <- TestObserveEngine.build[IO]
      s      <- commandRoutes(engine)
      wsb    <- WebSocketBuilder2[IO]
      l      <- s(
                  Request[IO](
                    method = Method.POST,
                    uri = Uri.unsafeFromString(
                      s"/${obsId.show}/${clientId.value}/instrumentEnabled/false"
                    )
                  )
                ).value
    yield l.map(_.status)
    assertIO(r, Some(Status.NoContent))

  test("disable dhs"):
    val r = for
      engine <- TestObserveEngine.build[IO]
      s      <- commandRoutes(engine)
      wsb    <- WebSocketBuilder2[IO]
      l      <- s(
                  Request[IO](
                    method = Method.POST,
                    uri = Uri.unsafeFromString(
                      s"/${obsId.show}/${clientId.value}/dhsEnabled/false"
                    )
                  )
                ).value
    yield l.map(_.status)
    assertIO(r, Some(Status.NoContent))

  test("set breakpoint"):
    val r = for
      engine <- TestObserveEngine.build[IO]
      s      <- commandRoutes(engine)
      wsb    <- WebSocketBuilder2[IO]
      l      <-
        s(
          Request[IO](
            method = Method.POST,
            uri = Uri.unsafeFromString(
              s"/${obsId.show}/${stepId.show}/${clientId.value}/breakpoint/observer/true"
            )
          )
        ).value
    yield l.map(_.status)
    assertIO(r, Some(Status.NoContent))

  test("set breakpoints"):
    val r = for
      engine <- TestObserveEngine.build[IO]
      s      <- commandRoutes(engine)
      wsb    <- WebSocketBuilder2[IO]
      l      <-
        s(
          Request[IO](
            method = Method.POST,
            uri = Uri
              .unsafeFromString(
                s"/${obsId.show}/${clientId.value}/breakpoints/observer/true"
              )
          )
            .withEntity(List(stepId).asJson)
        ).value
    yield l.map(_.status)
    assertIO(r, Some(Status.NoContent))

  test("start sequence"):
    val r = for
      engine <- TestObserveEngine.build[IO]
      s      <- commandRoutes(engine)
      wsb    <- WebSocketBuilder2[IO]
      l      <-
        s(
          Request[IO](
            method = Method.POST,
            uri = Uri.unsafeFromString(
              s"/${obsId.show}/${clientId.value}/startSequence/observer?overrideTargetCheck=true"
            )
          )
        ).value
    yield l.map(_.status)
    assertIO(r, Some(Status.NoContent))

  test("set operator"):
    val r = for
      engine <- TestObserveEngine.build[IO]
      s      <- commandRoutes(engine)
      wsb    <- WebSocketBuilder2[IO]
      l      <-
        s(
          Request[IO](
            method = Method.POST,
            uri = Uri.unsafeFromString(
              s"/${clientId.value}/operator/Anybody"
            )
          )
        ).value
    yield l.map(_.status)
    assertIO(r, Some(Status.NoContent))

  test("set observer"):
    val r = for
      engine <- TestObserveEngine.build[IO]
      s      <- commandRoutes(engine)
      wsb    <- WebSocketBuilder2[IO]
      l      <-
        s(
          Request[IO](
            method = Method.POST,
            uri = Uri.unsafeFromString(
              s"/${obsId.show}/${clientId.value}/observer/Anybody"
            )
          )
        ).value
    yield l.map(_.status)
    assertIO(r, Some(Status.NoContent))

  test("request sequence hold"):
    val uri = Uri.unsafeFromString(s"/${obsId.show}/${clientId.value}/holdSequence/observer")
    val r   = for
      engine <- TestObserveEngine.build[IO]
      s      <- commandRoutes(engine)
      wsb    <- WebSocketBuilder2[IO]
      l      <- s(Request[IO](method = Method.POST, uri = uri)).value
    yield l.map(_.status)
    assertIO(r, Some(Status.NoContent))

  test("cancel sequence hold request"):
    val uri = Uri.unsafeFromString(s"/${obsId.show}/${clientId.value}/cancelHoldSequence/observer")
    val r   = for
      engine <- TestObserveEngine.build[IO]
      s      <- commandRoutes(engine)
      wsb    <- WebSocketBuilder2[IO]
      l      <- s(Request[IO](method = Method.POST, uri = uri)).value
    yield l.map(_.status)
    assertIO(r, Some(Status.NoContent))

  test("rewind step"):
    val uri = Uri.unsafeFromString(s"/${obsId.show}/${clientId.value}/rewindStep/observer")
    val r   = for
      engine <- TestObserveEngine.build[IO]
      s      <- commandRoutes(engine)
      wsb    <- WebSocketBuilder2[IO]
      l      <- s(Request[IO](method = Method.POST, uri = uri)).value
    yield l.map(_.status)
    assertIO(r, Some(Status.NoContent))

  test("cancel rewind step request"):
    val uri = Uri.unsafeFromString(s"/${obsId.show}/${clientId.value}/cancelRewindStep/observer")
    val r   = for
      engine <- TestObserveEngine.build[IO]
      s      <- commandRoutes(engine)
      wsb    <- WebSocketBuilder2[IO]
      l      <- s(Request[IO](method = Method.POST, uri = uri)).value
    yield l.map(_.status)
    assertIO(r, Some(Status.NoContent))

  test("stop exposure"):
    val uri = Uri.unsafeFromString(s"/${obsId.show}/${clientId.value}/stopExposure/observer")
    val r   = for
      engine <- TestObserveEngine.build[IO]
      s      <- commandRoutes(engine)
      wsb    <- WebSocketBuilder2[IO]
      l      <- s(Request[IO](method = Method.POST, uri = uri)).value
    yield l.map(_.status)
    assertIO(r, Some(Status.NoContent))

  test("stop exposure gracefully"):
    val uri = Uri.unsafeFromString(
      s"/${obsId.show}/${clientId.value}/stopExposureGracefully/observer"
    )
    val r   = for
      engine <- TestObserveEngine.build[IO]
      s      <- commandRoutes(engine)
      wsb    <- WebSocketBuilder2[IO]
      l      <- s(Request[IO](method = Method.POST, uri = uri)).value
    yield l.map(_.status)
    assertIO(r, Some(Status.NoContent))

  test("abort exposure"):
    val uri =
      Uri.unsafeFromString(s"/${obsId.show}/${clientId.value}/abortExposure/observer")
    val r   = for
      engine <- TestObserveEngine.build[IO]
      s      <- commandRoutes(engine)
      wsb    <- WebSocketBuilder2[IO]
      l      <- s(Request[IO](method = Method.POST, uri = uri)).value
    yield l.map(_.status)
    assertIO(r, Some(Status.NoContent))

  test("pause exposure"):
    val uri =
      Uri.unsafeFromString(s"/${obsId.show}/${clientId.value}/pauseExposure/observer")
    val r   = for
      engine <- TestObserveEngine.build[IO]
      s      <- commandRoutes(engine)
      wsb    <- WebSocketBuilder2[IO]
      l      <- s(Request[IO](method = Method.POST, uri = uri)).value
    yield l.map(_.status)
    assertIO(r, Some(Status.NoContent))

  test("pause exposure gracefully"):
    val uri = Uri.unsafeFromString(
      s"/${obsId.show}/${clientId.value}/pauseExposureGracefully/observer"
    )
    val r   = for
      engine <- TestObserveEngine.build[IO]
      s      <- commandRoutes(engine)
      wsb    <- WebSocketBuilder2[IO]
      l      <- s(Request[IO](method = Method.POST, uri = uri)).value
    yield l.map(_.status)
    assertIO(r, Some(Status.NoContent))

  test("resume exposure"):
    val uri =
      Uri.unsafeFromString(s"/${obsId.show}/${clientId.value}/resumeExposure/observer")
    val r   = for
      engine <- TestObserveEngine.build[IO]
      s      <- commandRoutes(engine)
      wsb    <- WebSocketBuilder2[IO]
      l      <- s(Request[IO](method = Method.POST, uri = uri)).value
    yield l.map(_.status)
    assertIO(r, Some(Status.NoContent))

  test("daytime queue commands"):
    val pauseId: String             = UUID.randomUUID().toString
    val client: String              = clientId.value.toString
    val obs: String                 = obsId.show
    val paths: List[String]         =
      List(
        s"/queue/add/$obs/$client",
        s"/queue/remove/obs/$obs/$client",
        s"/queue/remove/pause/$pauseId/$client",
        s"/queue/move/obs/$obs/2/$client",
        s"/queue/move/pause/$pauseId/0/$client",
        s"/queue/pause/1/$client",
        s"/queue/orderingMode/strict/$client",
        s"/queue/run/$client/observer",
        s"/queue/holdAfterSteps/$client",
        s"/queue/holdAfterObservations/$client",
        s"/queue/abortAll/$client/observer",
        s"/queue/obs/$obs/$client/run/observer",
        s"/queue/obs/$obs/$client/abort/observer",
        s"/queue/obs/$obs/$client/retry",
        s"/queue/clearCompleted/$client",
        s"/queue/lampsOff/cancel/$client"
      )
    val r: IO[List[Option[Status]]] =
      for
        engine <- TestObserveEngine.build[IO]
        s      <- commandRoutes(engine)
        l      <- paths.traverse: path =>
                    s(Request[IO](method = Method.POST, uri = Uri.unsafeFromString(path))).value
      yield l.map(_.map(_.status))
    assertIO(r, List.fill(paths.length)(Some(Status.NoContent)))

  test("route template keeps the daytime queue command words"):
    def template(path: String): String =
      ObserveCommandRoutes.routeTemplate(
        Uri.unsafeFromString(path).path.segments.map(_.decoded()).toList
      )

    val pauseId: UUID = UUID.randomUUID()
    assertEquals(
      template(s"/api/observe/queue/move/pause/$pauseId/3/${clientId.value}"),
      "/api/observe/queue/move/pause/{pauseId}/{param}/{clientId}"
    )
    assertEquals(
      template(s"/api/observe/queue/obs/${obsId.show}/${clientId.value}/abort/telops"),
      "/api/observe/queue/obs/{obsId}/{clientId}/abort/{param}"
    )
    assertEquals(
      template(s"/api/observe/queue/orderingMode/strict/${clientId.value}"),
      "/api/observe/queue/orderingMode/{param}/{clientId}"
    )
    assertEquals(
      template(s"/api/observe/queue/pause/1/${clientId.value}"),
      "/api/observe/queue/pause/{param}/{clientId}"
    )

  test("route template anonymizes ids and free-form values, keeping the command"):
    def template(path: String): String =
      ObserveCommandRoutes.routeTemplate(
        Uri.unsafeFromString(path).path.segments.map(_.decoded()).toList
      )

    assertEquals(
      template(s"/api/observe/${obsId.show}/${clientId.value}/startSequence/telops"),
      "/api/observe/{obsId}/{clientId}/startSequence/{param}"
    )
    assertEquals(
      template(
        s"/api/observe/${obsId.show}/${stepId.show}/${clientId.value}/execute/Igrins2/telops"
      ),
      "/api/observe/{obsId}/{stepId}/{clientId}/execute/{param}/{param}"
    )
    assertEquals(
      template(s"/api/observe/load/Igrins2/${obsId.show}/${clientId.value}/telops"),
      "/api/observe/load/{param}/{obsId}/{clientId}/{param}"
    )
    assertEquals(
      template(s"/api/observe/${clientId.value}/iq"),
      "/api/observe/{clientId}/iq"
    )
    assertEquals(template("/api/observe/resetconditions"), "/api/observe/resetconditions")

  test("route template is the same for different values of the same route"):
    def template(path: String): String =
      ObserveCommandRoutes.routeTemplate(
        Uri.unsafeFromString(path).path.segments.map(_.decoded()).toList
      )

    val other = Observation.Id.fromLong(2000).get
    assertEquals(
      template(s"/api/observe/${obsId.show}/${clientId.value}/tcsEnabled/true"),
      template(s"/api/observe/${other.show}/${ClientId(UUID.randomUUID()).value}/tcsEnabled/false")
    )

  test("path attributes carry the ids the route template anonymizes"):
    def attributes(path: String): Map[String, String] =
      ObserveCommandRoutes
        .pathAttributes(Uri.unsafeFromString(path).path.segments.map(_.decoded()).toList)
        .map(a => a.key.name -> a.value.toString)
        .toMap

    assertEquals(
      attributes(
        s"/api/observe/${obsId.show}/${stepId.show}/${clientId.value}/execute/Igrins2/Telops"
      ),
      Map(
        "observe.obs.id"    -> obsId.show,
        "observe.step.id"   -> stepId.show,
        "observe.client.id" -> clientId.value.toString
      )
    )

    // Free-form values (observer here) are not turned into attributes.
    assertEquals(
      attributes(s"/api/observe/${obsId.show}/${clientId.value}/startSequence/Telops").keySet,
      Set("observe.obs.id", "observe.client.id")
    )

    assertEquals(attributes("/api/observe/resetconditions"), Map.empty[String, String])

    val pauseId: UUID = UUID.randomUUID()
    assertEquals(
      attributes(s"/api/observe/queue/remove/pause/$pauseId/${clientId.value}"),
      Map(
        "observe.queue.pause.id" -> pauseId.toString,
        "observe.client.id"      -> clientId.value.toString
      )
    )

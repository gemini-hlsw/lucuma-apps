// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import boopickle.DefaultBasic.*
import cats.effect.IO
import cats.effect.unsafe.implicits.*
import cats.syntax.all.*
import explore.events.AgsMessage
import explore.model.AgsResult
import explore.model.AppConfig
import explore.model.boopickle.CatalogPicklers.given
import lucuma.ags.Ags
import lucuma.ags.AgsAnalysis.*
import lucuma.core.geom.ShapeInterpreter
import lucuma.core.geom.ShapePolygon
import lucuma.core.geom.jts.JtsShapeInterpreter
import lucuma.core.geom.wasm.WasmGeometry
import lucuma.core.math.Angle
import org.scalajs.dom
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.trace.Tracer
import org.typelevel.otel4s.trace.TracerProvider
import workers.*

import java.time.Duration
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("AgsServer", moduleID = "exploreworkers")
object AgsServer extends WorkerServer[AgsMessage.Request] {
  @JSExport
  def runWorker(): Unit = run.unsafeRunAndForget()

  private val AgsCacheVersion: Int = 48

  private val CacheRetention: Duration = Duration.ofDays(60)

  def agsCalculation(
    r: AgsMessage.AgsRequest
  )(using Logger[IO], Tracer[IO], ShapeInterpreter): IO[AgsResult] =
    IO.blocking:
      val correctedCandidates = r.candidates.map(_.at(r.vizTime))
      Ags
        .agsAnalysis(
          r.constraints,
          r.wavelength.value,
          r.baseCoordinates,
          r.scienceCoordinates,
          r.blindOffset,
          r.posAngles,
          r.acqOffsets,
          r.sciOffsets,
          r.params,
          correctedCandidates
        )
    .flatTap: r =>
        Tracer[IO].currentSpanOrNoop.flatMap(_.addAttributes(r._2.toSpanAttributes*)) *>
          Logger[IO].debug(pprint.apply(r._2.show).render)
      .map: res =>
        AgsResult(res._1.sortUsablePositions, patrolFields(r))

  // The intersection is the same for every position at a given angle; evaluate it here, in the
  // worker, so the UI never runs the geometry on the main thread.
  private def patrolFields(r: AgsMessage.AgsRequest)(using
    si: ShapeInterpreter
  ): Map[Angle, List[ShapePolygon]] =
    val positions =
      Ags.generatePositions(
        r.baseCoordinates.some,
        r.blindOffset,
        r.posAngles,
        r.acqOffsets,
        r.sciOffsets
      )
    si.withArena:
      r.params
        .posCalculations(positions.value.toNonEmptyList)
        .toNel
        .toList
        .distinctBy(_._1.posAngle)
        .map((pos, calc) => pos.posAngle -> calc.intersectionPatrolFieldShape.polygons)
        .toMap

  protected def handler(
    config: Option[AppConfig]
  ): (LoggerFactory[IO], Tracer[IO], TracerProvider[IO]) ?=> IO[Invocation => IO[Unit]] =
    for
      self                   <- IO(dom.DedicatedWorkerGlobalScope.self)
      cache                  <- Cache.withIDB[IO](self.indexedDB.toOption, "ags")
      _                      <- cache.evict(CacheRetention).start
      given Logger[IO]       <- LoggerFactory[IO].fromName("ags-worker")
      engine                 <- WasmGeometry.load.attempt
      given ShapeInterpreter <-
        engine.fold(
          e =>
            Logger[IO]
              .warn(e)("wasm geometry kernel unavailable, using lucuma-jts")
              .as(JtsShapeInterpreter),
          si => Logger[IO].info("AGS geometry: lucuma-wasm kernel").as(si)
        )
    yield invocation =>
      invocation.data match {
        case AgsMessage.CleanCache               =>
          cache.clear *> invocation.respond(())
        case req @ AgsMessage.AgsRequest(id = _) =>
          val cacheName = CacheName("ags")
          val cacheVer  = CacheVersion(AgsCacheVersion)
          cache
            .get[AgsMessage.AgsRequest, AgsResult](cacheName, cacheVer, req)
            .flatMap {
              case Some(result) => invocation.respond(result) // hit: nothing calculated, no span
              case None         =>                            // miss: trace the calculation
                Tracer[IO]
                  .span("ags", Attribute("ags.mode", req.params.mode))
                  .surround:
                    val compute = (r: AgsMessage.AgsRequest) => agsCalculation(r)
                    cache
                      .eval(Cacheable(cacheName, cacheVer, compute))
                      .apply(req)
                      .flatMap(invocation.respond)
            }
      }
}

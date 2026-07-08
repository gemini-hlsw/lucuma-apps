// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import boopickle.DefaultBasic.*
import cats.effect.IO
import cats.effect.unsafe.implicits.*
import explore.events.AgsMessage
import explore.model.boopickle.CatalogPicklers.given
import lucuma.ags.Ags
import lucuma.ags.AgsAnalysis
import lucuma.ags.AgsAnalysis.*
import org.scalajs.dom
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.LoggerFactory
import workers.*

import java.time.Duration
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("AgsServer", moduleID = "exploreworkers")
object AgsServer extends WorkerServer[IO, AgsMessage.Request] {
  @JSExport
  def runWorker(): Unit = run.unsafeRunAndForget()

  private val AgsCacheVersion: Int = 39

  private val CacheRetention: Duration = Duration.ofDays(60)

  def agsCalculation(r: AgsMessage.AgsRequest)(using Logger[IO]): IO[List[AgsAnalysis.Usable]] =
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
        // We should send these as a trace but workers are out of the trace context so far.
        Logger[IO].debug(pprint.apply(r._2).render)
      .map:
        _._1.sortUsablePositions

  protected val handler: LoggerFactory[IO] ?=> IO[Invocation => IO[Unit]] =
    for
      self             <- IO(dom.DedicatedWorkerGlobalScope.self)
      cache            <- Cache.withIDB[IO](self.indexedDB.toOption, "ags")
      _                <- cache.evict(CacheRetention).start
      given Logger[IO] <- LoggerFactory[IO].fromName("ags-worker")
    yield invocation =>
      invocation.data match {
        case AgsMessage.CleanCache               =>
          cache.clear *> invocation.respond(())
        case req @ AgsMessage.AgsRequest(id = _) =>
          val cacheableRequest =
            Cacheable(CacheName("ags"), CacheVersion(AgsCacheVersion), agsCalculation)
          cache
            .eval(cacheableRequest)
            .apply(req)
            .flatMap(invocation.respond)
      }
}

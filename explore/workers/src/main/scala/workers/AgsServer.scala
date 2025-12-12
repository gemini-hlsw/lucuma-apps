// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import boopickle.DefaultBasic.*
import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.unsafe.implicits.*
import explore.events.AgsMessage
import explore.model.CandidateAnalysis
import explore.model.boopickle.CatalogPicklers.given
import lucuma.ags.Ags
import lucuma.ags.AgsAnalysis
import lucuma.ags.AgsParams
import lucuma.ags.AcquisitionOffsets
import lucuma.ags.GuideStarCandidate
import lucuma.ags.ScienceOffsets
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Target
import org.scalajs.dom
import org.typelevel.log4cats.Logger
import workers.*

import java.time.Duration
import java.time.Instant
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("AgsServer", moduleID = "exploreworkers")
object AgsServer extends WorkerServer[IO, AgsMessage.Request] {
  @JSExport
  def runWorker(): Unit = run.unsafeRunAndForget()

  private val AgsCacheVersion: Int = 32

  private val CacheRetention: Duration = Duration.ofDays(60)

  // Cache key for unconstrained run (excludes posAngles since unconstrained always uses same range)
  private case class UnconstrainedCacheKey(
    id:                  Target.Id,
    vizTime:             Instant,
    constraints:         ConstraintSet,
    wavelength:          Wavelength,
    baseCoordinates:     Coordinates,
    scienceCoordinates:  List[Coordinates],
    blindOffset:         Option[Coordinates],
    unconstrainedAngles: NonEmptyList[Angle],
    params:              AgsParams,
    candidates:          List[GuideStarCandidate]
  )

  // Cache key for constrained run (excludes unconstrainedAngles and isUnbounded)
  private case class ConstrainedCacheKey(
    id:                 Target.Id,
    vizTime:            Instant,
    constraints:        ConstraintSet,
    wavelength:         Wavelength,
    baseCoordinates:    Coordinates,
    scienceCoordinates: List[Coordinates],
    blindOffset:        Option[Coordinates],
    posAngles:          NonEmptyList[Angle],
    acqOffsets:         Option[AcquisitionOffsets],
    sciOffsets:         Option[ScienceOffsets],
    params:             AgsParams,
    candidates:         List[GuideStarCandidate]
  )

  private given Pickler[UnconstrainedCacheKey] = generatePickler
  private given Pickler[ConstrainedCacheKey]   = generatePickler

  // Result type for unconstrained cache: map of target ID -> usable angle
  private type UnconstrainedResult = Map[Long, Angle]

  private def runUnconstrainedAgs(key: UnconstrainedCacheKey): IO[UnconstrainedResult] =
    IO.blocking {
      val correctedCandidates = key.candidates.map(_.at(key.vizTime))
      val results = Ags.agsAnalysis(
        key.constraints,
        key.wavelength,
        key.baseCoordinates,
        key.scienceCoordinates,
        key.blindOffset,
        key.unconstrainedAngles,
        None,
        None,
        key.params,
        correctedCandidates
      )
      results.collect {
        case u: AgsAnalysis.Usable => u.target.id -> u.posAngle
      }.toMap
    }

  private def runConstrainedAgs(key: ConstrainedCacheKey): IO[List[AgsAnalysis]] =
    IO.blocking {
      val correctedCandidates = key.candidates.map(_.at(key.vizTime))
      Ags.agsAnalysis(
        key.constraints,
        key.wavelength,
        key.baseCoordinates,
        key.scienceCoordinates,
        key.blindOffset,
        key.posAngles,
        key.acqOffsets,
        key.sciOffsets,
        key.params,
        correctedCandidates
      )
    }

  private def mergeResults(
    constrainedResults:  List[AgsAnalysis],
    usableAngleByTarget: UnconstrainedResult
  ): List[CandidateAnalysis] = {
    val usable    = constrainedResults.sortUsablePositions
    val nonUsable = constrainedResults.filterNot(_.isUsable)

    val usableCandidates = usable.map(a =>
      CandidateAnalysis(a, usableAt = None)
    )
    val nonUsableCandidates = nonUsable.map { a =>
      CandidateAnalysis(a, usableAt = usableAngleByTarget.get(a.target.id))
    }

    usableCandidates ++ nonUsableCandidates
  }

  protected val handler: Logger[IO] ?=> IO[Invocation => IO[Unit]] =
    for
      self  <- IO(dom.DedicatedWorkerGlobalScope.self)
      cache <- Cache.withIDB[IO](self.indexedDB.toOption, "ags")
      _     <- cache.evict(CacheRetention).start
    yield invocation =>
      invocation.data match {
        case AgsMessage.CleanCache               =>
          cache.clear *> invocation.respond(())
        case req @ AgsMessage.AgsRequest(id = _) =>
          val unconstrainedKey = UnconstrainedCacheKey(
            req.id,
            req.vizTime,
            req.constraints,
            req.wavelength,
            req.baseCoordinates,
            req.scienceCoordinates,
            req.blindOffset,
            req.unconstrainedAngles,
            req.params,
            req.candidates
          )

          val constrainedKey = ConstrainedCacheKey(
            req.id,
            req.vizTime,
            req.constraints,
            req.wavelength,
            req.baseCoordinates,
            req.scienceCoordinates,
            req.blindOffset,
            req.posAngles,
            req.acqOffsets,
            req.sciOffsets,
            req.params,
            req.candidates
          )

          val unconstrainedCacheable = Cacheable(
            CacheName("ags-unconstrained"),
            CacheVersion(AgsCacheVersion),
            runUnconstrainedAgs
          )

          val constrainedCacheable = Cacheable(
            CacheName("ags-constrained"),
            CacheVersion(AgsCacheVersion),
            runConstrainedAgs
          )

          val result = for {
            usableAngles       <- if (req.isUnbounded) IO.pure(Map.empty[Long, Angle])
                                  else cache.eval(unconstrainedCacheable).apply(unconstrainedKey)
            constrainedResults <- cache.eval(constrainedCacheable).apply(constrainedKey)
          } yield mergeResults(constrainedResults, usableAngles)

          result.flatMap(invocation.respond)
      }
}

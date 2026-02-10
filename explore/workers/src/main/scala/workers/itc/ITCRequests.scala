// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import boopickle.DefaultBasic.*
import cats.*
import cats.data.*
import cats.effect.*
import cats.effect.std.Semaphore
import cats.syntax.all.*
import explore.model.Constants
import explore.model.boopickle.ItcPicklers.given
import explore.model.itc.*
import explore.modes.ItcInstrumentConfig
import lucuma.core.enums.ScienceMode
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.Timestamp
import lucuma.itc.Error
import lucuma.itc.client.ClientCalculationResult
import lucuma.itc.client.ImagingInput
import lucuma.itc.client.ImagingParameters
import lucuma.itc.client.ItcClient
import lucuma.itc.client.ItcConstraintsInput
import lucuma.itc.client.SpectroscopyInput
import lucuma.itc.client.SpectroscopyParameters
import org.typelevel.log4cats.Logger
import queries.schemas.itc.syntax.*
import workers.*

object ITCRequests:
  val cacheVersion = CacheVersion(23)

  val itcErrorToQueryProblems: Error => ItcQueryProblem =
    case Error.SourceTooBright(halfWell) => ItcQueryProblem.SourceTooBright(halfWell)
    case Error.General(message)          => ItcQueryProblem.GenericError(message)

  // Copied from https://gist.github.com/gvolpe/44e2263f9068efe298a1f30390de6d22
  def parTraverseN[F[_]: {Concurrent, Parallel}, G[_]: Traverse, A, B](
    n:  Long,
    ga: G[A]
  )(f: A => F[B]) =
    Semaphore[F](n).flatMap: s =>
      ga.parTraverse(a => s.permit.use(_ => f(a)))

  // Wrapper method to match the call in ItcServer.scala
  def queryItc[F[_]: {Concurrent, Parallel, Logger}](
    constraints:         ConstraintSet,
    asterism:            NonEmptyList[ItcTarget],
    customSedTimestamps: List[Timestamp],
    modes:               List[(ItcInstrumentConfig, ExposureTimeMode)],
    cache:               Cache[F],
    callback:            ((ItcRequestParams, EitherNec[ItcTargetProblem, ItcResult])) => F[Unit]
  )(using Monoid[F[Unit]], ItcClient[F]): F[Unit] = {
    def itcResults(r: ClientCalculationResult): EitherNec[ItcTargetProblem, ItcResult] =
      // Convert to usable types
      r.targetTimes.partitionErrors.fold(
        errors =>
          errors
            .map: (error, idx) =>
              ItcTargetProblem(
                asterism.get(idx).map(_.name),
                itcErrorToQueryProblems(error)
              )
            .asLeft,
        times =>
          ItcResult
            .Result(times.value, r.targetTimes.brightestIndex)
            .rightNec
      )

    def doRequest(
      params: ItcRequestParams
    ): F[Option[(ItcRequestParams, EitherNec[ItcTargetProblem, ItcResult])]] =
      Logger[F].debug(
        s"ITC: Request for mode: ${params.mode}, exposureTimeMode: ${params.exposureTimeMode} and target count: ${params.asterism.length}"
      ) *>
        params.mode.toItcClientMode
          .traverse: mode =>
            val result =
              if (params.mode.mode == ScienceMode.Imaging)
                ItcClient[F]
                  .imaging(
                    ImagingInput(
                      ImagingParameters(
                        exposureTimeMode = params.exposureTimeMode,
                        constraints = ItcConstraintsInput.fromConstraintSet(params.constraints),
                        mode = mode
                      ),
                      params.asterism.map(_.gaiaFree.input)
                    ),
                    false
                  )
              else
                ItcClient[F]
                  .spectroscopy(
                    SpectroscopyInput(
                      SpectroscopyParameters(
                        exposureTimeMode = params.exposureTimeMode,
                        constraints = ItcConstraintsInput.fromConstraintSet(params.constraints),
                        mode = mode
                      ),
                      params.asterism.map(_.gaiaFree.input)
                    ),
                    false
                  )
            result.map(r => params -> itcResults(r))

    val cacheableRequest =
      Cacheable(
        CacheName("itcQuery"),
        cacheVersion,
        doRequest,
        (_, g) =>
          g.exists:
            _._2 match
              case Right(_)                                     => true
              case Left(Chain(ItcQueryProblem.GenericError(_))) => false
              case Left(_)                                      => true
      )

    val itcRowsParams: List[ItcRequestParams] =
      modes
        // Only handle known modes
        .collect:
          case (m @ ItcInstrumentConfig.GmosNorthSpectroscopy(_, _, _, _), exposureTimeMode) =>
            ItcRequestParams(exposureTimeMode, constraints, asterism, customSedTimestamps, m)
          case (m @ ItcInstrumentConfig.GmosSouthSpectroscopy(_, _, _, _), exposureTimeMode) =>
            ItcRequestParams(exposureTimeMode, constraints, asterism, customSedTimestamps, m)
          case (m @ ItcInstrumentConfig.Flamingos2Spectroscopy(_, _, _), exposureTimeMode)   =>
            ItcRequestParams(exposureTimeMode, constraints, asterism, customSedTimestamps, m)
          case (m @ ItcInstrumentConfig.GmosNorthImaging(_, _), exposureTimeMode)            =>
            ItcRequestParams(exposureTimeMode, constraints, asterism, customSedTimestamps, m)
          case (m @ ItcInstrumentConfig.GmosSouthImaging(_, _), exposureTimeMode)            =>
            ItcRequestParams(exposureTimeMode, constraints, asterism, customSedTimestamps, m)

    // NOTE: callback is called once per mode. So, if you have more than one mode
    // you can't use `requestSingle`. You need to use `request` and handle the stream.
    parTraverseN(
      Constants.MaxConcurrentItcRequests.toLong,
      itcRowsParams.reverse
    ) { params =>
      // ITC supports sending many modes at once, but sending them one by one
      // maximizes cache hits
      cache.eval(cacheableRequest).apply(params).flatMap(_.map(callback).orEmpty)
    }.void
  }

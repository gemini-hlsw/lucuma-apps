// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import boopickle.DefaultBasic.*
import cats.Eq
import cats.data.EitherNec
import cats.data.EitherT
import cats.data.NonEmptyList
import cats.derived.*
import cats.effect.IO
import cats.syntax.all.*
import explore.events.ItcMessage
import explore.model.Constants
import explore.model.Observation
import explore.model.TargetList
import explore.model.WorkerClients.ItcClient
import explore.model.boopickle.ItcPicklers.given
import explore.model.itc.*
import explore.model.reusability.given
import explore.modes.ItcInstrumentConfig
import japgolly.scalajs.react.ReactCats.*
import japgolly.scalajs.react.Reusability
import lucuma.core.data.Zipper
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.Timestamp
import queries.schemas.itc.syntax.*
import workers.WorkerClient

case class ItcImagingQuerier(
  observation:         Observation,
  selectedConfigs:     List[ItcInstrumentConfig],
  allTargets:          TargetList,
  customSedTimestamps: List[Timestamp]
) derives Eq:

  private val constraints = observation.constraints
  private val asterismIds = observation.scienceTargetIds

  private val itcTargets: EitherNec[ItcTargetProblem, NonEmptyList[ItcTarget]] =
    asterismIds.toItcTargets(allTargets)

  private val obsModeConfigs: Option[NonEmptyList[(ItcInstrumentConfig, ExposureTimeMode)]] =
    NonEmptyList.fromList(observation.toInstrumentConfig(allTargets))

  private def requirementsExposureTimeMode: EitherNec[ItcQueryProblem, ExposureTimeMode] =
    observation.scienceRequirements.exposureTimeMode.toRightNec(
      ItcQueryProblem.MissingExposureTimeMode
    )

  private def requirementsConfigs
    : EitherNec[ItcQueryProblem, NonEmptyList[(ItcInstrumentConfig, ExposureTimeMode)]] =
    requirementsExposureTimeMode.flatMap: etm =>
      NonEmptyList
        .fromList(
          selectedConfigs.map((_, etm))
        )
        .toRightNec(ItcQueryProblem.GenericError(Constants.MissingMode))

  private val finalConfigs
    : EitherNec[ItcQueryProblem, NonEmptyList[(ItcInstrumentConfig, ExposureTimeMode)]] =
    obsModeConfigs
      .map(_.rightNec)
      .getOrElse(requirementsConfigs)

  private val queryProps: EitherNec[ItcQueryProblem, ItcImagingQuerier.QueryProps] =
    for {
      t       <- itcTargets.leftMap(_.map(_.problem))
      configs <- finalConfigs
    } yield ItcImagingQuerier.QueryProps(constraints, t, configs.toList, customSedTimestamps)

  def requestCalculations(using
    WorkerClient[IO, ItcMessage.Request]
  ): IO[EitherNec[ItcQueryProblem, ImagingResults]] =
    def action(
      qp: ItcImagingQuerier.QueryProps
    ): IO[ImagingResults] =
      ItcClient[IO]
        .request:
          ItcMessage.Query(
            qp.constraints,
            qp.targets,
            qp.customSedTimestamps,
            qp.instrumentConfigs
          )
        .map(
          _.compile.toList
        )
        .use: listF =>
          listF.map: list =>
            list.foldLeft(ImagingResults.empty): (acc, entry) =>
              val (params, result) = entry
              val newEntries: List[
                (ItcTarget, ItcRequestParams, EitherNec[ItcTargetProblem, ItcResult])
              ] =
                result match
                  case Left(problem)    =>
                    // For failures, we want to keep track of the problem for each target in the asterism
                    params.asterism.toList
                      .map(target => (target, params, problem.asLeft[ItcResult]))
                  case Right(itcResult) =>
                    itcResult match
                      // We should never actually have Pending results here
                      case ItcResult.Pending                       => List.empty
                      case ItcResult.Result(times, brightestIndex) =>
                        // Split the results into 1 per target, and make a target map for easier access by target.
                        params.asterism.toList
                          .zip(times.toList)
                          .map: (target, time) =>
                            (target,
                             params,
                             ItcResult
                               .Result(Zipper.of(time), brightestIndex)
                               .rightNec
                            )
              newEntries.foldLeft(acc):
                case (acc, (target, params, result)) =>
                  acc.updatedWith(target):
                    case Some(targetMap) => Some(targetMap + (params -> result))
                    case None            => Some(Map(params -> result))

    (for {
      qp <- EitherT(queryProps.pure[IO])
      r  <- EitherT.right(action(qp))
    } yield r).value

object ItcImagingQuerier:
  private case class QueryProps(
    constraints:         ConstraintSet,
    targets:             NonEmptyList[ItcTarget],
    instrumentConfigs:   List[(ItcInstrumentConfig, ExposureTimeMode)],
    customSedTimestamps: List[Timestamp]
  ) derives Eq

  private given Reusability[QueryProps] = Reusability.byEq
  given Reusability[ItcImagingQuerier]  = Reusability.by(_.queryProps)

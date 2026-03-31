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
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.Timestamp
import queries.schemas.itc.syntax.*
import workers.WorkerClient

case class ItcGraphQuerier(
  observation:         Observation,
  configs:             Option[ItcInstrumentConfig], // configs for imaging or single config for spectroscopy
  allTargets:          TargetList,
  customSedTimestamps: List[Timestamp]
) derives Eq:

  private val constraints = observation.constraints
  private val asterismIds = observation.scienceTargetIds

  // The remote configuration is read in a different query than the itc results.
  // This will work even in the case the user has overriden some parameters.
  // When we use the remote configuration we don't need the exposure time.
  private val remoteConfig: Option[ItcInstrumentConfig] =
    observation
      .toInstrumentConfig(allTargets)
      .headOption

  private def requirementsExposureTimeMode: EitherNec[ItcQueryProblem, ExposureTimeMode] =
    observation.scienceRequirements.exposureTimeMode.toRightNec(
      ItcQueryProblem.MissingExposureTimeMode
    )

  private def requirementsConfig: EitherNec[ItcQueryProblem, ItcInstrumentConfig] =
    // If the user has set an exposure time mode, it will be part of the config,
    // but we still need to makes sure it is set in the requirements or it is not valid.
    requirementsExposureTimeMode.flatMap: _ =>
      configs
        .map(_.rightNec)
        .getOrElse(
          ItcQueryProblem.GenericError(Constants.MissingMode).leftNec
        )

  // If the observation has an assigned configuration, we use that one.
  // Otherwise, we use the first one from the provided configs (for spectroscopy compatibility).
  private val finalConfig: EitherNec[ItcQueryProblem, ItcInstrumentConfig] =
    remoteConfig.fold(requirementsConfig)(_.rightNec)

  private val itcTargets: EitherNec[ItcTargetProblem, NonEmptyList[ItcTarget]] =
    asterismIds.toItcTargets(allTargets)

  private val queryProps: EitherNec[ItcTargetProblem, ItcGraphQuerier.QueryProps] =
    for {
      t <- itcTargets
      i <- finalConfig.leftMap(_.map(_.toTargetProblem))
    } yield ItcGraphQuerier.QueryProps(constraints, t, i, customSedTimestamps)

  // Returns graphs for each target and the brightest target
  def requestGraphs(using
    WorkerClient[IO, ItcMessage.Request]
  ): IO[EitherNec[ItcTargetProblem, (ItcAsterismGraphResults, ItcInstrumentConfig)]] =
    def action(
      qp: ItcGraphQuerier.QueryProps
    ): IO[EitherNec[ItcTargetProblem, (ItcAsterismGraphResults, ItcInstrumentConfig)]] =
      ItcClient[IO]
        .requestSingle:
          ItcMessage.GraphQuery(
            qp.constraints,
            qp.targets,
            qp.customSedTimestamps,
            qp.instrumentConfig
          )
        .map(
          _.fold(
            ItcQueryProblem.GenericError("No response from ITC server").toTargetProblem.leftNec
          )(
            _.leftMap(_.map(_.toTargetProblem))
          )
        )
        .map(_.map((_, qp.instrumentConfig)))

    (for {
      qp <- EitherT(queryProps.pure[IO])
      r  <- EitherT(action(qp))
    } yield r).value

object ItcGraphQuerier:
  private case class QueryProps(
    constraints:         ConstraintSet,
    targets:             NonEmptyList[ItcTarget],
    instrumentConfig:    ItcInstrumentConfig,
    customSedTimestamps: List[Timestamp]
  ) derives Eq

  private given Reusability[QueryProps] = Reusability.byEq
  given Reusability[ItcGraphQuerier]    = Reusability.by(_.queryProps)

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.odb

import cats.effect.Sync
import cats.syntax.all.*
import clue.FetchClient
import clue.syntax.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.model.Observation
import lucuma.schemas.ObservationDB
import observe.common.ObsQueriesGql.*
import observe.model.dhs.*
import observe.server.ObserveFailure

trait OdbProxy[F[_]] private[odb] () extends OdbCommands[F] {
  def read(oid:               Observation.Id): F[OdbObservationData]
  def resetAcquisition(obsId: Observation.Id): F[Unit]
}

object OdbProxy {

  def apply[F[_]](
    evCmds: OdbCommands[F]
  )(using FetchClient[F, ObservationDB])(using F: Sync[F]): OdbProxy[F] =
    new OdbProxy[F] {
      def read(oid: Observation.Id): F[OdbObservationData] =
        ObsCalibrationRoleQuery[F]
          .query(oid)
          .raiseGraphQLErrors
          .map(_.observation.flatMap(_.calibrationRole))
          .flatMap: calibrationRole =>
            val skipTargets: Boolean = calibrationRole.contains_(CalibrationRole.DaytimePinhole)
            ObsQuery[F]
              .query(oid, skipTargets)
              .raiseGraphQLErrors
              .flatMap: data =>
                (data.observation, data.executionConfig).tupled
                  .fold(
                    F.raiseError[OdbObservationData]:
                      ObserveFailure.Unexpected(s"OdbProxy: Unable to read observation $oid")
                  )((obs, ec) => OdbObservationData(obs, ec).pure[F])

      def resetAcquisition(obsId: Observation.Id): F[Unit] =
        ResetAcquisitionMutation[F].execute(obsId = obsId).void

      export evCmds.*
    }

}

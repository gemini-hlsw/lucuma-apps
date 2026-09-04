// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.odb

import cats.effect.Sync
import cats.syntax.all.*
import clue.FetchClient
import clue.syntax.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.model.Observation
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.schemas.ObservationDB
import observe.common.ObsQueriesGql.*
import observe.model.dhs.*
import observe.server.ObserveFailure

trait OdbProxy[F[_]] private[odb] () extends OdbCommands[F] {
  def read(oid:                Observation.Id): F[OdbObservationData]
  def readExecutionConfig(oid: Observation.Id): F[InstrumentExecutionConfig]
  def resetAcquisition(obsId:  Observation.Id): F[Unit]
}

object OdbProxy {

  def apply[F[_]](
    evCmds: OdbCommands[F]
  )(using FetchClient[F, ObservationDB])(using F: Sync[F]): OdbProxy[F] =
    new OdbProxy[F] {
      override def read(oid: Observation.Id): F[OdbObservationData] =
        evCmds.flushEvents(oid) >>
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

      // The sequence the ODB generates depends on the events we sent, so they have to be in first.
      override def readExecutionConfig(oid: Observation.Id): F[InstrumentExecutionConfig] =
        evCmds.flushEvents(oid) >>
          ObsExecutionQuery[F]
            .query(oid)
            .raiseGraphQLErrors
            .flatMap {
              _.executionConfig.fold(
                F.raiseError[InstrumentExecutionConfig]:
                  ObserveFailure.Unexpected(s"OdbProxy: Unable to read observation $oid")
              )(_.pure[F])
            }

      override def resetAcquisition(obsId: Observation.Id): F[Unit] =
        evCmds.flushEvents(obsId) >>
          ResetAcquisitionMutation[F].execute(obsId = obsId).void

      export evCmds.*
    }

}

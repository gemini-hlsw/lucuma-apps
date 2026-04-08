// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.odb

import cats.effect.Sync
import cats.syntax.all.*
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Step
import observe.model.dhs.*
import observe.model.odb.ObsRecordedIds
import observe.server.ObserveFailure

class DummyOdbProxy[F[_]: Sync as F] extends OdbProxy[F] {
  val evCmds = new DummyOdbCommands[F]

  override def read(oid: Observation.Id): F[OdbObservationData] =
    F.raiseError(ObserveFailure.Unexpected("TestOdbProxy.read: Not implemented."))

  override def resetAcquisition(obsId: Observation.Id): F[Unit] =
    F.raiseError(ObserveFailure.Unexpected("TestOdbProxy.resetAcquisition: Not implemented."))

  export evCmds.{
    datasetEndExposure,
    datasetEndReadout,
    datasetEndWrite,
    datasetStartExposure,
    datasetStartReadout,
    datasetStartWrite,
    obsContinue,
    obsPause,
    obsStop,
    sequenceStart,
    stepContinue,
    stepPause,
    stepStop
  }

  override def stepStartStep[D](
    obsId:  Observation.Id,
    stepId: Step.Id
  ): F[Unit] = F.unit

  override def stepStartConfigure(obsId: Observation.Id, stepId: Step.Id): F[Unit] =
    F.unit

  override def stepEndConfigure(obsId: Observation.Id, stepId: Step.Id): F[Boolean] = false.pure

  override def stepStartObserve(obsId: Observation.Id, stepId: Step.Id): F[Boolean] = false.pure

  override def stepEndObserve(obsId: Observation.Id, stepId: Step.Id): F[Boolean] = false.pure

  override def stepEndStep(obsId: Observation.Id, stepId: Step.Id): F[Boolean] = false.pure

  override def stepAbort(obsId: Observation.Id, stepId: Step.Id): F[Boolean] = false.pure

  override def visitStart(obsId: Observation.Id): F[Unit] = F.unit

  override def getCurrentRecordedIds: F[ObsRecordedIds] = ObsRecordedIds.Empty.pure
}

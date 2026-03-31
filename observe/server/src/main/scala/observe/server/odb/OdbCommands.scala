// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.odb

import lucuma.core.model.Observation
import lucuma.core.model.sequence.Step
import observe.common.EventsGQL.*
import observe.model.dhs.*
import observe.model.odb.ObsRecordedIds

trait OdbCommands[F[_]] private[odb] () {
  def visitStart[S](obsId:       Observation.Id, staticCfg: S): F[Unit]
  def sequenceStart(obsId:       Observation.Id): F[Unit]
  def stepStartStep[D](obsId:    Observation.Id, stepId:    Step.Id): F[Unit]
  def stepStartConfigure(obsId:  Observation.Id, stepId:    Step.Id): F[Unit]
  def stepEndConfigure(obsId:    Observation.Id, stepId:    Step.Id): F[Boolean]
  def stepStartObserve(obsId:    Observation.Id, stepId:    Step.Id): F[Boolean]
  def datasetStartExposure(
    obsId:  Observation.Id,
    stepId: Step.Id,
    fileId: ImageFileId
  ): F[RecordDatasetMutation.Data.RecordDataset.Dataset]
  def datasetEndExposure(obsId:  Observation.Id, fileId:    ImageFileId): F[Boolean]
  def datasetStartReadout(obsId: Observation.Id, fileId:    ImageFileId): F[Boolean]
  def datasetEndReadout(obsId:   Observation.Id, fileId:    ImageFileId): F[Boolean]
  def datasetStartWrite(obsId:   Observation.Id, fileId:    ImageFileId): F[Boolean]
  def datasetEndWrite(obsId:     Observation.Id, fileId:    ImageFileId): F[Boolean]
  def stepEndObserve(obsId:      Observation.Id, stepId:    Step.Id): F[Boolean]
  def stepEndStep(obsId:         Observation.Id, stepId:    Step.Id): F[Boolean]
  def stepAbort(obsId:           Observation.Id, stepId:    Step.Id): F[Boolean]
  def stepStop(obsId:            Observation.Id, stepId:    Step.Id): F[Boolean]
  def stepPause(obsId:           Observation.Id, stepId:    Step.Id): F[Boolean]
  def stepContinue(obsId:        Observation.Id, stepId:    Step.Id): F[Boolean]
  def obsContinue(obsId:         Observation.Id): F[Boolean]
  def obsPause(obsId:            Observation.Id): F[Boolean]
  def obsStop(obsId:             Observation.Id): F[Boolean]

  def getCurrentRecordedIds: F[ObsRecordedIds]
}

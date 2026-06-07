// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.gnirs

import cats.Show
import fs2.Stream
import lucuma.core.enums.StepType as CoreStepType
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsStaticConfig
import lucuma.core.util.TimeSpan
import observe.model.dhs.ImageFileId
import observe.model.enums.ObserveCommandResult
import observe.server.Progress

trait GnirsController[F[_]] {

  def applyConfig(config: GnirsController.GnirsConfig): F[Unit]

  def observe(fileId: ImageFileId, expTime: TimeSpan): F[ObserveCommandResult]

  // endObserve is to notify the completion of the observation, not to cause its end.
  def endObserve: F[Unit]

  def stopObserve: F[Unit]

  def abortObserve: F[Unit]

  def observeProgress(total: TimeSpan): Stream[F, Progress]
}

object GnirsController {

  // Observe is a passthrough of the ODB-resolved static and dynamic configs; the controller maps
  // each field directly to the instrument. `stepType` is only used to recognize dark steps (cover
  // closed, dark filter). No observing-mode resolution is done here (the ODB already did it).
  final case class GnirsConfig(
    staticConfig:  GnirsStaticConfig,
    dynamicConfig: GnirsDynamicConfig,
    stepType:      CoreStepType
  )

  object GnirsConfig {
    given Show[GnirsConfig] = Show.fromToString
  }

  /**
   * Total observation time for a GNIRS dynamic config: coadds * (exposure + readout per coadd). The
   * per-coadd readout time is provided by the read mode in lucuma-core.
   */
  def calcObserveTime(dc: GnirsDynamicConfig): TimeSpan =
    (dc.exposure +| dc.readMode.readoutTimePerCoadd) *| dc.coadds.value
}

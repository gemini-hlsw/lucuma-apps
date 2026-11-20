// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.odb

import lucuma.core.enums.CalibrationRole
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.syntax.isDaytime

/** What says whether the daytime queue accepts an observation: its role and mode. */
case class OdbQueueObservation(
  calibrationRole: Option[CalibrationRole],
  observingMode:   Option[BasicConfiguration]
):
  /** Daytime observations are the only ones the daytime queue accepts. */
  def isDaytime: Boolean = calibrationRole.isDaytime(observingMode)

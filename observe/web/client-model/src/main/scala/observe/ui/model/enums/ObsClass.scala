// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.model.enums

import cats.Order
import cats.derived.*
import lucuma.core.enums.CalibrationRole
import lucuma.schemas.model.BasicConfiguration

enum ObsClass(val label: String) derives Order:
  case Daytime   extends ObsClass("Day")
  case Nighttime extends ObsClass("Night")

object ObsClass:
  // Twilight flats are night calibrations: they need no ITC results but do need offsets.
  def fromObservation(
    calibrationRole: Option[CalibrationRole],
    observingMode:   Option[BasicConfiguration]
  ): ObsClass =
    (calibrationRole, observingMode) match
      case (Some(CalibrationRole.DaytimePinhole), _) |
          (_, Some(BasicConfiguration.DaytimeCalibration(_))) =>
        ObsClass.Daytime
      case _ =>
        ObsClass.Nighttime

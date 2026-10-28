// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.syntax

import lucuma.core.enums.CalibrationRole
import lucuma.schemas.model.BasicConfiguration

trait calibration:
  extension (cr: CalibrationRole)
    def needsITC: Boolean = cr match
      case CalibrationRole.Twilight           => false
      case CalibrationRole.DaytimePinhole     => false
      case CalibrationRole.DaytimeCalibration => false
      case _                                  => true

  extension (cr: Option[CalibrationRole])
    def needsITC: Boolean = cr.forall(_.needsITC)

    // GNIRS pinholes are daytime too, but they use a regular spectroscopy observing mode.
    def isDaytime(observingMode: Option[BasicConfiguration]): Boolean =
      (cr, observingMode) match
        case (Some(CalibrationRole.DaytimePinhole | CalibrationRole.DaytimeCalibration), _) |
            (_, Some(BasicConfiguration.DaytimeCalibration(_))) =>
          true
        case _ => false

object calibration extends calibration

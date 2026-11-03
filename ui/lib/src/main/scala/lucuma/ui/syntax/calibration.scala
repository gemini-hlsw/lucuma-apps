// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.syntax

import lucuma.core.enums.CalibrationRole
import lucuma.schemas.model.BasicConfiguration

trait calibration:
  extension (cr: CalibrationRole)
    def needsITC: Boolean = cr match
      case CalibrationRole.Twilight           => false
      case CalibrationRole.DaytimeArc         => false
      case CalibrationRole.DaytimeCalibration => false
      case CalibrationRole.DaytimeFlat        => false
      case CalibrationRole.DaytimePinhole     => false
      case _                                  => true

    def isDaytimeRole: Boolean = cr match
      case CalibrationRole.DaytimeArc | CalibrationRole.DaytimeCalibration |
          CalibrationRole.DaytimeFlat | CalibrationRole.DaytimePinhole =>
        true
      case _ => false

  extension (cr: Option[CalibrationRole])
    def needsITC: Boolean = cr.forall(_.needsITC)

    // Pinholes, flats and arcs are daytime too, but they use a regular science observing mode.
    def isDaytime(observingMode: Option[BasicConfiguration]): Boolean =
      (cr, observingMode) match
        case (Some(role), _) if role.isDaytimeRole               => true
        case (_, Some(BasicConfiguration.DaytimeCalibration(_))) => true
        case _                                                   => false

object calibration extends calibration

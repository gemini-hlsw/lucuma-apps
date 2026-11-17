// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.syntax

import lucuma.core.enums.CalibrationRole

trait calibration:
  extension (cr: CalibrationRole)
    def needsITC: Boolean = cr match
      case CalibrationRole.Twilight           => false
      case CalibrationRole.DaytimeArc         => false
      case CalibrationRole.DaytimeCalibration => false
      case CalibrationRole.DaytimeFlat        => false
      case CalibrationRole.DaytimePinhole     => false
      case CalibrationRole.DaytimeMaskImage   => false
      case _                                  => true

    def needsOffsets: Boolean = cr match
      case CalibrationRole.DaytimeArc | CalibrationRole.DaytimeFlat |
          CalibrationRole.DaytimePinhole | CalibrationRole.DaytimeMaskImage =>
        false
      case _ => true

  extension (cr: Option[CalibrationRole])
    def needsITC: Boolean = cr.forall(_.needsITC)

    def needsOffsets: Boolean = cr.forall(_.needsOffsets)

object calibration extends calibration

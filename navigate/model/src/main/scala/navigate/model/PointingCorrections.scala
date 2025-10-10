// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import cats.Eq
import cats.derived.*
import lucuma.core.math.Angle
import navigate.model.HandsetAdjustment.HorizontalAdjustment

case class PointingCorrections(
  local: HorizontalAdjustment,
  guide: HorizontalAdjustment
) derives Eq

object PointingCorrections {
  val default: PointingCorrections = PointingCorrections(
    local = HorizontalAdjustment(Angle.Angle0, Angle.Angle0),
    guide = HorizontalAdjustment(Angle.Angle0, Angle.Angle0)
  )
}

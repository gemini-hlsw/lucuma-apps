// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import cats.Show
import cats.syntax.all.*
import lucuma.core.math.Angle

case class RotatorTrackConfig(
  ipa:  Angle,
  mode: RotatorTrackingMode
)

object RotatorTrackConfig {

  given Show[RotatorTrackConfig] =
    Show.show(r => f"RotatorTrackConfig(ipa = ${r.ipa.toDoubleDegrees}%.2f, mode = ${r.mode.show})")

}

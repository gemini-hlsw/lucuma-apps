// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import cats.Show
import cats.derived.*

case class TrackingConfig(
  nodAchopA: Boolean,
  nodAchopB: Boolean,
  nodBchopA: Boolean,
  nodBchopB: Boolean
) derives Show {
  lazy val active: Boolean = nodAchopA || nodAchopB || nodBchopA || nodBchopB
}

object TrackingConfig {
  val default: TrackingConfig    = TrackingConfig(true, false, false, true)
  val noTracking: TrackingConfig = TrackingConfig(false, false, false, false)
}

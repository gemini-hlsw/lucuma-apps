// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import lucuma.core.util.TimeSpan

case class WfsConfiguration(
  exposureTime: TimeSpan,
  saving:       Boolean
)

object WfsConfiguration {
  val default: WfsConfiguration = WfsConfiguration(TimeSpan.Zero, false)
}

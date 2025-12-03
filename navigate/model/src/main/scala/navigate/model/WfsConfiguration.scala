// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import cats.Eq
import cats.derived.*
import lucuma.core.util.TimeSpan
import monocle.Focus

case class WfsConfiguration(
  exposureTime: TimeSpan,
  saving:       Boolean
) derives Eq

object WfsConfiguration {
  val exposureTime = Focus[WfsConfiguration](_.exposureTime)
  val saving       = Focus[WfsConfiguration](_.saving)

  val default: WfsConfiguration = WfsConfiguration(TimeSpan.Zero, false)
}

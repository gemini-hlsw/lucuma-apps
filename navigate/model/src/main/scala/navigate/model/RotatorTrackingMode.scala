// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import cats.Show
import cats.derived.*
import lucuma.core.util.Enumerated

enum RotatorTrackingMode(val tag: String) derives Enumerated, Show {
  case Tracking extends RotatorTrackingMode("Tracking")
  case Fixed    extends RotatorTrackingMode("Fixed")
}

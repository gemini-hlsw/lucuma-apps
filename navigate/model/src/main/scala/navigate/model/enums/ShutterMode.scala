// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model.enums

import navigate.model.Distance

enum ShutterMode(val tag: String) {
  case FullyOpen extends ShutterMode("FullyOpen")
  case Tracking(aperture: Distance) extends ShutterMode("Tracking")
}

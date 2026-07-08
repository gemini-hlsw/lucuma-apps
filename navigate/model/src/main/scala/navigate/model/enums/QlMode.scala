// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model.enums

import lucuma.core.util.Enumerated

enum QlMode(val tag: String) derives Enumerated {
  case Off extends QlMode("Off")
  case On extends QlMode("On")
  case Auto extends QlMode("Auto")
}
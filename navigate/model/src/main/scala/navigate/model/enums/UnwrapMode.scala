// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model.enums

import lucuma.core.util.Enumerated

enum UnwrapMode(val tag: String) derives Enumerated {
  case Minus extends UnwrapMode("Minus")
  case Auto  extends UnwrapMode("Auto")
  case Plus  extends UnwrapMode("Plus")
}

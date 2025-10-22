// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import lucuma.core.util.Enumerated

enum ChopSource(val tag: String) derives Enumerated {
  case Scs  extends ChopSource("SCS")
  case Ics0 extends ChopSource("ICS0")
  case Ics1 extends ChopSource("ICS1")
  case Ics2 extends ChopSource("ICS2")
  case Ics3 extends ChopSource("ICS3")
  case Ics4 extends ChopSource("ICS4")
}

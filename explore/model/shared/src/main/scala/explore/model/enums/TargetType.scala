// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

enum TargetType(val tag: String, val label: String) derives Enumerated:
  case Sidereal    extends TargetType("sidereal", "Sidereal")
  case Nonsidereal extends TargetType("nonsidereal", "Nonsidereal")

object TargetType:
  given Display[TargetType] = Display.byShortName(_.label)

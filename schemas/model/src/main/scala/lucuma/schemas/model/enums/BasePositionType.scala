// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model.enums

import lucuma.core.util.Enumerated

enum BasePositionType(val tag: String) derives Enumerated:
  case SingleTarget extends BasePositionType("SINGLE_TARGET")
  case Asterism     extends BasePositionType("ASTERISM")
  case ExplicitBase extends BasePositionType("EXPLICIT_BASE")

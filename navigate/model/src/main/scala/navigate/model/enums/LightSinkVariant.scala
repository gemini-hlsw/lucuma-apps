// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model.enums

import lucuma.core.util.Enumerated

enum LightSinkVariant(val tag: String) derives Enumerated {
  case GmosIfu extends LightSinkVariant("GmosIfu")
  case NiriF6 extends LightSinkVariant("NiriF6")
  case NiriF14 extends LightSinkVariant("NiriF14")
  case NiriF32 extends LightSinkVariant("NiriF32")
}
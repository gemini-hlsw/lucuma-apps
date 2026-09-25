// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model.navigate

import lucuma.core.util.Enumerated

enum LightSource(val tag: String) derives Enumerated {
  case Sky extends LightSource("Sky")

  case AO extends LightSource("Ao")

  case GCAL extends LightSource("Gcal")
}

object LightSource {
  extension (self: LightSource) {
    def sendsStarlight: Boolean = self match {
      case Sky | AO => true
      case GCAL     => false
    }
  }
}

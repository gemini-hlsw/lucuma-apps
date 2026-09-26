// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import lucuma.schemas.model.navigate.LightSource
import navigate.model.enums.LightSink

case class LightPath(
  from: LightSource,
  to:   LightSink
)

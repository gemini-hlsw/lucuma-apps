// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import navigate.model.enums.LightSink
import navigate.model.enums.LightSource

case class LightPath(
  from: LightSource,
  to:   LightSink
)

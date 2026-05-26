// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import lucuma.core.enums.Instrument

// This trait contains all the constant information of an instrument
trait InstrumentStaticInfo {
  val instrument: Instrument
}

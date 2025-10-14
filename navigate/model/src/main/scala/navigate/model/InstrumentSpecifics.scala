// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import cats.Show
import lucuma.core.math.Angle
import navigate.model.Distance

case class InstrumentSpecifics(
  iaa:         Angle,
  focusOffset: Distance,
  agName:      String,
  origin:      Origin
)

object InstrumentSpecifics {
  given Show[InstrumentSpecifics] = Show.show { i =>
    f"InstrumentSpecifics(iaa = ${i.iaa.toDoubleDegrees}%.2f, focusOffset = ${i.focusOffset.toMillimeters.value.toDouble}%.2f, agName = ${i.agName}, origin = ${i.origin})"
  }
}

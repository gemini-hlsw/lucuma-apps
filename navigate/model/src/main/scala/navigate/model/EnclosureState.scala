// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import lucuma.core.model.IntPercent
import navigate.model.enums.DomeMode
import navigate.model.enums.ShutterMode

case class EnclosureState(
  dome:             Option[DomeMode],
  shutters:         Option[ShutterMode],
  eastVentGateOpen: IntPercent,
  westVentGateOpen: IntPercent
) derives Eq

object EnclosureState {
  val default: EnclosureState =
    EnclosureState(none, none, IntPercent.unsafeFrom(0), IntPercent.unsafeFrom(0))
}

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import cats.Eq
import cats.derived.*

case class EnclosureState(
  domeEnabled:      Boolean,
  shuttersEnabled:  Boolean,
  eastVentGateOpen: Boolean,
  westVentGateOpen: Boolean
) derives Eq

object EnclosureState {
  val default: EnclosureState = EnclosureState(false, false, false, false)
}

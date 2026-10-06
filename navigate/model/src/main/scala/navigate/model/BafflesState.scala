// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import cats.Eq
import cats.derived.*
import navigate.model.enums.CentralBafflePosition
import navigate.model.enums.DeployableBafflePosition

case class BafflesState(central: CentralBafflePosition, deployable: DeployableBafflePosition)
    derives Eq

object BafflesState:
  val default: BafflesState =
    BafflesState(CentralBafflePosition.Open, DeployableBafflePosition.Visible)

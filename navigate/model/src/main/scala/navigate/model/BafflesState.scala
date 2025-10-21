// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import navigate.model.enums.CentralBafflePosition
import navigate.model.enums.DeployableBafflePosition

case class BafflesState(central: CentralBafflePosition, deployable: DeployableBafflePosition)

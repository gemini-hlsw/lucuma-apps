// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import cats.Show
import cats.derived.*
import cats.syntax.all.*
import lucuma.core.enums.Instrument
import lucuma.core.math.Wavelength
import navigate.model.enums.CentralBafflePosition
import navigate.model.enums.DeployableBafflePosition

sealed trait BafflesConfig derives Show

object BafflesConfig {
  case class ManualConfig(central: CentralBafflePosition, deployable: DeployableBafflePosition)
      extends BafflesConfig
  case class AutoConfig(visibleLimit: Wavelength, nearirLimit: Wavelength) extends BafflesConfig

  extension (ac: AutoConfig) {
    def toManualConfig(centralWavelength: Wavelength, instrument: Instrument): ManualConfig =
      instrument match {
        case Instrument.Gpi =>
          ManualConfig(CentralBafflePosition.Closed, DeployableBafflePosition.ThermalIr)
        case _              =>
          if (centralWavelength <= ac.visibleLimit)
            ManualConfig(CentralBafflePosition.Closed, DeployableBafflePosition.Visible)
          else if (centralWavelength <= ac.nearirLimit)
            ManualConfig(CentralBafflePosition.Open, DeployableBafflePosition.NearIr)
          else
            ManualConfig(CentralBafflePosition.Open, DeployableBafflePosition.ThermalIr)
      }
  }

  extension (bc: BafflesConfig) {
    def deployable(
      centralWavelength: Wavelength,
      instrument:        Instrument
    ): DeployableBafflePosition = bc match {
      case ManualConfig(_, deployable) => deployable
      case x: AutoConfig               => x.toManualConfig(centralWavelength, instrument).deployable
    }
    def central(centralWavelength: Wavelength, instrument: Instrument): CentralBafflePosition =
      bc match {
        case ManualConfig(central, _) => central
        case x: AutoConfig            => x.toManualConfig(centralWavelength, instrument).central
      }
  }

}

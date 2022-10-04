// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.itc

import cats.data.*
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import explore.model.itc.*
import explore.modes.InstrumentRow
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.NonNegDuration
import lucuma.core.util.NewType

object CoverageCenterWavelength extends NewType[Wavelength]
type CoverageCenterWavelength = CoverageCenterWavelength.Type

case class ItcRequestParams(
  wavelength:    CoverageCenterWavelength,
  signalToNoise: PosBigDecimal,
  constraints:   ConstraintSet,
  target:        ItcTarget,
  mode:          InstrumentRow
)

case class ItcGraphRequestParams(
  wavelength:   CoverageCenterWavelength,
  exposureTime: NonNegDuration,
  exposures:    PosInt,
  constraints:  ConstraintSet,
  target:       NonEmptyList[ItcTarget],
  mode:         InstrumentRow
)

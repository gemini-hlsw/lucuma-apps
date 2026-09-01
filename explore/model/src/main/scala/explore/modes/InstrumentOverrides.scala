// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.modes

import cats.Eq
import cats.derived.*
import cats.syntax.option.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.GmosRoi
import lucuma.core.math.Wavelength
import lucuma.core.model.GmosIfuAnalysis
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.util.Enumerated
import lucuma.schemas.model.CentralWavelength

enum InstrumentOverrides derives Eq:
  // `ifuAnalysis` is empty for every mode but the IFU, where it is the sampling the ITC
  // integrates over: without it the ITC silently answers for its own default instead.
  case GmosSpectroscopy(
    centralWavelength: CentralWavelength,
    ccdMode:           GmosCcdMode,
    roi:               GmosRoi,
    ifuAnalysis:       Option[GmosIfuAnalysis] = none
  )
  case GnirsSpectroscopy(centralWavelength: CentralWavelength, coadds: PosInt)

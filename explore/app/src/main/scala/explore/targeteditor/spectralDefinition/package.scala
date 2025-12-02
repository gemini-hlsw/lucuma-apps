// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor.spectralDefinition

import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.schemas.ObservationDB.Types.UnnormalizedSedInput

private val DefaultUnnormalizedSedInput: UnnormalizedSedInput =
  UnnormalizedSedInput.StellarLibrary(StellarLibrarySpectrum.O5V)

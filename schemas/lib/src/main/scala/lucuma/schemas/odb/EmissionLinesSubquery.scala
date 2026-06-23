// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import io.circe.Decoder
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.model.SpectralDefinition
import lucuma.odb.json.sourceprofile.given
import lucuma.schemas.ObservationDB

// The shared selection is validated against every concrete root type the subclasses use.
@GraphQLType("EmissionLinesIntegrated", "EmissionLinesSurface")
class EmissionLinesSubquery[T](using
  Decoder[SpectralDefinition.EmissionLines[T]]
) extends GraphQLSubquery.Typed[ObservationDB, SpectralDefinition.EmissionLines[T]]:
  override val subquery: String = s"""
        {
          lines {
            wavelength $WavelengthSubquery
            lineWidth
            lineFlux {
              value
              units
            }
          }
          fluxDensityContinuum {
            value
            units
          }
        }
      """

object EmissionLinesIntegratedSubquery extends EmissionLinesSubquery[Integrated]

object EmissionLinesSurfaceSubquery extends EmissionLinesSubquery[Surface]

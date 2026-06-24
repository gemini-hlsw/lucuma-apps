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
@GraphQLType("BandNormalizedIntegrated", "BandNormalizedSurface")
class BandNormalizedSubquery[T](using
  Decoder[SpectralDefinition.BandNormalized[T]]
) extends GraphQLSubquery.Typed[ObservationDB, SpectralDefinition.BandNormalized[T]]:
  override val subquery: String = s"""
        {
          sed $UnnormalizedSEDSubquery
          brightnesses $BandBrightnessIntegratedSubquery
        }
      """

object BandNormalizedIntegratedSubquery extends BandNormalizedSubquery[Integrated]

object BandNormalizedSurfaceSubquery extends BandNormalizedSubquery[Surface]

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import io.circe.Decoder
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessUnits.*
import lucuma.odb.json.sourceprofile.*
import lucuma.schemas.ObservationDB

// The shared selection is validated against every concrete root type the subclasses use.
@GraphQLType("BandBrightnessIntegrated", "BandBrightnessSurface")
class BandBrightnessSubquery[T](
  override val dataDecoder: Decoder[(Band, BrightnessMeasure[T])]
) extends GraphQLSubquery[ObservationDB]:
  override type Data = (Band, BrightnessMeasure[T])

  override val subquery: String = """
        {
          band
          value
          units
          error
        }
      """
object BandBrightnessIntegratedSubquery
    extends BandBrightnessSubquery[Integrated](CodecBandBrightness[Integrated])

object BandBrightnessSurfaceSubquery
    extends BandBrightnessSubquery[Surface](CodecBandBrightness[Surface])

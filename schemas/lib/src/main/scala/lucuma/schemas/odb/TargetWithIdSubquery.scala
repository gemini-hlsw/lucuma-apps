// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import lucuma.schemas.ObservationDB
import lucuma.schemas.decoders.given
import lucuma.schemas.model.TargetWithId

@GraphQLType("Target")
object TargetWithIdSubquery extends GraphQLSubquery.Typed[ObservationDB, TargetWithId]:
  override val subquery: String = s"""  
    {
      id
      name
      sidereal $SiderealSubquery
      nonsidereal {
        key
      }
      opportunity {
        region {
          rightAscensionArc {
            type
            start $RASubquery
            end $RASubquery
          }
          declinationArc {
            type
            start $DecSubquery
            end $DecSubquery
          }
        }
      }
      sourceProfile {
        point {
          bandNormalized $BandNormalizedIntegratedSubquery
          emissionLines $EmissionLinesIntegratedSubquery
        }
        uniform {
          bandNormalized $BandNormalizedSurfaceSubquery
          emissionLines $EmissionLinesSurfaceSubquery
        }
        gaussian {
          fwhm $AngleSubquery
          bandNormalized $BandNormalizedIntegratedSubquery
          emissionLines $EmissionLinesIntegratedSubquery
        }
      }
      disposition
      calibrationRole
    }
  """

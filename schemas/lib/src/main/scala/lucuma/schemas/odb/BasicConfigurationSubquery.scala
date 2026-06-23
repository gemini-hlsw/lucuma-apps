// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import lucuma.schemas.ObservationDB
import lucuma.schemas.model.BasicConfiguration

@GraphQLType("ObservingMode")
object BasicConfigurationSubquery
    extends GraphQLSubquery.Typed[ObservationDB, BasicConfiguration]:
  override val subquery: String = s"""
        {
          instrument
          gmosNorthLongSlit {
            grating
            filter
            fpu
            centralWavelength $WavelengthSubquery
          }
          gmosSouthLongSlit {
            grating
            filter
            fpu
            centralWavelength $WavelengthSubquery
          }
          gmosNorthImaging {
            filters {
              filter
            }
          }
          gmosSouthImaging {
            filters {
              filter
            }
          }
          flamingos2LongSlit {
            disperser
            filter
            fpu
          }
          igrins2LongSlit {
            offsetMode
          }
          gnirsLongSlit {
            filter
            fpu
            prism
            grating
            camera
            centralWavelength $WavelengthSubquery
          }
          ghostIfu {
            resolutionMode
            stepCount
            red {
              exposureTimeMode $ExposureTimeModeSubquery
              readMode
              binning
            }
            blue {
              exposureTimeMode $ExposureTimeModeSubquery
              readMode
              binning
            }
          }
          visitor {
            mode
            centralWavelength $WavelengthSubquery
            agsDiameter $AngleSubquery
          }
        }
      """

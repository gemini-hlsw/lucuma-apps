// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import lucuma.schemas.ObservationDB
import lucuma.schemas.model.BasicConfiguration

@GraphQLType("ObservingMode")
object BasicConfigurationSubquery extends GraphQLSubquery.Typed[ObservationDB, BasicConfiguration]:
  override val subquery = gql"""
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
          gmosNorthMos {
            grating
            filter
            customMask {
              slitWidth
            }
            centralWavelength $WavelengthSubquery
          }
          gmosSouthMos {
            grating
            filter
            customMask {
              slitWidth
            }
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
          flamingos2Imaging {
            filters {
              filter
            }
          }
          igrins2LongSlit {
            __typename
          }
          gnirsImaging {
            filters {
              filter
            }
            camera
          }
          gnirsSpectroscopy {
            filter
            slit { fpu }
            ifu { fpu }
            prism
            grating
            camera
            centralWavelengths {
              centralWavelength $WavelengthSubquery
            }
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
            scienceFovDiameter $AngleSubquery
          }
          exchange {
            keckInstrument
            subaruInstrument
            totalRequestTime $TimeSpanSubquery
          }
        }
      """

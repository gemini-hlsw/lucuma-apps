// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import lucuma.schemas.ObservationDB
import lucuma.schemas.model.ObservingMode

object ObservingModeSubquery
    extends GraphQLSubquery.Typed[ObservationDB, ObservingMode]("ObservingMode"):
  override val subquery: String = s"""
        {
          gmosNorthLongSlit {
            initialGrating
            initialFilter
            initialFpu
            initialCentralWavelength $WavelengthSubquery
            grating
            filter
            fpu
            centralWavelength $WavelengthSubquery
            defaultXBin
            explicitXBin
            defaultYBin
            explicitYBin
            defaultAmpReadMode
            explicitAmpReadMode
            defaultAmpGain
            explicitAmpGain
            defaultRoi
            explicitRoi
            defaultWavelengthDithers $WavelengthDitherSubquery
            explicitWavelengthDithers $WavelengthDitherSubquery
            defaultOffsets $AngleSubquery
            explicitOffsets $AngleSubquery
            exposureTimeMode $ExposureTimeModeSubquery
            acquisition {
              defaultFilter
              explicitFilter
              defaultRoi
              explicitRoi
              exposureTimeMode $ExposureTimeModeSubquery
            }
          }
          gmosSouthLongSlit {
            initialGrating
            initialFilter
            initialFpu
            initialCentralWavelength $WavelengthSubquery
            grating
            filter
            fpu
            centralWavelength $WavelengthSubquery
            defaultXBin
            explicitXBin
            defaultYBin
            explicitYBin
            defaultAmpReadMode
            explicitAmpReadMode
            defaultAmpGain
            explicitAmpGain
            defaultRoi
            explicitRoi
            defaultWavelengthDithers $WavelengthDitherSubquery
            explicitWavelengthDithers $WavelengthDitherSubquery
            defaultOffsets $AngleSubquery
            explicitOffsets $AngleSubquery
            exposureTimeMode $ExposureTimeModeSubquery
            acquisition {
              defaultFilter
              explicitFilter
              defaultRoi
              explicitRoi
              exposureTimeMode $ExposureTimeModeSubquery
            }
          }
          gmosNorthImaging {
            variant {
              variantType
              grouped {
                order
                offsets $TelescopeConfigGeneratorSubquery
                skyCount
                skyOffsets $TelescopeConfigGeneratorSubquery
              }
              interleaved {
                offsets $TelescopeConfigGeneratorSubquery
                skyCount
                skyOffsets $TelescopeConfigGeneratorSubquery
              }
              preImaging { 
                offset1 $OffsetSubquery
                offset2 $OffsetSubquery
                offset3 $OffsetSubquery
                offset4 $OffsetSubquery
              }
            }
            initialFilters {
              filter
              exposureTimeMode $ExposureTimeModeSubquery
            }
            filters {
              filter
              exposureTimeMode $ExposureTimeModeSubquery
            }
            defaultBin
            explicitBin
            defaultAmpReadMode
            explicitAmpReadMode
            defaultAmpGain
            explicitAmpGain
            defaultRoi
            explicitRoi
          }
          gmosSouthImaging {
            variant {
              variantType
              grouped {
                order
                offsets $TelescopeConfigGeneratorSubquery
                skyCount
                skyOffsets $TelescopeConfigGeneratorSubquery
              }
              interleaved {
                offsets $TelescopeConfigGeneratorSubquery
                skyCount
                skyOffsets $TelescopeConfigGeneratorSubquery
              }
              preImaging { 
                offset1 $OffsetSubquery
                offset2 $OffsetSubquery
                offset3 $OffsetSubquery
                offset4 $OffsetSubquery
              }
            }          
            initialFilters {
              filter
              exposureTimeMode $ExposureTimeModeSubquery
            }
            filters {
              filter
              exposureTimeMode $ExposureTimeModeSubquery
            }
            defaultBin
            explicitBin
            defaultAmpReadMode
            explicitAmpReadMode
            defaultAmpGain
            explicitAmpGain
            defaultRoi
            explicitRoi
          }
          flamingos2LongSlit {
            initialDisperser
            initialFilter
            initialFpu
            disperser
            filter
            fpu
            explicitReadMode
            explicitReads
            defaultDecker
            explicitDecker
            defaultReadoutMode
            explicitReadoutMode
            defaultOffsets $OffsetSubquery
            explicitOffsets $OffsetSubquery
            exposureTimeMode $ExposureTimeModeSubquery
            acquisition {
              exposureTimeMode $ExposureTimeModeSubquery
            }
          }
        }
      """

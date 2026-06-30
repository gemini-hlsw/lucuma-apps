// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import lucuma.schemas.ObservationDB
import lucuma.schemas.model.ObservingMode

@GraphQLType("ObservingMode")
object ObservingModeSubquery extends GraphQLSubquery.Typed[ObservationDB, ObservingMode]:
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
            variant $ImagingVariantSubquery
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
            variant $ImagingVariantSubquery
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
          flamingos2Imaging {
            initialFilters {
              filter
              exposureTimeMode $ExposureTimeModeSubquery
            }
            filters {
              filter
              exposureTimeMode $ExposureTimeModeSubquery
            }
            defaultReadMode
            explicitReadMode
            defaultReads
            explicitReads
            defaultDecker
            explicitDecker
            defaultReadoutMode
            explicitReadoutMode
            variant $ImagingVariantSubquery
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
              defaultFilter
              explicitFilter
              exposureTimeMode $ExposureTimeModeSubquery
            }
          }
          igrins2LongSlit {
            exposureTimeMode $ExposureTimeModeSubquery
            defaultOffsetMode
            explicitOffsetMode
            defaultSaveSVCImages
            explicitSaveSVCImages
            defaultOffsets $OffsetSubquery
            explicitOffsets $OffsetSubquery
          }
          gnirsSpectroscopy {
            initialGrating
            initialFilter
            initialFpuSlit
            initialFpuIfu
            initialPrism
            initialCamera
            grating
            filter
            fpuSlit
            fpuIfu
            prism
            camera
            centralWavelength $WavelengthSubquery
            initialCentralWavelength $WavelengthSubquery
            defaultDecker
            explicitDecker
            explicitReadMode
            defaultWellDepth
            explicitWellDepth
            explicitFocusMotorSteps
            defaultTelescopeConfigs $SlitTelescopeConfigsSubquery
            explicitTelescopeConfigs $SlitTelescopeConfigsSubquery
            exposureTimeMode $ExposureTimeModeSubquery
            coadds
            acquisition {
              explicitAcquisitionType
              explicitFilter
              skyOffset $OffsetSubquery
              exposureTimeMode $ExposureTimeModeSubquery
              coadds
            }
          }
          ghostIfu {
            stepCount
            resolutionMode
            red {
              exposureTimeMode $ExposureTimeModeSubquery
              defaultBinning
              explicitBinning
              defaultReadMode
              explicitReadMode
            }
            blue {
              exposureTimeMode $ExposureTimeModeSubquery
              defaultBinning
              explicitBinning
              defaultReadMode
              explicitReadMode
            }
            defaultIfu1Agitator
            explicitIfu1Agitator
            defaultIfu2Agitator
            explicitIfu2Agitator
            skyPosition $CoordinatesSubquery
          }
          visitor {
            mode
            centralWavelength $WavelengthSubquery
            agsDiameter $AngleSubquery
            name
            totalRequestTime $TimeSpanSubquery
          }
        }
      """

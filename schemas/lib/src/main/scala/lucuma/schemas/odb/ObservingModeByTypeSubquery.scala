// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import lucuma.schemas.ObservationDB
import lucuma.schemas.model.ObservingMode

// Each instrument-mode view is gated behind an `@include(if:)` directive, so a caller selects
// only the mode it needs. `mode` is always selected: a caller removing the observing mode turns
// off every flag, and an empty selection set on the non-leaf `observingMode` field is a server
// error.
@GraphQLType("ObservingMode")
object ObservingModeByTypeSubquery extends GraphQLSubquery.Typed[ObservationDB, ObservingMode]:
  type VariableDefs =
    "($includeGmosNorthLongSlit: Boolean!, $includeGmosSouthLongSlit: Boolean!, $includeGmosNorthImaging: Boolean!, $includeGmosSouthImaging: Boolean!, $includeGmosNorthMos: Boolean!, $includeGmosSouthMos: Boolean!, $includeFlamingos2LongSlit: Boolean!, $includeFlamingos2Mos: Boolean!, $includeFlamingos2Imaging: Boolean!, $includeGnirsSpectroscopy: Boolean!, $includeGnirsImaging: Boolean!, $includeIgrins2LongSlit: Boolean!, $includeGhostIfu: Boolean!, $includeVisitor: Boolean!, $includeExchange: Boolean!)"

  override val subquery = gql"""
        {
          mode
          gmosNorthLongSlit @include(if: $$includeGmosNorthLongSlit) {
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
          gmosSouthLongSlit @include(if: $$includeGmosSouthLongSlit) {
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
          gmosNorthMos @include(if: $$includeGmosNorthMos) {
            initialGrating
            initialFilter
            initialSlitWidth
            initialCentralWavelength $WavelengthSubquery
            grating
            filter
            customMask {
              attachmentId
              slitWidth
            }
            centralWavelength $WavelengthSubquery
            acquisitionType
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
              exposureTimeMode $ExposureTimeModeSubquery
            }
          }
          gmosSouthMos @include(if: $$includeGmosSouthMos) {
            initialGrating
            initialFilter
            initialSlitWidth
            initialCentralWavelength $WavelengthSubquery
            grating
            filter
            customMask {
              attachmentId
              slitWidth
            }
            centralWavelength $WavelengthSubquery
            acquisitionType
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
              exposureTimeMode $ExposureTimeModeSubquery
            }
          }
          gmosNorthImaging @include(if: $$includeGmosNorthImaging) {
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
          gmosSouthImaging @include(if: $$includeGmosSouthImaging) {
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
          flamingos2Imaging @include(if: $$includeFlamingos2Imaging) {
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
          flamingos2LongSlit @include(if: $$includeFlamingos2LongSlit) {
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
            defaultTelescopeConfigs $SlitTelescopeConfigsSubquery
            explicitTelescopeConfigs $SlitTelescopeConfigsSubquery
            exposureTimeMode $ExposureTimeModeSubquery
            acquisition {
              defaultFilter
              explicitFilter
              exposureTimeMode $ExposureTimeModeSubquery
            }
          }
          flamingos2Mos @include(if: $$includeFlamingos2Mos) {
            initialDisperser
            initialFilter
            initialSlitWidth
            disperser
            filter
            customMask {
              attachmentId
              slitWidth
            }
            explicitReadMode
            explicitReads
            defaultDecker
            explicitDecker
            defaultReadoutMode
            explicitReadoutMode
            defaultTelescopeConfigs $SlitTelescopeConfigsSubquery
            explicitTelescopeConfigs $SlitTelescopeConfigsSubquery
            exposureTimeMode $ExposureTimeModeSubquery
            acquisition {
              defaultFilter
              explicitFilter
              exposureTimeMode $ExposureTimeModeSubquery
            }
          }
          igrins2LongSlit @include(if: $$includeIgrins2LongSlit) {
            exposureTimeMode $ExposureTimeModeSubquery
            svc {
              defaultExposure $TimeSpanSubquery
              explicitExposure $TimeSpanSubquery
              defaultTelescopeConfigs $TelescopeConfigSubquery
              explicitTelescopeConfigs $TelescopeConfigSubquery
            }
            defaultTelescopeConfigs $SlitTelescopeConfigsSubquery
            explicitTelescopeConfigs $SlitTelescopeConfigsSubquery
          }
          gnirsImaging @include(if: $$includeGnirsImaging) {
            initialFilters {
              filter
              exposureTimeMode $ExposureTimeModeSubquery
              coadds
            }
            filters {
              filter
              exposureTimeMode $ExposureTimeModeSubquery
              coadds
            }
            camera
            explicitReadMode
            defaultWellDepth
            explicitWellDepth
            variant $ImagingVariantSubquery
            acquisition {
              explicitAcquisitionType
              explicitFilter
              skyOffset $OffsetSubquery
              exposureTimeMode $ExposureTimeModeSubquery
              explicitExposureTimeMode $ExposureTimeModeSubquery
              coadds
            }
          }
          gnirsSpectroscopy @include(if: $$includeGnirsSpectroscopy) {
            initialGrating
            initialFilter
            initialPrism
            initialCamera
            grating
            filter
            slit {
              fpu
              initialFpu
              defaultTelescopeConfigs $SlitTelescopeConfigsSubquery
              explicitTelescopeConfigs $SlitTelescopeConfigsSubquery
            }
            ifu {
              fpu
              initialFpu
              telescopeConfigs $TelescopeConfigSubquery
            }
            prism
            camera
            centralWavelengths {
              centralWavelength $WavelengthSubquery
              exposureTimeMode $ExposureTimeModeSubquery
              coadds
            }
            initialCentralWavelengths {
              centralWavelength $WavelengthSubquery
              exposureTimeMode $ExposureTimeModeSubquery
              coadds
            }
            defaultDecker
            explicitDecker
            explicitReadMode
            defaultWellDepth
            explicitWellDepth
            explicitFocusMotorSteps
            acquisition {
              explicitAcquisitionType
              explicitFilter
              skyOffset $OffsetSubquery
              exposureTimeMode $ExposureTimeModeSubquery
              explicitExposureTimeMode $ExposureTimeModeSubquery
              coadds
            }
          }
          ghostIfu @include(if: $$includeGhostIfu) {
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
          visitor @include(if: $$includeVisitor) {
            mode
            centralWavelength $WavelengthSubquery
            agsDiameter $AngleSubquery
            scienceFovDiameter $AngleSubquery
            name
            totalRequestTime $TimeSpanSubquery
          }
          exchange @include(if: $$includeExchange) {
            keckInstrument
            subaruInstrument
            totalRequestTime $TimeSpanSubquery
          }
        }
      """

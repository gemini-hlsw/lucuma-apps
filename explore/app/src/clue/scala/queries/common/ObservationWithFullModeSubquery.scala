// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import clue.annotation.GraphQLType
import explore.model.Observation
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.*

// Mirror of ObservationSubquery that returns the full ObservingMode instead
// of just BasicConfiguration. Used by per-observation paths (mutations, the
// edit subscription) where the caller needs the hydrated mode directly,
// without a follow-up detail query.
@GraphQL
@GraphQLType("Observation")
object ObservationWithFullModeSubquery
    extends GraphQLSubquery.Typed[ObservationDB, Observation]:

  override val subquery: String = s"""
        {
          id
          title
          subtitle
          observationTime
          observationDuration $TimeSpanSubquery
          posAngleConstraint $PosAngleConstraintSubquery
          targetEnvironment {
            asterism { id }
            useBlindOffset
            blindOffsetTarget { id }
            blindOffsetType
          }
          constraintSet $ConstraintSetSubquery
          schedulingConstraints {
            isSplittable
            timingWindows $TimingWindowSubquery
          }
          attachments { id }
          scienceRequirements {
            exposureTimeMode $ExposureTimeModeSubquery
            spectroscopy {
              wavelength $WavelengthSubquery
              resolution
              wavelengthCoverage $WavelengthDeltaSubquery
              focalPlane
              focalPlaneAngle $AngleSubquery
              capability
            }
            imaging {
              minimumFov $AngleSubquery
              narrowFilters
              broadFilters
              combinedFilters
            }
          }
          observingMode $ObservingModeSubquery
          observerNotes
          calibrationRole
          scienceBand
          configuration $ConfigurationSubquery
          configurationRequests { id }
          workflow $CalculatedObservationWorkflowSubquery
          groupId
          groupIndex
          reference { label }
          execution $ExecutionSubquery
        }
      """

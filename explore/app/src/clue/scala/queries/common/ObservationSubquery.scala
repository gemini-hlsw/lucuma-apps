// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import clue.annotation.GraphQL
import explore.model.Observation
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.*

@GraphQL
@GraphQLType("Observation")
object ObservationSubquery extends GraphQLSubquery.Typed[ObservationDB, Observation]:

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
            explicitBase {
              ra $RASubquery
              dec $DecSubquery
            }
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
          observingMode $BasicConfigurationSubquery
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

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB
import lucuma.core.math.Coordinates
import lucuma.core.model
import lucuma.schemas.model.TargetWithId
import lucuma.schemas.odb.*
// gql: import io.circe.refined.*
// gql: import lucuma.schemas.decoders.given
// gql: import lucuma.odb.json.all.query.given

object ObsQueriesGql:

  // Query observations with targets.
  @GraphQL
  trait ObsQuery extends GraphQLOperation[ObservationDB]:
    val document = s"""
      query($$obsId: ObservationId!, $$skipTargets: Boolean! = false) {
        observation(observationId: $$obsId) {
          id
          title
          observationTime
          program {
            id
            name
            goa { proprietaryMonths }
          }
          targetEnvironment @skip(if: $$skipTargets) {
            asterism $TargetWithIdSubquery
            firstScienceTarget {
              targetId: id
              targetName: name
            }
            basePosition {
              type
              name
              sidereal $SiderealSubquery
              nonsidereal {
                des
                keyType
                key
              }
              coordinates $CoordinatesSubquery
            }
            guideEnvironment {
              guideTargets { probe }
            }
            explicitBase $CoordinatesSubquery
          }
          constraintSet $ConstraintSetSubquery
          timingWindows $TimingWindowSubquery
        }

        executionConfig(observationId: $$obsId, futureLimit: 100) $ExecutionConfigSubquery
      }
    """

    object Data:
      object Observation:
        type ConstraintSet = model.ConstraintSet
        type TimingWindows = model.TimingWindow
        object TargetEnvironment:
          type Asterism     = TargetWithId
          type ExplicitBase = Coordinates

  // Lightweight query to determine the skipTargets parameter in the query above.
  @GraphQL
  trait ObsCalibrationRoleQuery extends GraphQLOperation[ObservationDB]:
    val document = """
      query($obsId: ObservationId!) {
        observation(observationId: $obsId) {
          calibrationRole
        }
      }
      """

  @GraphQL
  trait ResetAcquisitionMutation extends GraphQLOperation[ObservationDB]:
    val document = """
      mutation($obsId: ObservationId!) {
        resetAcquisition(input: { observationId: $obsId } ) {
          observation { id }
        }
      }
      """

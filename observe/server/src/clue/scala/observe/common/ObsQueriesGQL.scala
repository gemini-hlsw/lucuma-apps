// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
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
    val document = gql"""
      query($$obsId: ObservationId!, $$skipTargets: Boolean!) {
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
          schedulingConstraints {
            timingWindows $TimingWindowSubquery
          }
          attachments {
            id
            mask { name }
          }
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

  // The execution config for one instrument. Selecting every instrument at once, as
  // ExecutionConfigSubquery does, makes the document ~39 KB; one of these is a fifth of the size.
  // Same shape for all of them so OdbProxy can dispatch by instrument.
  @GraphQL
  trait GmosNorthExecutionQuery extends GraphQLOperation[ObservationDB]:
    val document = gql"""
      query($$obsId: ObservationId!, $$futureLimit: NonNegInt!) {
        executionConfig(observationId: $$obsId, futureLimit: $$futureLimit) {
          gmosNorth $GmosNorthExecutionConfigSubquery
        }
      }
    """

  @GraphQL
  trait GmosSouthExecutionQuery extends GraphQLOperation[ObservationDB]:
    val document = gql"""
      query($$obsId: ObservationId!, $$futureLimit: NonNegInt!) {
        executionConfig(observationId: $$obsId, futureLimit: $$futureLimit) {
          gmosSouth $GmosSouthExecutionConfigSubquery
        }
      }
    """

  @GraphQL
  trait Flamingos2ExecutionQuery extends GraphQLOperation[ObservationDB]:
    val document = gql"""
      query($$obsId: ObservationId!, $$futureLimit: NonNegInt!) {
        executionConfig(observationId: $$obsId, futureLimit: $$futureLimit) {
          flamingos2 $Flamingos2ExecutionConfigSubquery
        }
      }
    """

  @GraphQL
  trait Igrins2ExecutionQuery extends GraphQLOperation[ObservationDB]:
    val document = gql"""
      query($$obsId: ObservationId!, $$futureLimit: NonNegInt!) {
        executionConfig(observationId: $$obsId, futureLimit: $$futureLimit) {
          igrins2 $Igrins2ExecutionConfigSubquery
        }
      }
    """

  @GraphQL
  trait GnirsExecutionQuery extends GraphQLOperation[ObservationDB]:
    val document = gql"""
      query($$obsId: ObservationId!, $$futureLimit: NonNegInt!) {
        executionConfig(observationId: $$obsId, futureLimit: $$futureLimit) {
          gnirs $GnirsExecutionConfigSubquery
        }
      }
    """

  @GraphQL
  trait GhostExecutionQuery extends GraphQLOperation[ObservationDB]:
    val document = gql"""
      query($$obsId: ObservationId!, $$futureLimit: NonNegInt!) {
        executionConfig(observationId: $$obsId, futureLimit: $$futureLimit) {
          ghost $GhostExecutionConfigSubquery
        }
      }
    """

  // Lightweight query to determine the skipTargets parameter in the query above.
  @GraphQL
  trait ObsCalibrationRoleQuery extends GraphQLOperation[ObservationDB]:
    val document = gql"""
      query($$obsId: ObservationId!) {
        observation(observationId: $$obsId) {
          calibrationRole
        }
      }
      """

  @GraphQL
  trait ResetAcquisitionMutation extends GraphQLOperation[ObservationDB]:
    val document = gql"""
      mutation($$obsId: ObservationId!) {
        resetAcquisition(input: { observationId: $$obsId } ) {
          observation { id }
        }
      }
      """

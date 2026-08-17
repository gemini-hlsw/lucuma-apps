// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.*
// gql: import lucuma.odb.json.configurationrequest.query.given
// gql: import lucuma.odb.json.sequence.given
// gql: import lucuma.schemas.decoders.given
// gql: import io.circe.refined.given

object ObsQueriesGQL:
  @GraphQL
  trait ProgramCreateObservation extends GraphQLOperation[ObservationDB]:
    val document = gql"""
      mutation($$createObservation: CreateObservationInput!) {
        createObservation(input: $$createObservation) {
          observation $ObservationSubquery
        }
      }
    """

  @GraphQL
  trait ObservationEditSubscription extends GraphQLOperation[ObservationDB]:
    // We need to include the `value {id}` to avoid a bug in grackle.
    val document = gql"""
      subscription($$input: ObservationEditInput!) {
        observationEdit(input: $$input) {
          observationId
        }
      }
    """

  @GraphQL
  trait UpdateObservationMutation extends GraphQLOperation[ObservationDB]:
    val document = gql"""
      mutation ($$input: UpdateObservationsInput!){
        updateObservations(input: $$input) {
          observations { id }
        }
      }
    """

  @GraphQL
  trait UpdateObservationTimesMutation extends GraphQLOperation[ObservationDB]:
    val document = gql"""
      mutation ($$input: UpdateObservationsTimesInput!){
        updateObservationsTimes(input: $$input) {
          observations { id }
        }
      }
    """

  // The response selects only the mode-view matching the mode being set.
  @GraphQL
  trait UpdateConfigurationMutation extends GraphQLOperation[ObservationDB]:
    val document = gql"""
      mutation (
        $$input: UpdateObservationsInput!,
        $$includeGmosNorthLongSlit: Boolean!,
        $$includeGmosSouthLongSlit: Boolean!,
        $$includeGmosNorthImaging: Boolean!,
        $$includeGmosSouthImaging: Boolean!,
        $$includeGmosNorthMos: Boolean!,
        $$includeGmosSouthMos: Boolean!,
        $$includeFlamingos2Imaging: Boolean!,
        $$includeFlamingos2LongSlit: Boolean!,
        $$includeIgrins2LongSlit: Boolean!,
        $$includeGnirsImaging: Boolean!,
        $$includeGnirsSpectroscopy: Boolean!,
        $$includeGhostIfu: Boolean!,
        $$includeVisitor: Boolean!,
        $$includeExchange: Boolean!
      ){
        updateObservations(input: $$input) {
          observations {
            observingMode $ObservingModeByTypeSubquery
          }
        }
      }
    """

  @GraphQL
  trait SetObservationWorkflowStateMutation extends GraphQLOperation[ObservationDB]:
    val document = gql"""
      mutation ($$input: SetObservationWorkflowStateInput!){
        setObservationWorkflowState(input: $$input) {
          state
        }
      }
    """

  @GraphQL
  trait CloneObservationMutation extends GraphQLOperation[ObservationDB]:
    val document = gql"""
      mutation ($$input: CloneObservationInput!){
        cloneObservation(input: $$input) {
          newObservation $ObservationSubquery
        }
      }
    """

  @GraphQL
  trait CreateConfigurationRequestMutation extends GraphQLOperation[ObservationDB]:
    val document = gql"""
      mutation($$input: CreateConfigurationRequestInput!) {
        createConfigurationRequest(input: $$input)
          $ConfigurationRequestSubquery
      }
    """

  @GraphQL
  trait ProgramObservationsDelta extends GraphQLOperation[ObservationDB]:
    // The full observe mode is hydrated separately (initial load + mutations),
    val document = gql"""
      subscription($$input: ObservationEditInput!) {
        observationEdit(input: $$input) {
          observationId
          value $ObservationSubquery
          meta:value { existence }
          editType
        }
      }
    """

  @GraphQL
  trait ObsCalcSubscription extends GraphQLOperation[ObservationDB]:
    val document = gql"""
      subscription($$input: ObscalcUpdateInput!) {
        obscalcUpdate(input: $$input) {
          observationId
          oldCalculationState
          newCalculationState
          editType
          value {
            groupId
            execution {
              digest $CalculatedDigestSubquery
            }
            workflow $CalculatedObservationWorkflowSubquery
          }
        }
      }
    """

  @GraphQL
  trait ResolveObsReference extends GraphQLOperation[ObservationDB]:
    val document = gql"""
      query($$input: ObservationReferenceLabel) {
        observation(observationReference: $$input) {
          id
          program { id }
        }
      }
    """

  // some of the components of ObservationSubquery are quite expensive to read in bulk
  // We can split the query into two parts, one for the bulk of the observation data and one for
  // observation specific parts that can be delayed until the obs is selected
  //
  // One such element is guide target name which tracing shows as very expensive
  @GraphQL
  trait ObservationLoadedElements extends GraphQLOperation[ObservationDB]:
    val document = gql"""
      query($$obsId: ObservationId!) {
        observation(observationId: $$obsId) {
          targetEnvironment {
            guideTargetName
          }
        }
      }
    """

  @GraphQL
  trait SetBlindOffsetMutation extends GraphQLOperation[ObservationDB]:
    val document = gql"""
      mutation(
        $$where: WhereObservation!,
        $$useBlindOffset: Boolean!,
        $$target: TargetPropertiesInput,
        $$blindType: BlindOffsetType!
      ) {
        updateObservations(input: {
          WHERE: $$where
          SET: {
            targetEnvironment: {
              useBlindOffset: $$useBlindOffset
              blindOffsetTarget: $$target
              blindOffsetType: $$blindType
            }
          }
        }) {
          observations {
            targetEnvironment {
              blindOffsetTarget {
                id
              }
            }
          }
        }
      }
    """

  @GraphQL
  trait StepEventSubscription extends GraphQLOperation[ObservationDB]:
    val document = gql"""
      subscription($$obsId: ObservationId!) {
        executionEventAdded(input: { observationId: $$obsId, eventType: { EQ: STEP } }) {
          value {
            ... on StepEvent {
              stepStage
            }
          }
        }
      }
    """

  @GraphQL
  trait DatasetEditSubscription extends GraphQLOperation[ObservationDB]:
    val document = gql"""
      subscription($$obsId: ObservationId!) {
        datasetEdit(input: { observationId: $$obsId }) {
          value { id }
        }
      }
    """

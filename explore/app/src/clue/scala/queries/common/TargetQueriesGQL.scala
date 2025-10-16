// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.*
// gql: import lucuma.schemas.decoders.given

object TargetQueriesGQL:

  @GraphQL
  trait CreateTargetMutation extends GraphQLOperation[ObservationDB]:
    val document = """
      mutation($input: CreateTargetInput!) {
        createTarget(input: $input) {
          target {
            id
          }
        }
      }
    """

  @GraphQL
  trait UpdateTargetsMutation extends GraphQLOperation[ObservationDB]:
    val document = """
      mutation($input: UpdateTargetsInput!) {
        updateTargets(input: $input) {
          targets {
            id
          }
        }
      }
    """

  @GraphQL
  trait CloneTargetMutation extends GraphQLOperation[ObservationDB]:
    val document = s"""
      mutation($$input: CloneTargetInput!) {
        cloneTarget(input: $$input) {
          newTarget $TargetWithIdSubquery
        }
      }
    """

  @GraphQL
  trait SetGuideTargetName extends GraphQLOperation[ObservationDB]:
    val document = s"""
      mutation($$input: SetGuideTargetNameInput!) {
        setGuideTargetName(input: $$input) {
          observation {
            id
          }
        }
      }
    """

  @GraphQL
  trait TargetEditSubscription extends GraphQLOperation[ObservationDB]:
    val document = """
      subscription($targetId: TargetId!) {
        targetEdit(input: {targetId: $targetId}) {
          targetId
        }
      }
    """

  @GraphQL
  trait ProgramTargetsDelta extends GraphQLOperation[ObservationDB]:
    val document = s"""
      subscription($$input: TargetEditInput!) {
        targetEdit(input: $$input) {
          targetId
          value $TargetWithIdSubquery
          meta:value {
            existence
          }
        }
      }
    """

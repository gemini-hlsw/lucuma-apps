// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB

// The Archive Duplication Search tile pulls its own data: nothing here is folded into
// `ObservationSubquery`, and there is no subscription, because a Search deliberately emits no
// observation edit events.
object ArchiveDuplicationQueriesGQL:
  @GraphQL
  trait ProgramArchiveDuplications extends GraphQLOperation[ObservationDB]:
    val document = gql"""
      query($$where: WhereObservation!, $$OFFSET: ObservationId) {
        observations(WHERE: $$where, OFFSET: $$OFFSET) {
          matches {
            id
            archiveDuplication $ArchiveDuplicationSubquery
          }
          hasMore
        }
      }
    """

  // One observation's header, for the refetch driven by an obscalc READY transition.
  @GraphQL
  trait ObservationArchiveDuplication extends GraphQLOperation[ObservationDB]:
    val document = gql"""
      query($$obsId: ObservationId!) {
        observation(observationId: $$obsId) {
          archiveDuplication $ArchiveDuplicationSubquery
        }
      }
    """

  @GraphQL
  trait ObservationArchiveMatches extends GraphQLOperation[ObservationDB]:
    val document = gql"""
      query($$obsId: ObservationId!) {
        observation(observationId: $$obsId) {
          archiveDuplication {
            matches $ArchiveMatchSubquery
          }
        }
      }
    """

  @GraphQL
  trait RefreshArchiveDuplication extends GraphQLOperation[ObservationDB]:
    val document = gql"""
      mutation($$input: RefreshArchiveDuplicationInput!) {
        refreshArchiveDuplication(input: $$input) {
          archiveDuplication $ArchiveDuplicationSubquery
        }
      }
    """

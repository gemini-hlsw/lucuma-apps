// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.*
// gql: import lucuma.schemas.decoders.given

object ProgramSummaryQueriesGQL {
  @GraphQL
  trait AllProgramObservations extends GraphQLOperation[ObservationDB] {
    val document = gql"""
      query($$where: WhereObservation!, $$OFFSET_AFTER: ObservationId) {
        observations(WHERE: $$where, OFFSET_AFTER: $$OFFSET_AFTER) {
          matches $ObservationSubquery
          hasMore
        }
      }
    """
  }

  // Paginated query returning only (id, observingMode) for observations matching
  // the WHERE clause.
  // Used by the per-instrument detail phase to hydrate `Observation.observingMode` after the
  // bulk summary has loaded.
  @GraphQL
  trait AllProgramObservationsObservingMode extends GraphQLOperation[ObservationDB] {
    val document = gql"""
      query(
        $$where: WhereObservation!,
        $$OFFSET_AFTER: ObservationId,
        $$includeGmosNorthLongSlit: Boolean!,
        $$includeGmosSouthLongSlit: Boolean!,
        $$includeGmosNorthImaging: Boolean!,
        $$includeGmosSouthImaging: Boolean!,
        $$includeGmosNorthMos: Boolean!,
        $$includeGmosSouthMos: Boolean!,
        $$includeGmosNorthIfu: Boolean!,
        $$includeGmosSouthIfu: Boolean!,
        $$includeFlamingos2Imaging: Boolean!,
        $$includeFlamingos2LongSlit: Boolean!,
        $$includeFlamingos2Mos: Boolean!,
        $$includeIgrins2LongSlit: Boolean!,
        $$includeGnirsImaging: Boolean!,
        $$includeGnirsLongSlit: Boolean!,
        $$includeGnirsIfu: Boolean!,
        $$includeGhostIfu: Boolean!,
        $$includeVisitor: Boolean!,
        $$includeExchange: Boolean!,
        $$includeDaytimeCalibration: Boolean!
      ) {
        observations(WHERE: $$where, OFFSET_AFTER: $$OFFSET_AFTER) {
          matches {
            id
            observingMode $ObservingModeByTypeSubquery
          }
          hasMore
        }
      }
    """
  }

  @GraphQL
  trait AllProgramTargets extends GraphQLOperation[ObservationDB] {
    val document = gql"""
      query($$where: WhereTarget!, $$OFFSET_AFTER: TargetId) {
        targets(WHERE: $$where, OFFSET_AFTER: $$OFFSET_AFTER) {
          matches $TargetWithIdSubquery
          hasMore
        }
      }
    """
  }

  @GraphQL
  trait AllProgramConfigurationRequests extends GraphQLOperation[ObservationDB] {
    val document = gql"""
      query($$pid: ProgramId!, $$OFFSET_AFTER: ConfigurationRequestId) {
        program(programId: $$pid) {
          configurationRequests(OFFSET_AFTER: $$OFFSET_AFTER) {
            matches $ConfigurationRequestSubquery
            hasMore
          }
        }
      }
    """
  }

  @GraphQL
  trait AllProgramAttachments extends GraphQLOperation[ObservationDB] {
    val document = gql"""
      query($$programId: ProgramId!) {
        program(programId: $$programId) {
          attachments $AttachmentSubquery
          proposalSummaryGeneration $ProposalSummaryGenerationSubquery
        }
      }
    """
  }

  @GraphQL
  trait AllPrograms extends GraphQLOperation[ObservationDB] {
    val document = gql"""
      query($$OFFSET_AFTER: ProgramId) {
        programs(OFFSET_AFTER: $$OFFSET_AFTER, includeDeleted: true) {
          matches $ProgramInfoSubquery
          hasMore
        }
      }
    """
  }

  @GraphQL
  trait ProgramDetailsQuery extends GraphQLOperation[ObservationDB] {
    val document = gql"""
      query($$programId: ProgramId!) {
        program(programId: $$programId) $ProgramDetailsSubquery
      }
    """
  }

  @GraphQL
  trait ProgramTimesQuery extends GraphQLOperation[ObservationDB] {
    val document = gql"""
      query($$programId: ProgramId!) {
        program(programId: $$programId) $ProgramTimesSubquery
      }
    """
  }

  @GraphQL
  trait GroupTimeRangeQuery extends GraphQLOperation[ObservationDB] {
    val document = gql"""
      query($$groupId: GroupId!) {
        group(groupId: $$groupId) {
          timeEstimateRange $CalculatedProgramTimeRangeSubquery
          parentId
        }
      }
    """
  }
}

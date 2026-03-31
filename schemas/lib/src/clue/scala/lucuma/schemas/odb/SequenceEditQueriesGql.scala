// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB
// gql: import lucuma.odb.json.sequence.given
// gql: import lucuma.odb.json.gmos.given
// gql: import lucuma.odb.json.flamingos2.given

object SequenceEditQueriesGql:
  @GraphQL
  trait ReplaceGmosNorthSequence extends GraphQLOperation[ObservationDB]:
    val document = s"""
      mutation($$obsId: ObservationId!, $$sequenceType: SequenceType!, $$sequence: [GmosNorthAtomInput!]!) {
        replaceGmosNorthSequence(input: {observationId: $$obsId, sequenceType: $$sequenceType, sequence: $$sequence}) {
          sequence $GmosNorthAtomSubquery
        }
      }
    """

  @GraphQL
  trait ReplaceGmosSouthSequence extends GraphQLOperation[ObservationDB]:
    val document = s"""
      mutation($$obsId: ObservationId!, $$sequenceType: SequenceType!, $$sequence: [GmosSouthAtomInput!]!) {
        replaceGmosSouthSequence(input: {observationId: $$obsId, sequenceType: $$sequenceType, sequence: $$sequence}) {
          sequence $GmosSouthAtomSubquery
        }
      }
    """

  @GraphQL
  trait ReplaceFlamingos2Sequence extends GraphQLOperation[ObservationDB]:
    val document = s"""
      mutation($$obsId: ObservationId!, $$sequenceType: SequenceType!, $$sequence: [Flamingos2AtomInput!]!) {
        replaceFlamingos2Sequence(input: {observationId: $$obsId, sequenceType: $$sequenceType, sequence: $$sequence}) {
          sequence $Flamingos2AtomSubquery
        }
      }
    """

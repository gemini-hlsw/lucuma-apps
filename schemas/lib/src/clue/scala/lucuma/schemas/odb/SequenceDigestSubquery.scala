// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import clue.annotation.GraphQLType
import lucuma.core.model.sequence.SequenceDigest
import lucuma.core.model.sequence.StepDigest
import lucuma.odb.json.sequence.given
import lucuma.schemas.ObservationDB

@GraphQL
@GraphQLType("SequenceDigest")
object SequenceDigestSubquery extends GraphQLSubquery.Typed[ObservationDB, SequenceDigest]:
  override val subquery = gql"""
      {
        observeClass
        atomCount
        gcalSets
        timeEstimate {
          program $TimeSpanSubquery
          nonCharged $TimeSpanSubquery
          total $TimeSpanSubquery
        }
        telescopeConfigs {
          offset $OffsetSubquery
          guiding
        }
        steps {
          bias $StepDigestSubquery
          dark $StepDigestSubquery
          arc $StepDigestSubquery
          flat $StepDigestSubquery
          science $StepDigestSubquery
        }
        executionState
        stepViolations {
          code
          message
        }
      }
  """

@GraphQL
@GraphQLType("StepDigest")
object StepDigestSubquery extends GraphQLSubquery.Typed[ObservationDB, StepDigest]:
  override val subquery = gql"""
      {
        count
        time {
          program $TimeSpanSubquery
          nonCharged $TimeSpanSubquery
          total $TimeSpanSubquery
        }
      }
  """

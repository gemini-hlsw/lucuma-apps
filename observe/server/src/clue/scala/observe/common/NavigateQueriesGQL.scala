// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.common

import clue.GraphQLOperation
import clue.annotation.GraphQL

object NavigateQueriesGQL {

  @GraphQL
  trait ConfigureStepMutation extends GraphQLOperation[NavigateDB] {
    val document = gql"""
      mutation($$config: ConfigureStepInput!) {
        configureStep(config: $$config) {
          result
          msg
        }
      }
      """
  }
}

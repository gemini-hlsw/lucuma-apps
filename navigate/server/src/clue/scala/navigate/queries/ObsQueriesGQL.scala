// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.queries

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB

object ObsQueriesGQL:

  @GraphQL
  trait AddSlewEventMutation extends GraphQLOperation[ObservationDB]:
    val document = """
      mutation($obsId: ObservationId!, $stg: SlewStage!)  {
        addSlewEvent(input: { observationId: $obsId, slewStage: $stg } ) {
          event {
            id
          }
        }
      }
      """

  @GraphQL
  trait ActiveNonsiderealTargetsQuery extends GraphQLOperation[ObservationDB] {
    val document =
      """
      query($site: Site!, $startDate: Date!, $endDate: Date!) {
        observations(
          WHERE: {
            site: { EQ: $site },
            program: {
              AND: [
                { activeStart: { LTE: $endDate } },
                { activeEnd: { GTE: $startDate } }
              ]
            },
            reference: { IS_NULL: false },
            workflow: {
              workflowState: {
                IN: [READY, ONGOING, COMPLETED]
              }
            }
          }
        ) {
          matches {
            id
            targetEnvironment {
              firstScienceTarget {
                nonsidereal {
                  des
                  keyType
                  key
                }
              }
              guideEnvironment {
                guideTargets {
                  nonsidereal {
                    des
                    keyType
                    key
                  }
                }
              }
              blindOffsetTarget {
                nonsidereal {
                  des
                  keyType
                  key
                }
              }
            }
          }
        }
      }
    """

    object Data {
      object Observations {
        object Matches {
          object TargetEnvironment {
            object FirstScienceTarget {
              type Nonsidereal = navigate.model.OdbNonsidereal
            }
            object GuideEnvironment   {
              object GuideTargets {
                type Nonsidereal = navigate.model.OdbNonsidereal
              }
            }
            object BlindOffsetTarget  {
              type Nonsidereal = navigate.model.OdbNonsidereal
            }
          }
        }
      }
    }
  }

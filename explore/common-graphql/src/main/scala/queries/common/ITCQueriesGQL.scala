// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import eu.timepit.refined.types.string
import explore.model.itc.ItcCcd
import explore.model.itc.remote.ItcChartGroupRemote
import queries.schemas.ITC
// gql: import io.circe.refined.*

object ITCQueriesGQL {

  @GraphQL
  trait SpectroscopyITCQuery extends GraphQLOperation[ITC] {
    val document =
      """
      query($input: SpectroscopyModeInput) {
        spectroscopy(input: $input) {
          serverVersion
          results {
            mode {
              instrument
            }
            itc {
              ... on ItcSuccess {
                exposures
                exposureTime {
                  microseconds
                }
              }
              ... on ItcError {
                msg
              }
            }
          }
        }
      }
    """

    object Data {
      object Spectroscopy {
        type ServerVersion = string.NonEmptyString
      }
    }
  }

  @GraphQL
  trait SpectroscopyGraphITCQuery extends GraphQLOperation[ITC] {
    val document =
      """
      query($input: SpectroscopyGraphModeInput) {
        spectroscopyGraph(input: $input) {
          serverVersion
          ccds {
            singleSNRatio
            totalSNRatio
            peakPixelFlux
            ampGain
            maxTotalSNRatio
            wavelengthForMaxTotalSNRatio {
              picometers
            }
          }
          charts {
            chartType
            series {
              title
              seriesType
              dataY
              xAxis {
                start
                end
                count
              }
              yAxis {
                min
                max
              }
            }
          }
        }
      }
    """

    object Data {
      object SpectroscopyGraph {
        type ServerVersion = string.NonEmptyString
        type Ccds          = ItcCcd
        type Charts        = ItcChartGroupRemote
      }
    }
  }

}

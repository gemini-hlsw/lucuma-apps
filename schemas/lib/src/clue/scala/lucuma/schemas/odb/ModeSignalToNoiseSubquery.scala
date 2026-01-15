// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB
import lucuma.schemas.model.ModeSignalToNoise
import lucuma.schemas.decoders.given

@GraphQL
object ModeSignalToNoiseSubquery
    extends GraphQLSubquery.Typed[ObservationDB, ModeSignalToNoise]("Itc"):
  override val subquery: String = s"""
    {
      itcType
      ... on ItcSpectroscopy {
          acquisition {
            selected {
              signalToNoiseAt $SignalToNoiseAtSubquery
            }
          }
          spectroscopyScience {
            selected {
              signalToNoiseAt $SignalToNoiseAtSubquery
            }
          }
        }
        ... on ItcGmosNorthImaging {
          gmosNorthImagingScience {
            filter
            results {
              selected {
                signalToNoiseAt $SignalToNoiseAtSubquery
              }
            }
          }
        }
        ... on ItcGmosSouthImaging {
          gmosSouthImagingScience {
            filter
            results {
              selected {
                signalToNoiseAt $SignalToNoiseAtSubquery
              }
            }
          }
        }
      }
  """

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB
import lucuma.schemas.decoders.given
import lucuma.schemas.model.ModeSignalToNoise

@GraphQL
@GraphQLType("Itc")
object ModeSignalToNoiseSubquery extends GraphQLSubquery.Typed[ObservationDB, ModeSignalToNoise]:
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
        ... on ItcFlamingos2Imaging {
          flamingos2ImagingScience {
            filter
            results {
              selected {
                signalToNoiseAt $SignalToNoiseAtSubquery
              }
            }
          }
        }
        ... on ItcIgrins2Spectroscopy {
          spectroscopyScience {
            selected {
              signalToNoiseAt $SignalToNoiseAtSubquery
            }
          }
        }
      }
  """

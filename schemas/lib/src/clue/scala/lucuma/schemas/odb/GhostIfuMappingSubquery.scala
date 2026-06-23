// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import clue.annotation.GraphQL
import lucuma.core.model.sequence.ghost.GhostIfuMapping
import lucuma.odb.json.ghost.decoder.given
import lucuma.schemas.ObservationDB

@GraphQL
@GraphQLType("GhostIfuMapping")
object GhostIfuMappingSubquery
    extends GraphQLSubquery.Typed[ObservationDB, GhostIfuMapping]:
  override val subquery: String = """
    {
      mappingType
      singleTarget {
        ifu1
      }
      targetPlusSky {
        ifu1
        ifu2 {
          ra { microseconds }
          dec { microarcseconds }
        }
      }
      skyPlusTarget {
        ifu1 {
          ra { microseconds }
          dec { microarcseconds }
        }
        ifu2
      }
      dualTarget {
        ifu1
        ifu2
      }
    }
  """

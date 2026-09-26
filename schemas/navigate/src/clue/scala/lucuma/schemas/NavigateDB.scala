// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas

import clue.annotation.GraphQLSchema
import lucuma.core.enums
import lucuma.core.model.*

// gql: import io.circe.refined.*
// gql: import lucuma.odb.json.all.query.given

/**
 * Navigate's GraphQL schema, for Navigate's clients. `NavigateDB.graphql` is `navigate.graphql`
 * with its imports from the ODB schema resolved. It is checked against the original by Navigate's
 * `NavigateSchemaSuite` and regenerated with `sbt navigateSchemaGenerate`.
 */
@GraphQLSchema
trait NavigateDB {

  object Scalars {
    // Ids
    type ObservationId  = Observation.Id
    type TargetId       = Target.Id
    // Basic types
    type BigDecimal     = scala.BigDecimal
    type Long           = scala.Long
    // Formatted strings
    type DmsString      = String
    type EpochString    = String
    type HmsString      = String
    // Refined
    type NonEmptyString = eu.timepit.refined.types.string.NonEmptyString
    type PosBigDecimal  = eu.timepit.refined.types.numeric.PosBigDecimal
    type PosInt         = eu.timepit.refined.types.numeric.PosInt
    // Core Types
    type IntPercent     = lucuma.core.model.IntPercent
    type Timestamp      = lucuma.core.util.Timestamp
    type Date           = java.time.LocalDate
  }

  object Enums {
    type CatalogName      = enums.CatalogName
    type EphemerisKeyType = enums.EphemerisKeyType
    type GuideProbe       = enums.GuideProbe
    type Instrument       = enums.Instrument
    type Site             = enums.Site
    // Navigate's own enums shared with its clients
    type LightSinkVariant = lucuma.schemas.model.navigate.LightSinkVariant
    type LightSource      = lucuma.schemas.model.navigate.LightSource
    type OperationResult  = lucuma.schemas.model.navigate.OperationResult
  }

  // Inputs Navigate imports from the ODB schema, reused so the ODB input helpers apply to them.
  object Types {
    type OffsetComponentInput = ObservationDB.Types.OffsetComponentInput
    type OffsetInput          = ObservationDB.Types.OffsetInput
    type WavelengthInput      = ObservationDB.Types.WavelengthInput
  }
}

// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import clue.GraphQLSubquery
import clue.annotation.GraphQLType
import io.circe.Decoder
import lucuma.core.enums.ObserveClass
import lucuma.core.model.sequence.StepConfig
import lucuma.odb.json.stepconfig.given
import lucuma.schemas.ObservationDB

// What a step's kind needs, validated against the step type of every instrument.
@GraphQLType(
  "GmosNorthStep",
  "GmosSouthStep",
  "Flamingos2Step",
  "Igrins2Step",
  "GnirsStep",
  "GhostStep"
)
object RemainingStepSubquery extends GraphQLSubquery[ObservationDB]:
  case class Data(stepConfig: StepConfig, observeClass: ObserveClass) derives Decoder

  override val dataDecoder: Decoder[Data] = Decoder[Data]

  override val subquery = gql"""
    {
      stepConfig $StepConfigSubquery
      observeClass
    }
  """

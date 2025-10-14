// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model

import cats.Eq
import io.circe.Decoder
import io.circe.Encoder
import io.circe.JsonObject
import io.circe.syntax.*

case class ObservationProgress(obsId: Observation.Id, stepProgress: StepProgress):
  export stepProgress.*

object ObservationProgress:
  given Eq[ObservationProgress] = Eq.by(x => (x.obsId, x.stepProgress))

  given Encoder.AsObject[ObservationProgress] = Encoder.AsObject.instance: op =>
    JsonObject(
      "obsId"        -> op.obsId.asJson,
      "stepProgress" -> op.stepProgress.asJson
    )

  given Decoder[ObservationProgress] = Decoder.instance: c =>
    for
      obsId        <- c.downField("obsId").as[Observation.Id]
      stepProgress <- c.downField("stepProgress").as[StepProgress]
    yield ObservationProgress(obsId, stepProgress)

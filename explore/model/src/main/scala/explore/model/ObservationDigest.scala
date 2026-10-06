// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import io.circe.Decoder
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.odb.json.sequence.given
import monocle.Focus
import monocle.Lens

/**
 * An execution digest with the ODB's total time estimate, which the lucuma-core digest drops. The
 * total excludes existing calibrations, as they are observations of their own.
 */
case class ObservationDigest(digest: ExecutionDigest, total: ProgramTime) derives Eq:
  export digest.*

object ObservationDigest:
  val digest: Lens[ObservationDigest, ExecutionDigest] = Focus[ObservationDigest](_.digest)

  given Decoder[ObservationDigest] = Decoder.instance: c =>
    for
      d <- c.as[ExecutionDigest]
      t <- c.downField("estimate").get[ProgramTime]("total")
    yield ObservationDigest(d, t)

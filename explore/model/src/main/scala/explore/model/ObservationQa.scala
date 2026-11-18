// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import io.circe.Decoder
import lucuma.core.enums.ObservationQaState
import monocle.Focus
import monocle.Lens

/**
 * The QA verdict for an observation as a whole. `computed` is calculated by the ODB from the
 * observation's science datasets, and is empty while there are none or any is not yet assessed.
 * `stateOverride` is the value staff have assigned, if any, and takes precedence.
 */
final case class ObservationQa(
  computed:      Option[ObservationQaState],
  stateOverride: Option[ObservationQaState]
) derives Eq,
      Decoder:
  val state: Option[ObservationQaState] = stateOverride.orElse(computed)

object ObservationQa:
  val computed: Lens[ObservationQa, Option[ObservationQaState]]      = Focus[ObservationQa](_.computed)
  val stateOverride: Lens[ObservationQa, Option[ObservationQaState]] =
    Focus[ObservationQa](_.stateOverride)

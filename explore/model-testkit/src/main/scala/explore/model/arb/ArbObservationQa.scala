// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.ObservationQa
import lucuma.core.enums.ObservationQaState
import lucuma.core.util.arb.ArbEnumerated.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

trait ArbObservationQa:
  given Arbitrary[ObservationQa] =
    Arbitrary:
      for
        computed      <- arbitrary[Option[ObservationQaState]]
        stateOverride <- arbitrary[Option[ObservationQaState]]
      yield ObservationQa(computed, stateOverride)

  given Cogen[ObservationQa] =
    Cogen[(Option[ObservationQaState], Option[ObservationQaState])].contramap(q =>
      (q.computed, q.stateOverride)
    )

object ArbObservationQa extends ArbObservationQa

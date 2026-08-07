// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.TooActivationCeiling
import lucuma.core.enums.TooActivation
import lucuma.core.util.arb.ArbEnumerated.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

trait ArbTooActivationCeiling:
  given Arbitrary[TooActivationCeiling] = Arbitrary(
    for {
      effective <- arbitrary[TooActivation]
      default   <- arbitrary[TooActivation]
      explicit  <- arbitrary[Option[TooActivation]]
    } yield TooActivationCeiling(effective, default, explicit)
  )

  given Cogen[TooActivationCeiling] = Cogen[
    (TooActivation, TooActivation, Option[TooActivation])
  ].contramap(c => (c.effective, c.default, c.explicit))

object ArbTooActivationCeiling extends ArbTooActivationCeiling

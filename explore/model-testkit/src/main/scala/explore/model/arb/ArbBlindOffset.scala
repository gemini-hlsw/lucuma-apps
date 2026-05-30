// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.BlindOffset
import lucuma.core.model.Target
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbGid.given
import lucuma.schemas.model.enums.BlindOffsetType
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

trait ArbBlindOffset:
  given Arbitrary[BlindOffset] =
    Arbitrary:
      for
        useBlind    <- arbitrary[Boolean]
        blindTarget <- arbitrary[Option[Target.Id]]
        blindType   <- arbitrary[BlindOffsetType]
      yield BlindOffset(useBlind, blindTarget, blindType)

  given Cogen[BlindOffset] =
    Cogen[(Boolean, Option[Target.Id], BlindOffsetType)].contramap(b =>
      (b.useBlindOffset, b.blindOffsetTargetId, b.blindOffsetType)
    )

object ArbBlindOffset extends ArbBlindOffset

// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import eu.timepit.refined.scalacheck.all.*
import explore.model.PartnerSplit
import lucuma.core.enums.Partner
import lucuma.core.model.IntPercent
import lucuma.core.model.ProgramUser
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbGid
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen.*

trait ArbPartnerSplit:
  import ArbEnumerated.given
  import ArbGid.given

  given Arbitrary[PartnerSplit] =
    Arbitrary {
      for {
        partner <- arbitrary[Partner]
        percent <- arbitrary[IntPercent]
        lead    <- arbitrary[Option[ProgramUser.Id]]
      } yield PartnerSplit(partner, percent, lead)
    }

  given Cogen[PartnerSplit] =
    Cogen[
      (Partner, IntPercent, Option[ProgramUser.Id])
    ].contramap(p => (p.partner, p.percent, p.lead))

object ArbPartnerSplit extends ArbPartnerSplit

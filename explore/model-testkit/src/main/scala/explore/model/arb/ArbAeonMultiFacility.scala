// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.AeonMultiFacility
import lucuma.core.enums.Instrument
import lucuma.core.util.arb.ArbEnumerated.given
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbAeonMultiFacility:

  given Arbitrary[AeonMultiFacility] =
    Arbitrary:
      arbitrary[List[Instrument]].map(is => AeonMultiFacility(is.toSet))

  given Cogen[AeonMultiFacility] =
    Cogen[List[String]].contramap(_.requiredInstruments.toList.map(_.tag).sorted)

object ArbAeonMultiFacility extends ArbAeonMultiFacility

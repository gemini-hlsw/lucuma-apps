// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import lucuma.core.enums.Instrument
import lucuma.core.util.arb.ArbEnumerated.given
import observe.model.Observation
import org.scalacheck.Cogen

trait ObserveServerArbitraries {

  given Cogen[Map[Instrument, Observation.Name]] =
    Cogen[List[(Instrument, Observation.Name)]].contramap(_.toList)

}

object ObserveServerArbitraries extends ObserveServerArbitraries

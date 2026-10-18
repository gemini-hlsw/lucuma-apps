// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model.arb

import cats.syntax.all.*
import lucuma.core.enums.Instrument
import lucuma.core.util.arb.ArbEnumerated.given
import observe.model.Subsystem
import observe.model.enums.ObservatorySubsystem
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbSubsystem:
  given Arbitrary[Subsystem] = Arbitrary:
    Gen.oneOf(arbitrary[Instrument], arbitrary[ObservatorySubsystem])

  given Cogen[Subsystem] = Cogen[Either[Instrument, ObservatorySubsystem]].contramap:
    case i: Instrument           => i.asLeft
    case o: ObservatorySubsystem => o.asRight

object ArbSubsystem extends ArbSubsystem

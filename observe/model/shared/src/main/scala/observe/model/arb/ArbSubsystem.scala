// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model.arb

import cats.syntax.all.*
import lucuma.core.enums.Instrument
import lucuma.core.util.arb.ArbEnumerated.given
import observe.model.Server
import observe.model.Subsystem
import observe.model.SubsystemOrServer
import observe.model.enums.Resource
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbSubsystem:
  given Arbitrary[Subsystem] = Arbitrary:
    Gen.oneOf(arbitrary[Resource], arbitrary[Instrument])

  given Cogen[Subsystem] = Cogen[Either[Resource, Instrument]].contramap:
    case r: Resource   => r.asLeft
    case i: Instrument => i.asRight

  given Arbitrary[SubsystemOrServer] = Arbitrary:
    Gen.oneOf(arbitrary[Subsystem], arbitrary[Server])

  given Cogen[SubsystemOrServer] = Cogen[Either[Subsystem, Server]].contramap:
    case s: Subsystem => s.asLeft
    case s: Server    => s.asRight

object ArbSubsystem extends ArbSubsystem

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.engine

import cats.Eq
import cats.effect.IO
import lucuma.core.util.arb.ArbGid.given
import observe.model.Observation
import observe.model.SequenceStatus
import observe.model.arb.ObserveModelArbitraries.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

final class EngineSpec extends munit.DisciplineSuite {

  given Eq[SequenceState[IO]] = Eq.fromUniversalEquals

  given Arbitrary[Sequence[IO]] = Arbitrary {
    for {
      id <- arbitrary[Observation.Id]
    } yield Sequence(id, None, Breakpoints.empty)
  }

  given Arbitrary[SequenceState[IO]] = Arbitrary {
    for {
      seq <- arbitrary[Sequence[IO]]
      st  <- arbitrary[SequenceStatus]
    } yield SequenceState[IO](
      obsId = seq.obsId,
      status = st,
      currentStep = None,
      breakpoints = Breakpoints.empty,
      singleRuns = Map.empty
    )
  }

  given Cogen[SequenceState[IO]] =
    Cogen[Observation.Id].contramap(_.obsId)

}

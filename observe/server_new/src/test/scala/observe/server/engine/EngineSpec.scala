// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.engine

import cats.Eq
import cats.effect.IO
import lucuma.core.enums.SequenceType
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbGid.given
import observe.model.Observation
import observe.model.SequenceStatus
import observe.model.arb.ObserveModelArbitraries.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

final class EngineSpec extends munit.DisciplineSuite {

  given Eq[SequenceState[IO]] = Eq.fromUniversalEquals

  given Arbitrary[SequenceState[IO]] = Arbitrary {
    for {
      obsId   <- arbitrary[Observation.Id]
      st      <- arbitrary[SequenceStatus]
      // currentStep <- arbitrary[Option[EngineStepExecutionZipper[IO]]]
      seqType <- arbitrary[SequenceType]
      // breakpoints <- arbitrary[Breakpoints]
      // singleRuns <- arbitrary[Map[ActionCoordsInSeq, ActionState]]
    } yield SequenceState[IO](
      obsId = obsId,
      status = st,
      loadedStep = None,
      currentSequenceType = seqType,
      breakpoints = Breakpoints.empty,
      singleRuns = Map.empty
    )
  }

  given Cogen[SequenceState[IO]] =
    Cogen[(Observation.Id, SequenceStatus, SequenceType)].contramap(sd =>
      (sd.obsId, sd.status, sd.currentSequenceType)
    )

}

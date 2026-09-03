// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model.arb

import lucuma.core.model.sequence.Step
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbNewType.given
import lucuma.core.util.arb.ArbTimeSpan.given
import lucuma.core.util.arb.ArbUid.given
import observe.model.*
import observe.model.GmosParameters.*
import observe.model.enums.ActionStatus
import observe.model.enums.PendingObserveCmd
import observe.model.enums.PendingObserveCmd.*
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Gen

import ArbNsRunningState.given
import ArbSubsystem.given

trait ArbNodAndShuffleStep {

  given Arbitrary[NodAndShuffleStatus] = Arbitrary[NodAndShuffleStatus] {
    for {
      as <- arbitrary[ActionStatus]
      t  <- arbitrary[TimeSpan]
      n  <- arbitrary[TimeSpan]
      c  <- arbitrary[NsCycles]
      s  <- arbitrary[Option[NsRunningState]]
    } yield NodAndShuffleStatus(as, t, n, c, s)
  }

  given Cogen[NodAndShuffleStatus] =
    Cogen[(ActionStatus, TimeSpan, TimeSpan, NsCycles, Option[NsRunningState])].contramap { x =>
      (x.observing, x.totalExposureTime, x.nodExposureTime, x.cycles, x.state)
    }

  given Arbitrary[PendingObserveCmd] =
    Arbitrary[PendingObserveCmd](
      Gen.oneOf(List(PauseGracefully, StopGracefully))
    )

  given nodShuffleStepArb: Arbitrary[ObserveStep.NodAndShuffle] =
    Arbitrary[ObserveStep.NodAndShuffle] {
      for {
        id <- arbitrary[Step.Id]
        cs <- arbitrary[Map[Subsystem, ActionStatus]]
        os <- arbitrary[NodAndShuffleStatus]
        oc <- arbitrary[Option[PendingObserveCmd]]
      } yield ObserveStep.NodAndShuffle(
        id = id,
        configStatus = cs,
        nsStatus = os,
        pendingObserveCmd = oc
      )
    }

  given nodShuffleStepCogen: Cogen[ObserveStep.NodAndShuffle] =
    Cogen[
      (
        Step.Id,
        List[(Subsystem, ActionStatus)],
        NodAndShuffleStatus
      )
    ].contramap(s =>
      (
        s.id,
        s.configStatus.toList,
        s.nsStatus
      )
    )

}

object ArbNodAndShuffleStep extends ArbNodAndShuffleStep

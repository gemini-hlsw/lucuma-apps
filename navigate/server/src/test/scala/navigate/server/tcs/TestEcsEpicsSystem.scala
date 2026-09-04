// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import cats.effect.Ref
import cats.effect.Temporal
import monocle.Focus
import navigate.epics.EpicsSystem.TelltaleChannel
import navigate.epics.TestChannel

object TestEcsEpicsSystem {
  case class State(
    telltale:        TestChannel.State[Int],
    eastVentGatePos: TestChannel.State[Double],
    westVentGatePos: TestChannel.State[Double]
  )

  val defaultState: State = State(
    TestChannel.State.default,
    TestChannel.State.default,
    TestChannel.State.default
  )

  def buildChannels[F[_]: Temporal](
    s: Ref[F, State]
  ): EcsChannels[F] = new EcsChannels[F](
    telltale =
      TelltaleChannel[F]("ECS", new TestChannel[F, State, Int](s, Focus[State](_.telltale))),
    eastVentGateAperture = new TestChannel[F, State, Double](s, Focus[State](_.eastVentGatePos)),
    westVentGateAperture = new TestChannel[F, State, Double](s, Focus[State](_.westVentGatePos))
  )

  def build[F[_]: Temporal](
    s: Ref[F, State]
  ): EcsEpicsSystem[F] = EcsEpicsSystem.buildSystem(buildChannels(s))

}

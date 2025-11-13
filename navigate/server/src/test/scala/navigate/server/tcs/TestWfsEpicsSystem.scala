// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import cats.Parallel
import cats.effect.Async
import cats.effect.Ref
import cats.effect.Temporal
import cats.effect.std.Dispatcher
import monocle.Focus
import navigate.epics.EpicsSystem.TelltaleChannel
import navigate.epics.TestChannel
import navigate.server.acm.CadDirective

object TestWfsEpicsSystem {
  case class State(
    telltale:                   TestChannel.State[String],
    tipGain:                    TestChannel.State[String],
    tiltGain:                   TestChannel.State[String],
    focusGain:                  TestChannel.State[String],
    scaleGain:                  TestChannel.State[String],
    reset:                      TestChannel.State[Double],
    gainsDir:                   TestChannel.State[CadDirective],
    flux:                       TestChannel.State[Int],
    centroid:                   TestChannel.State[Int],
    integrationTime:            TestChannel.State[Double],
    circularBufferDir:          TestChannel.State[CadDirective],
    circularBufferImage:        TestChannel.State[String],
    circularBufferAo:           TestChannel.State[String],
    circularBufferFg:           TestChannel.State[String],
    circularBufferImageEnabled: TestChannel.State[String],
    circularBufferAoEnabled:    TestChannel.State[String],
    circularBufferFgEnabled:    TestChannel.State[String]
  )

  val defaultState: State = State(
    TestChannel.State.default,
    TestChannel.State.default,
    TestChannel.State.default,
    TestChannel.State.default,
    TestChannel.State.default,
    TestChannel.State.default,
    TestChannel.State.default,
    TestChannel.State.default,
    TestChannel.State.default,
    TestChannel.State.default,
    TestChannel.State.default,
    TestChannel.State.default,
    TestChannel.State.default,
    TestChannel.State.default,
    TestChannel.State.default,
    TestChannel.State.default,
    TestChannel.State.default
  )

  def buildWfsChannels[F[_]: Temporal](
    sysName: String,
    s:       Ref[F, State]
  ): WfsChannels[F] = WfsChannels(
    telltale =
      TelltaleChannel[F](sysName, new TestChannel[F, State, String](s, Focus[State](_.telltale))),
    tipGain = new TestChannel[F, State, String](s, Focus[State](_.tipGain)),
    tiltGain = new TestChannel[F, State, String](s, Focus[State](_.tiltGain)),
    focusGain = new TestChannel[F, State, String](s, Focus[State](_.focusGain)),
    scaleGain = new TestChannel[F, State, String](s, Focus[State](_.scaleGain)),
    reset = new TestChannel[F, State, Double](s, Focus[State](_.reset)),
    gainsDir = new TestChannel[F, State, CadDirective](s, Focus[State](_.gainsDir)),
    flux = new TestChannel[F, State, Int](s, Focus[State](_.flux)),
    centroidDetected = new TestChannel[F, State, Int](s, Focus[State](_.centroid)),
    integrationTime = new TestChannel[F, State, Double](s, Focus[State](_.integrationTime))
  )

  def buildCircularBufferChannels[F[_]: Temporal](
    telltale: TelltaleChannel[F],
    s:        Ref[F, State]
  ): CircularBufferChannels[F] = CircularBufferChannels[F](
    telltale,
    new TestChannel[F, State, CadDirective](s, Focus[State](_.circularBufferDir)),
    new TestChannel[F, State, String](s, Focus[State](_.circularBufferImage)),
    new TestChannel[F, State, String](s, Focus[State](_.circularBufferAo)),
    new TestChannel[F, State, String](s, Focus[State](_.circularBufferFg)),
    new TestChannel[F, State, String](s, Focus[State](_.circularBufferImageEnabled)),
    new TestChannel[F, State, String](s, Focus[State](_.circularBufferAoEnabled)),
    new TestChannel[F, State, String](s, Focus[State](_.circularBufferFgEnabled))
  )

  def build[F[_]: {Async, Temporal, Parallel, Dispatcher}](
    sysName: String,
    s:       Ref[F, State]
  ): PwfsEpicsSystem[F] = {
    val wfs = WfsEpicsSystem.buildSystem(buildWfsChannels(sysName, s))
    val cb  = CircularBufferControl.buildSystem(buildCircularBufferChannels(wfs.telltale, s))

    new WfsEpicsSystem[F] with CircularBufferControl[F] {
      export wfs.*
      export cb.*
    }
  }

}

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

import cats.Parallel
import cats.effect.Async
import cats.effect.Ref
import cats.effect.Temporal
import cats.effect.std.Dispatcher
import monocle.Focus
import navigate.epics.TestChannel
import navigate.server.acm.CadDirective

object TestOiwfsEpicsSystem {
  case class State(
    detSigModeSeqDarkDir: TestChannel.State[CadDirective],
    seqDarkFilename:      TestChannel.State[String],
    detSigModeSeqDir:     TestChannel.State[CadDirective],
    z2m2:                 TestChannel.State[String],
    detSigInitDir:        TestChannel.State[CadDirective],
    darkFilename:         TestChannel.State[String]
  )

  val defaultState: State = State(
    TestChannel.State.default,
    TestChannel.State.default,
    TestChannel.State.default,
    TestChannel.State.default,
    TestChannel.State.default,
    TestChannel.State.default
  )

  def buildChannels[F[_]: Temporal](s: Ref[F, State]): OiwfsChannels[F] = OiwfsChannels(
    detSigModeSeqDarkDir = TestChannel(s, Focus[State](_.detSigModeSeqDarkDir)),
    seqDarkFilename = TestChannel(s, Focus[State](_.seqDarkFilename)),
    detSigModeSeqDir = TestChannel(s, Focus[State](_.detSigModeSeqDir)),
    z2m2 = TestChannel(s, Focus[State](_.z2m2)),
    detSigInitDir = TestChannel(s, Focus[State](_.detSigInitDir)),
    darkFilename = TestChannel(s, Focus[State](_.darkFilename))
  )

  def build[F[_]: {Async, Temporal, Parallel, Dispatcher}](
    wfs:   Ref[F, TestWfsEpicsSystem.State],
    oiwfs: Ref[F, State]
  ): OiwfsEpicsSystem[F] & CircularBufferControl[F] = {
    val wfsChannels = TestWfsEpicsSystem.buildWfsChannels("OIWFS", wfs)
    val cbChannels  = TestWfsEpicsSystem.buildCircularBufferChannels(wfsChannels.telltale, wfs)
    val ois         = OiwfsEpicsSystem.buildSystem(wfsChannels, buildChannels(oiwfs))
    val cb          = CircularBufferControl.buildSystem(cbChannels)
    new OiwfsEpicsSystem[F] with CircularBufferControl[F] {
      export ois.*
      export cb.*
    }

  }

}

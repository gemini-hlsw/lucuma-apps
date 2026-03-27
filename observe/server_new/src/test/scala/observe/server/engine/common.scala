// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.engine

import cats.data.NonEmptyList
import cats.effect.IO
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import observe.common.test.stepId
import observe.model.ActionType
import observe.model.SequenceStatus
import observe.model.dhs.DataId
import observe.model.enums.Resource
import observe.server.StepGen
import observe.server.engine.Action.ActionState

import java.util.UUID

// The only thing accessed during execution in tests is atomId compare it with previous one.
val DummyStepGen = StepGen.GmosNorth[IO](
  atomId = Atom.Id.fromUuid(UUID.randomUUID()),
  sequenceType = null,
  id = null,
  dataId = DataId(""),
  resources = Set(Resource.TCS, Resource.Gcal, Instrument.GmosNorth),
  obsControl = null,
  generator = null,
  instConfig = null,
  config = null,
  telescopeConfig = null,
  signalToNoise = null,
  breakpoint = null
)

val DummyExecutionZipper =
  ExecutionZipper
    .currentify[IO](
      EngineStep[IO](
        stepId(1),
        List(
          NonEmptyList.one:
            Action(ActionType.Undefined, fs2.Stream.empty, Action.State(ActionState.Idle, Nil))
        )
      )
    )
    .get

def initSeqState(
  obsId:        Observation.Id,
  loadedStep:   EngineStep[IO],
  sequenceType: SequenceType,
  breakpoints:  Breakpoints,
  status:       SequenceStatus = SequenceStatus.Idle
): SequenceState[IO] =
  SequenceState[IO](
    obsId,
    status,
    ExecutionZipper.currentify(loadedStep).map(LoadedStep(DummyStepGen, _)),
    sequenceType,
    breakpoints,
    Map.empty
  )

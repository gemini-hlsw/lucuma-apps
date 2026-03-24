// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.engine

import cats.effect.IO
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import observe.model.SequenceStatus

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
    ExecutionZipper.currentify(loadedStep).map(LoadedStep(null, _)),
    sequenceType,
    breakpoints,
    Map.empty
  )

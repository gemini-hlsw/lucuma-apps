// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.engine

import cats.effect.IO
import cats.syntax.option.*
import observe.model.Conditions
import observe.model.Observation
import observe.model.SystemOverrides
import observe.server.EngineState
import observe.server.Selected
import observe.server.SequenceData
import observe.server.TestCommon.*

object TestUtil {
  def initStateWithSequence(
    obsId: Observation.Id,
    seq:   SequenceState[IO]
  ): EngineState[IO] =
    EngineState[IO](
      queues = Map.empty,
      selected = Selected(
        gmosNorth = SequenceData
          .GmosNorth[IO](
            observer = none,
            overrides = SystemOverrides.AllEnabled,
            obsData = odbObservation(obsId),
            staticCfg = staticCfg1,
            currentStep = none,
            seq = seq,
            pendingObsCmd = none,
            visitStartDone = false,
            cleanup = IO.unit
          )
          .some,
        gmosSouth = none,
        flamingos2 = none
      ),
      conditions = Conditions.Default,
      operator = None
    )
}

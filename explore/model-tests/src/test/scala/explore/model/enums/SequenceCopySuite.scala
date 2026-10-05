// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import cats.syntax.all.*
import explore.model.Execution
import explore.model.arb.ArbExecution.given
import lucuma.core.enums.ExecutionState
import lucuma.core.util.Enumerated
import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

class SequenceCopySuite extends ScalaCheckSuite:

  private def withState(
    e:     Execution,
    acq:   Boolean,
    sci:   Boolean,
    state: ExecutionState
  ): Execution =
    e.copy(
      acquisitionSequenceIsMaterialized = acq,
      scienceSequenceIsMaterialized = sci,
      executionState = state
    )

  private val allStates: List[ExecutionState] = Enumerated[ExecutionState].all

  property("no choices without a materialized sequence"):
    forAll: (e: Execution) =>
      allStates.foreach: state =>
        assertEquals(SequenceCopy.choices(withState(e, false, false, state)), Nil)

  private val materialized: List[(Boolean, Boolean)] = List((true, false), (false, true), (true, true))

  property("generate new or copy steps when nothing has started"):
    forAll: (e: Execution) =>
      materialized.foreach: (acq, sci) =>
        assertEquals(
          SequenceCopy.choices(withState(e, acq, sci, ExecutionState.NotStarted)),
          List(SequenceCopy.GenerateNew, SequenceCopy.AllSteps)
        )

  property("all three choices once execution has started"):
    forAll: (e: Execution) =>
      for
        state      <- allStates.filterNot(_ === ExecutionState.NotStarted)
        (acq, sci) <- materialized
      do
        assertEquals(
          SequenceCopy.choices(withState(e, acq, sci, state)),
          List(SequenceCopy.GenerateNew, SequenceCopy.PendingSteps, SequenceCopy.AllSteps)
        )

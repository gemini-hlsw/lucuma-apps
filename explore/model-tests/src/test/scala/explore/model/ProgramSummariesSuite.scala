// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Order.given
import cats.syntax.all.*
import explore.model.arb.ArbObservation.given
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.model.ObservationWorkflow
import lucuma.core.model.arb.ArbObservationWorkflow.given
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.model.sequence.arb.ArbExecutionDigest.given
import lucuma.core.util.CalculatedValue
import lucuma.core.util.arb.ArbCalculatedValue.given
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.arb.ArbBasicConfiguration.given
import munit.Location
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*

import scala.collection.immutable.SortedMap

class ProgramSummariesSuite extends ScalaCheckSuite:
  val emptyPS = ProgramSummaries(none,
                                 SortedMap.empty,
                                 SortedMap.empty,
                                 SortedMap.empty,
                                 SortedMap.empty,
                                 SortedMap.empty,
                                 SortedMap.empty
  )

  def obsCalcValues(obs: Observation)                           = Observation.calculatedValues.get(obs)
  def psCalcValues(ps: ProgramSummaries, obsId: Observation.Id) =
    obsCalcValues(ps.observations(obsId))

  def assertSameExceptCalculatedValues(ps: ProgramSummaries, obs: Observation)(using
    loc: Location
  ): Unit =
    val cached    = ps.observations(obs.id)
    val predicate = Observation.calculatedValues
      .replace(Observation.calculatedValues.get(obs))
      .andThen(Observation.observingMode.replace(obs.observingMode))(cached)
    assertEquals(predicate, obs)

  test("inserting observation with no orphan"):
    forAll: (obs: Observation) =>
      val ps = emptyPS.upsertObs(obs)
      assertEquals(ps.observations(obs.id), obs)

  test("inserting observation with an orphan"):
    forAll:
      (
        obs:      Observation,
        workflow: CalculatedValue[ObservationWorkflow],
        digest:   CalculatedValue[Option[ExecutionDigest]]
      ) =>
        val ps  = emptyPS.updateCalculatedValues(obs.id, workflow, digest)
        assertEquals(1, ps.calculatedValueOrphans.size)
        val ps1 = ps.upsertObs(obs)
        // it should have applied the orphan values to the observation
        assertEquals(psCalcValues(ps1, obs.id), (workflow, digest))
        assertEquals(0, ps1.calculatedValueOrphans.size)
        assertSameExceptCalculatedValues(ps1, obs)

  test("Updating observation preserves old calculated values and guide target name"):
    forAll: (obs1: Observation, obs2: Observation) =>
      val obs2as1 = Observation.id.replace(obs1.id)(obs2)
      val ps      = emptyPS.upsertObs(obs1)
      assertEquals(psCalcValues(ps, obs1.id), obsCalcValues(obs1))
      val ps1     = ps.upsertObs(obs2as1)
      assertEquals(psCalcValues(ps1, obs1.id), obsCalcValues(obs1))
      assertEquals(ps1.observations(obs1.id).selectedGSName, obs1.selectedGSName)
      assertSameExceptCalculatedValues(
        ps1,
        Observation.selectedGSName.replace(obs1.selectedGSName)(obs2as1)
      )

  test("Update observation calculated values"):
    forAll:
      (
        obs:      Observation,
        workflow: CalculatedValue[ObservationWorkflow],
        digest:   CalculatedValue[Option[ExecutionDigest]]
      ) =>
        val ps = emptyPS.upsertObs(obs).updateCalculatedValues(obs.id, workflow, digest)
        assertEquals(psCalcValues(ps, obs.id), (workflow, digest))
        assertSameExceptCalculatedValues(ps, obs)

  private def obsWith(
    obs:           Observation,
    config:        Option[BasicConfiguration],
    state:         ObservationWorkflowState,
    isCalibration: Boolean
  ): Observation =
    Observation.basicConfiguration
      .replace(config)
      .andThen(Observation.workflowState.replace(state))
      .andThen(
        Observation.calibrationRole.replace(Option.when(isCalibration)(CalibrationRole.Twilight))
      )(obs)

  test("aeonEligibleInstruments: a defined-or-higher observation contributes its instrument"):
    forAll: (obs: Observation, config: BasicConfiguration, state: ObservationWorkflowState) =>
      val ps       = emptyPS.upsertObs(obsWith(obs, config.some, state, isCalibration = false))
      val expected =
        if state >= ObservationWorkflowState.Defined then
          (config.instrument, config.siteFor).tupled.toList.toMap
        else Map.empty
      assertEquals(ps.aeonEligibleInstruments, expected)

  test("aeonEligibleInstruments: calibrations never contribute"):
    forAll: (obs: Observation, config: BasicConfiguration) =>
      val ps = emptyPS.upsertObs(
        obsWith(obs, config.some, ObservationWorkflowState.Ready, isCalibration = true)
      )
      assert(ps.aeonEligibleInstruments.isEmpty)

  test("aeonEligibleInstruments: an observation with no configuration contributes nothing"):
    forAll: (obs: Observation) =>
      val ps =
        emptyPS.upsertObs(obsWith(obs, none, ObservationWorkflowState.Ready, isCalibration = false))
      assert(ps.aeonEligibleInstruments.isEmpty)

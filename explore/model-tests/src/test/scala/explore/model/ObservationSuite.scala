// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.kernel.laws.discipline.EqTests
import cats.syntax.all.*
import crystal.Pot
import explore.model.arb.ArbObservation
import io.circe.parser.parse
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.ObservingMode
import munit.DisciplineSuite
import org.scalacheck.Prop.*

class ObservationSuite extends DisciplineSuite:
  import ArbObservation.given

  checkAll("Eq[Observation]", EqTests[Observation].eqv)

  property("a suspended observation is ongoing, and so executed"):
    forAll: (obs: Observation, state: ObservationWorkflowState) =>
      val o = Observation.workflowState.replace(state)(obs)
      assertEquals(
        o.isOngoing,
        state === ObservationWorkflowState.Ongoing || state === ObservationWorkflowState.Suspended
      )
      assertEquals(o.isExecuted, o.isOngoing || state === ObservationWorkflowState.Completed)

  private def decodeModes(json: String) =
    Observation.decodeObservingModes(parse(json).toOption.get.hcursor)

  test("a summary-only daytime calibration mode is pending"):
    assertEquals(
      decodeModes("""{ "daytimeCalibration": { "instrument": "GMOS_NORTH" } }"""),
      Right((BasicConfiguration.DaytimeCalibration(Instrument.GmosNorth).some, Pot.Pending))
    )

  test("a hydrated daytime calibration mode is ready"):
    assertEquals(
      decodeModes(
        """{ "daytimeCalibration": { "instrument": "GMOS_NORTH", "calibrationSets": [] } }"""
      ),
      Right(
        (BasicConfiguration.DaytimeCalibration(Instrument.GmosNorth).some,
         Pot.Ready(ObservingMode.DaytimeCalibration(Instrument.GmosNorth, Nil).some)
        )
      )
    )

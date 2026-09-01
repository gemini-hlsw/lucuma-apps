// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import explore.model.arb.ArbObservation
import explore.model.enums.ObsValidationSeverity
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.model.ObservationValidation
import munit.FunSuite
import org.scalacheck.Arbitrary.arbitrary

class ObsValidationSeveritySuite extends FunSuite:
  import ArbObservation.given

  private val ErrorCode   = ObservationValidationCode.ConfigurationError
  private val WarningCode = ObservationValidationCode.GenericWarning

  private val baseObs: Observation =
    arbitrary[Observation].sample.get

  private def obsWith(
    state:            ObservationWorkflowState,
    codes:            List[ObservationValidationCode],
    validTransitions: List[ObservationWorkflowState] = List.empty
  ): Observation =
    Observation.workflowState.replace(state)(
      Observation.workflowValidTransitions.replace(validTransitions)(
        Observation.validationErrors
          .replace(codes.map(c => ObservationValidation.fromMsgs(c, "a message")))(baseObs)
      )
    )

  test("no validations has no severity"):
    ObservationWorkflowState.values.foreach: state =>
      assertEquals(obsWith(state, List.empty).validationSeverity, None)

  test("a fatal validation is an Error in every state"):
    ObservationWorkflowState.values.foreach: state =>
      val obs = obsWith(state, List(ErrorCode))
      assertEquals(obs.validationSeverity, Some(ObsValidationSeverity.Error))
      assertEquals(obs.severityOf(ErrorCode), ObsValidationSeverity.Error)

  test("warnings before Ready are unacknowledged"):
    List(ObservationWorkflowState.Undefined,
         ObservationWorkflowState.Unapproved,
         ObservationWorkflowState.Defined
    ).foreach: state =>
      val obs = obsWith(state, List(WarningCode))
      assertEquals(obs.validationSeverity, Some(ObsValidationSeverity.Warning))
      assertEquals(obs.severityOf(WarningCode), ObsValidationSeverity.Warning)

  test("warnings at Ready or later are acknowledged"):
    List(ObservationWorkflowState.Ready,
         ObservationWorkflowState.Ongoing,
         ObservationWorkflowState.Completed
    ).foreach: state =>
      val obs = obsWith(state, List(WarningCode))
      assertEquals(obs.validationSeverity, Some(ObsValidationSeverity.AcknowledgedWarning))
      assertEquals(obs.severityOf(WarningCode), ObsValidationSeverity.AcknowledgedWarning)

  test("an inactive observation acknowledges warnings based on its valid transitions"):
    assertEquals(
      obsWith(ObservationWorkflowState.Inactive,
              List(WarningCode),
              List(ObservationWorkflowState.Ready)
      ).validationSeverity,
      Some(ObsValidationSeverity.AcknowledgedWarning)
    )
    assertEquals(
      obsWith(ObservationWorkflowState.Inactive,
              List(WarningCode),
              List(ObservationWorkflowState.Defined)
      ).validationSeverity,
      Some(ObsValidationSeverity.Warning)
    )

  test("errors keep warnings on the same observation unacknowledged"):
    val obs = obsWith(ObservationWorkflowState.Ready, List(ErrorCode, WarningCode))
    assertEquals(obs.validationSeverity, Some(ObsValidationSeverity.Error))
    assertEquals(obs.severityOf(ErrorCode), ObsValidationSeverity.Error)
    assertEquals(obs.severityOf(WarningCode), ObsValidationSeverity.Warning)

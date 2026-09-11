// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import explore.model.arb.ArbObservation
import explore.model.enums.ObsValidationSeverity
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.model.ObservationValidation
import munit.FunSuite
import org.scalacheck.Arbitrary.arbitrary

class ObsValidationSeveritySuite extends FunSuite:
  import ArbObservation.given

  private val ErrorCode    = ObservationValidationCode.ConfigurationError
  private val WarningCode  = ObservationValidationCode.LowTotalSignalToNoise
  private val WarningCode2 = ObservationValidationCode.ConditionsUnlikely

  private val baseObs: Observation =
    arbitrary[Observation].sample.get

  private def obsWith(codes: ObservationValidationCode*): Observation =
    Observation.validationErrors
      .replace(codes.toList.map(c => ObservationValidation.fromMsgs(c, "a message")))(baseObs)

  private val nothingDismissed: DismissedWarnings = Set.empty

  test("no validations has no severity"):
    assertEquals(obsWith().validationSeverity(nothingDismissed), None)
    assertEquals(obsWith().validationSeverity(Set(WarningCode)), None)

  test("an error is an Error whatever has been dismissed"):
    val obs = obsWith(ErrorCode)
    assertEquals(obs.validationSeverity(nothingDismissed), Some(ObsValidationSeverity.Error))
    assertEquals(obs.validationSeverity(Set(WarningCode)), Some(ObsValidationSeverity.Error))
    assertEquals(obs.severityOf(ErrorCode, Set(WarningCode)), ObsValidationSeverity.Error)

  test("a warning is dismissed only when its code is in the program's dismissed list"):
    val obs = obsWith(WarningCode)
    assertEquals(obs.validationSeverity(nothingDismissed), Some(ObsValidationSeverity.Warning))
    assertEquals(obs.severityOf(WarningCode, nothingDismissed), ObsValidationSeverity.Warning)
    assertEquals(
      obs.validationSeverity(Set(WarningCode)),
      Some(ObsValidationSeverity.DismissedWarning)
    )
    assertEquals(
      obs.severityOf(WarningCode, Set(WarningCode)),
      ObsValidationSeverity.DismissedWarning
    )

  test("dismissing an unrelated warning leaves this one alone"):
    val obs = obsWith(WarningCode)
    assertEquals(obs.validationSeverity(Set(WarningCode2)), Some(ObsValidationSeverity.Warning))

  test("an observation can mix dismissed and non-dismissed warnings"):
    val obs                          = obsWith(WarningCode, WarningCode2)
    val dismissed: DismissedWarnings = Set(WarningCode)
    assertEquals(obs.severityOf(WarningCode, dismissed), ObsValidationSeverity.DismissedWarning)
    assertEquals(obs.severityOf(WarningCode2, dismissed), ObsValidationSeverity.Warning)
    // The observation as a whole is still warning, because one warning stands.
    assertEquals(obs.validationSeverity(dismissed), Some(ObsValidationSeverity.Warning))

  test("an observation with all of its warnings dismissed is a DismissedWarning"):
    assertEquals(
      obsWith(WarningCode, WarningCode2).validationSeverity(Set(WarningCode, WarningCode2)),
      Some(ObsValidationSeverity.DismissedWarning)
    )

  test("an error outranks a dismissed warning on the same observation"):
    val obs                          = obsWith(ErrorCode, WarningCode)
    val dismissed: DismissedWarnings = Set(WarningCode)
    assertEquals(obs.validationSeverity(dismissed), Some(ObsValidationSeverity.Error))
    // ...but the warning itself is still reported as dismissed.
    assertEquals(obs.severityOf(WarningCode, dismissed), ObsValidationSeverity.DismissedWarning)

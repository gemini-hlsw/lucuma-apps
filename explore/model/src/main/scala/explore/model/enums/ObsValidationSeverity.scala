// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import cats.Order
import explore.model.DismissedWarnings
import lucuma.core.enums.ObservationValidationCode

/**
 * How an observation's validations should be presented to the user. `DismissedWarning` still reads
 * as "Warning", but is displayed with a checkmark over the warning icon. The cases are declared in
 * ascending severity, so the `Order` picks the most severe.
 */
enum ObsValidationSeverity(val label: String):
  case DismissedWarning extends ObsValidationSeverity("Warning")
  case Warning          extends ObsValidationSeverity("Warning")
  case Error            extends ObsValidationSeverity("Error")

  /**
   * Appended to the severity label, or to a validation code's name, to call out that the warning
   * has been dismissed.
   */
  def dismissedSuffix: String =
    this match
      case DismissedWarning => " (Dismissed)"
      case Error | Warning  => ""

  def fullLabel: String = label + dismissedSuffix

object ObsValidationSeverity:
  // Ascending severity, matching the declaration order of the cases.
  given Order[ObsValidationSeverity] = Order.by(_.ordinal)

  /** The severity of a validation code, given the warnings dismissed by its program. */
  def of(
    code:      ObservationValidationCode,
    dismissed: DismissedWarnings
  ): ObsValidationSeverity =
    code.fold(_ => Error, w => if (dismissed.contains(w)) DismissedWarning else Warning)

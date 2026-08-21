// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import cats.Eq
import cats.derived.*

/**
 * How an observation's validations should be presented to the user. `AcknowledgedWarning` still
 * reads as "Warning", but is displayed with a checkmark over the warning icon.
 */
enum ObsValidationSeverity(val label: String) derives Eq:
  case Error               extends ObsValidationSeverity("Error")
  case AcknowledgedWarning extends ObsValidationSeverity("Warning")
  case Warning             extends ObsValidationSeverity("Warning")

  /**
   * Appended to the severity label, or to a validation code's name, to call out that the
   * observation's warnings have been acknowledged.
   */
  def acknowledgedSuffix: String =
    this match
      case AcknowledgedWarning => " (Acknowledged)"
      case Error | Warning     => ""

  def fullLabel: String = label + acknowledgedSuffix

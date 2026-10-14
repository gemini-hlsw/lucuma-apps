// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import eu.timepit.refined.collection.NonEmpty
import lucuma.core.math.Wavelength
import lucuma.core.optics.ValidWedge
import lucuma.core.syntax.validation.*
import lucuma.core.validation.InputValidFormat
import lucuma.core.validation.InputValidWedge
import lucuma.refined.*
import lucuma.ui.input.ChangeAuditor

/**
 * How the sequence table shows and edits wavelengths, so apps with a units preference can override
 * the nm default.
 */
case class SequenceWavelengthFormat(
  symbol:        String,
  display:       Wavelength => String,
  validFormat:   InputValidFormat[Wavelength],
  changeAuditor: ChangeAuditor
)

object SequenceWavelengthFormat:
  val Nanometers: SequenceWavelengthFormat =
    SequenceWavelengthFormat(
      "nm",
      SequenceRowFormatters.FormatWavelength(_).value,
      InputValidWedge
        .truncatedBigDecimal(1.refined)
        .andThen(
          ValidWedge
            .fromFormat(Wavelength.decimalNanometers, _ => "Invalid Wavelength".refined[NonEmpty])
            .toErrorsValidWedge
        ),
      ChangeAuditor.posBigDecimal(1.refined)
    )

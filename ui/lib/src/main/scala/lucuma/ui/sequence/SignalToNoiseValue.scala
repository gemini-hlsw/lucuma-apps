// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import cats.Eq
import cats.derived.*
import lucuma.core.math.SignalToNoise
import lucuma.itc.SignalToNoiseAt
import lucuma.schemas.model.ItcResultValues

enum SignalToNoiseValue derives Eq:
  case Value(sn: SignalToNoise)
  case Saturated

object SignalToNoiseValue:
  /**
   * The value for an ITC result. The ODB reports a result without a signal to noise when the target
   * saturates, which is different from having no ITC result at all.
   */
  def fromItc(itc: ItcResultValues)(sn: SignalToNoiseAt => SignalToNoise): SignalToNoiseValue =
    itc.signalToNoise.fold(Saturated)(s => Value(sn(s)))

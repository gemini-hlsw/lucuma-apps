// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence.byInstrument

import lucuma.core.enums.SequenceType
import lucuma.core.math.SignalToNoise
import lucuma.core.math.SingleSN
import lucuma.core.math.TotalSN

trait SpectroscopySequenceTable[D]:
  def acquisitonSN: Option[(SingleSN, TotalSN)]
  def scienceSN: Option[(SingleSN, TotalSN)]

  def signalToNoise: SequenceType => D => Option[SignalToNoise] =
    seqType =>
      _ =>
        val snPerClass: Option[(SingleSN, TotalSN)] =
          seqType match
            case SequenceType.Acquisition => acquisitonSN
            case SequenceType.Science     => scienceSN
        snPerClass.map(_._1.value)

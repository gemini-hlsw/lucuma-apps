// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence.byInstrument

import lucuma.core.enums.SequenceType
import lucuma.core.math.SignalToNoise
import lucuma.itc.SignalToNoiseAt

trait SpectroscopySequenceTable[D]:
  def acquisitonSN: Option[SignalToNoiseAt]
  def scienceSN: Option[SignalToNoiseAt]

  def signalToNoise: SequenceType => D => Option[SignalToNoise] =
    seqType =>
      _ =>
        val snPerClass: Option[SignalToNoiseAt] =
          seqType match
            case SequenceType.Acquisition => acquisitonSN
            case SequenceType.Science     => scienceSN
        snPerClass.map(_.single.value)

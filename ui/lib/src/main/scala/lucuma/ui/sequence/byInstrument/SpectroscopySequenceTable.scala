// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence.byInstrument

import lucuma.core.enums.SequenceType
import lucuma.core.math.SignalToNoise
import lucuma.itc.SignalToNoiseAt

trait SpectroscopySequenceTable[D](useAcquisitionCoadds: Boolean = false):
  def acquisitonSN: Option[SignalToNoiseAt]
  def scienceSN: Option[SignalToNoiseAt]

  private def selectSNValue(seqType: SequenceType)(snAt: SignalToNoiseAt): SignalToNoise =
    seqType match // For instruments that use coadds in acquisition, we have to report the total S/N.
      case SequenceType.Acquisition if useAcquisitionCoadds => snAt.total.value
      case _                                                => snAt.single.value

  def signalToNoise: SequenceType => D => Option[SignalToNoise] =
    seqType =>
      _ =>
        val snPerClass: Option[SignalToNoiseAt] =
          seqType match
            case SequenceType.Acquisition => acquisitonSN
            case SequenceType.Science     => scienceSN
        snPerClass.map(selectSNValue(seqType))

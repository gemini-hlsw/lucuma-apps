// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence.byInstrument

import lucuma.core.enums.SequenceType
import lucuma.core.math.SignalToNoise
import lucuma.itc.SignalToNoiseAt
import lucuma.schemas.model.ItcResultValues

trait SpectroscopySequenceTable[D](useAcquisitionCoadds: Boolean = false):
  def acquisitionItc: ItcResultValues
  def scienceItc: ItcResultValues

  private def itcForSequenceType(seqType: SequenceType): ItcResultValues =
    seqType match
      case SequenceType.Acquisition => acquisitionItc
      case SequenceType.Science     => scienceItc

  private def selectSNValue(seqType: SequenceType)(snAt: SignalToNoiseAt): SignalToNoise =
    seqType match // For instruments that use coadds in acquisition, we have to report the total S/N.
      case SequenceType.Acquisition if useAcquisitionCoadds => snAt.total.value
      case _                                                => snAt.single.value

  def signalToNoise: SequenceType => D => Option[SignalToNoise] =
    seqType => _ => itcForSequenceType(seqType).signalToNoise.map(selectSNValue(seqType))

  def peakPixelFlux: SequenceType => D => Option[Double] =
    seqType => _ => itcForSequenceType(seqType).peakPixelFlux

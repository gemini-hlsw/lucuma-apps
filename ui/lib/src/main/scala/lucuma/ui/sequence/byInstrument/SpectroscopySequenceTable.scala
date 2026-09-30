// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence.byInstrument

import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.SequenceType
import lucuma.core.math.SignalToNoise
import lucuma.schemas.model.ItcResultValues
import lucuma.schemas.model.PeakPixel

trait SpectroscopySequenceTable[D]:
  def acquisitionItc: ItcResultValues
  def scienceItc: ItcResultValues

  private def itcForSequenceType(seqType: SequenceType): ItcResultValues =
    seqType match
      case SequenceType.Acquisition => acquisitionItc
      case SequenceType.Science     => scienceItc

  def signalToNoise: SequenceType => Option[NonEmptyString] => D => Option[SignalToNoise] =
    seqType => _ => _ => itcForSequenceType(seqType).signalToNoise.map(_.single.value)

  def peakPixel: SequenceType => Option[NonEmptyString] => D => Option[PeakPixel] =
    seqType => _ => _ => itcForSequenceType(seqType).peakPixel

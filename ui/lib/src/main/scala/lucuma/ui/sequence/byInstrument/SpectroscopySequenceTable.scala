// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence.byInstrument

import cats.syntax.option.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.SequenceType
import lucuma.schemas.model.ItcResultValues
import lucuma.schemas.model.PeakPixel
import lucuma.ui.sequence.SignalToNoiseValue

/**
 * `acquisitionItc` is empty for modes that have no acquisition ITC.
 */
trait SpectroscopySequenceTable[D]:
  def acquisitionItc: Option[ItcResultValues]
  def scienceItc: ItcResultValues

  private def itcForSequenceType(seqType: SequenceType): Option[ItcResultValues] =
    seqType match
      case SequenceType.Acquisition => acquisitionItc
      case SequenceType.Science     => scienceItc.some

  def signalToNoise: SequenceType => Option[NonEmptyString] => D => Option[SignalToNoiseValue] =
    seqType =>
      _ => _ => itcForSequenceType(seqType).map(SignalToNoiseValue.fromItc(_)(_.single.value))

  def peakPixel: SequenceType => Option[NonEmptyString] => D => Option[PeakPixel] =
    seqType => _ => _ => itcForSequenceType(seqType).flatMap(_.peakPixel)

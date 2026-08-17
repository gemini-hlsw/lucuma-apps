// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence.byInstrument

import lucuma.core.enums.SequenceType
import lucuma.core.math.SignalToNoise
import lucuma.schemas.model.ItcResultValues
import lucuma.schemas.model.PeakPixel

trait ImagingSequenceTable[D, Filter]:
  def itcPerFilter: Map[Filter, ItcResultValues]
  def filterFromDynamicConfig: D => Option[Filter]

  private def itcFor(dynamicConfig: D): Option[ItcResultValues] =
    filterFromDynamicConfig(dynamicConfig).flatMap(itcPerFilter.get)

  def signalToNoise: SequenceType => D => Option[SignalToNoise] =
    _ => dynamicConfig => itcFor(dynamicConfig).flatMap(_.signalToNoise).map(_.single.value)

  def peakPixel: SequenceType => D => Option[PeakPixel] =
    _ => dynamicConfig => itcFor(dynamicConfig).flatMap(_.peakPixel)

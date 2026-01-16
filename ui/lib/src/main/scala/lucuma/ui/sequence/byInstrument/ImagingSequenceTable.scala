// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence.byInstrument

import lucuma.core.enums.SequenceType
import lucuma.core.math.SignalToNoise
import lucuma.itc.SignalToNoiseAt

trait ImagingSequenceTable[D, Filter]:
  def snPerFilter: Map[Filter, SignalToNoiseAt]
  def filterFromDynamicConfig: D => Option[Filter]

  def signalToNoise: SequenceType => D => Option[SignalToNoise] =
    _ =>
      dynamicConfig =>
        filterFromDynamicConfig(dynamicConfig).flatMap(snPerFilter.get(_)).map(_.single.value)

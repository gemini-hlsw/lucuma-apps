// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence.byInstrument

import lucuma.core.enums.SequenceType
import lucuma.core.math.SignalToNoise
import lucuma.core.math.SingleSN
import lucuma.core.math.TotalSN

trait ImagingSequenceTable[D, Filter]:
  def snPerFilter: Map[Filter, (SingleSN, TotalSN)]

  def filterFromDynamicConfig: D => Option[Filter]

  def signalToNoise: SequenceType => D => Option[SignalToNoise] =
    _ =>
      dynamicConfig =>
        filterFromDynamicConfig(dynamicConfig).flatMap(snPerFilter.get).map(_._1.value)

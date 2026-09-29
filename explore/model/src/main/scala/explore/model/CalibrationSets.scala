// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.refined.numeric.NonZeroInt
import lucuma.core.util.TimeSpan
import lucuma.ui.format.DurationSpacedFormatter

object CalibrationSets:
  private def format(t: TimeSpan): String = DurationSpacedFormatter(t.toDuration)

  /**
   * Summary of a number of calibration sets and their total time, or `None` when there are none.
   * The time is left out when it is zero, so a predicted count still shows.
   */
  def text(count: NonNegInt, total: TimeSpan): Option[String] =
    NonZeroInt
      .from(count.value)
      .toOption
      .map: n =>
        val sets = if n.value === 1 then "1 set" else s"$n sets"
        if total === TimeSpan.Zero then sets
        else if n.value === 1 then s"$sets, ${format(total)}"
        else s"$sets, ${format(total)} (${format(total /| n)} each)"

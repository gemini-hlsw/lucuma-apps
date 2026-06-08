// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.Order.given
import cats.derived.*
import clue.data.syntax.*
import io.circe.Decoder
import lucuma.core.model.TimingWindow
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.decoders.given
import lucuma.schemas.odb.input.*
import monocle.Focus

case class SchedulingConstraints(
  isSplittable:  Boolean,
  timingWindows: List[TimingWindow]
) derives Eq:
  def toInput: SchedulingPropertiesInput =
    SchedulingPropertiesInput(isSplittable.assign, timingWindows.map(_.toInput).assign)

object SchedulingConstraints:
  val isSplittable  = Focus[SchedulingConstraints](_.isSplittable)
  val timingWindows = Focus[SchedulingConstraints](_.timingWindows)

  // The ODB no longer exposes `isSplittable` on read (only `timingWindows`), so
  // it defaults to splittable. See SchedulingPropertiesInput on write.
  def fromTimingWindows(timingWindows: List[TimingWindow]): SchedulingConstraints =
    SchedulingConstraints(isSplittable = true, timingWindows = timingWindows.sorted)

  given Decoder[SchedulingConstraints] = Decoder.instance: c =>
    for
      isSplittable  <- c.get[Boolean]("isSplittable")
      timingWindows <- c.get[List[TimingWindow]]("timingWindows")
    yield SchedulingConstraints(isSplittable, timingWindows.sorted)

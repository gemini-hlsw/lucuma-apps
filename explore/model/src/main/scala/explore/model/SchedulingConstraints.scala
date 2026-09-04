// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.Order.given
import cats.derived.*
import clue.data.syntax.*
import io.circe.Decoder
import lucuma.core.enums.SchedulingMode
import lucuma.core.model.TimingWindow
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.decoders.given
import lucuma.schemas.odb.input.*
import monocle.Focus

// `schedulingMode` is what the Scheduler is permitted to do with the observation, a single
// ordered value that is both stored and edited. The observation's ToO activation is derived
// from it and the asterism by the ODB, so there is no second field to keep in step.
case class SchedulingConstraints(
  schedulingMode: SchedulingMode,
  timingWindows:  List[TimingWindow]
) derives Eq:
  def toInput: SchedulingConstraintsInput =
    SchedulingConstraintsInput(
      schedulingMode = schedulingMode.assign,
      timingWindows = timingWindows.map(_.toInput).assign
    )

object SchedulingConstraints:
  val schedulingMode = Focus[SchedulingConstraints](_.schedulingMode)
  val timingWindows  = Focus[SchedulingConstraints](_.timingWindows)

  given Decoder[SchedulingConstraints] = Decoder.instance: c =>
    for
      schedulingMode <- c.get[SchedulingMode]("schedulingMode")
      timingWindows  <- c.get[List[TimingWindow]]("timingWindows")
    yield SchedulingConstraints(schedulingMode, timingWindows.sorted)

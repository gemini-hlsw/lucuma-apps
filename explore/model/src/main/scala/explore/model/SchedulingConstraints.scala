// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.Order.given
import cats.derived.*
import clue.data.syntax.*
import io.circe.Decoder
import lucuma.core.enums.ExecutionRequirement
import lucuma.core.model.TimingWindow
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.decoders.given
import lucuma.schemas.odb.input.*
import monocle.Focus

// `executionRequirement` is the effective value the Scheduler honors: the more restrictive
// of the explicit requirement and the default implied by the ToO activation. Only the
// explicit one is editable, and it is all the API accepts as input.
case class SchedulingConstraints(
  executionRequirement:         ExecutionRequirement,
  explicitExecutionRequirement: Option[ExecutionRequirement],
  timingWindows:                List[TimingWindow]
) derives Eq:
  def toInput: SchedulingConstraintsInput =
    SchedulingConstraintsInput(
      explicitExecutionRequirement = explicitExecutionRequirement.orUnassign,
      timingWindows = timingWindows.map(_.toInput).assign
    )

object SchedulingConstraints:
  val executionRequirement         = Focus[SchedulingConstraints](_.executionRequirement)
  val explicitExecutionRequirement = Focus[SchedulingConstraints](_.explicitExecutionRequirement)
  val timingWindows                = Focus[SchedulingConstraints](_.timingWindows)

  given Decoder[SchedulingConstraints] = Decoder.instance: c =>
    for
      executionRequirement         <- c.get[ExecutionRequirement]("executionRequirement")
      explicitExecutionRequirement <-
        c.get[Option[ExecutionRequirement]]("explicitExecutionRequirement")
      timingWindows                <- c.get[List[TimingWindow]]("timingWindows")
    yield SchedulingConstraints(executionRequirement,
                                explicitExecutionRequirement,
                                timingWindows.sorted
    )

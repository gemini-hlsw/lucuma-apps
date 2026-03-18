// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model

import cats.Eq
import cats.Show
import cats.syntax.option.*
import lucuma.core.model.sequence.Step

// TODO Remove this?
sealed trait RunningStepProgress {
  val id: Option[Step.Id]
  val last: Int
  val total: Int
}

object RunningStepProgress {
  val Zero: RunningStepProgress = RunningStepImpl(none, 0, 0)

  private final case class RunningStepImpl(id: Option[Step.Id], last: Int, total: Int)
      extends RunningStepProgress

  def fromInt(id: Option[Step.Id], last: Int, total: Int): Option[RunningStepProgress] =
    if (total >= last) RunningStepImpl(id, last, total).some else none

  def unapply(r: RunningStepProgress): Option[(Option[Step.Id], Int, Int)] =
    Some((r.id, r.last, r.total))

  given Show[RunningStepProgress] =
    Show.show(u => s"${u.last + 1}/${u.total}")

  given Eq[RunningStepProgress] =
    Eq.by(x => (x.id, x.last, x.total))
}

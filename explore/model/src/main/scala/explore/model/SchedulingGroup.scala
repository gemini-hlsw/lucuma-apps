// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import monocle.Focus

case class SchedulingGroup(schedulingConstraints: SchedulingConstraints, obsIds: ObsIdSet)
    derives Eq

object SchedulingGroup:
  val schedulingConstraints = Focus[SchedulingGroup](_.schedulingConstraints)
  val obsIds                = Focus[SchedulingGroup](_.obsIds)

  def fromTuple(tuple: (ObsIdSet, SchedulingConstraints)): SchedulingGroup =
    SchedulingGroup(tuple._2, tuple._1)

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.implicits.*
import lucuma.core.model.Target
import monocle.Focus
import monocle.Lens

import scala.collection.immutable.SortedSet

// We keep track of the observations a target is in to know if it's shared or not.
case class TargetWithObs(target: Target, obsIds: SortedSet[Observation.Id]) derives Eq {
  def addObsIds(ids: ObsIdSet): TargetWithObs =
    TargetWithObs.obsIds.modify(_ ++ ids.toSortedSet)(this)

  def removeObsIds(ids: ObsIdSet): TargetWithObs =
    TargetWithObs.obsIds.modify(_ -- ids.toSortedSet)(this)
}

object TargetWithObs {
  val target: Lens[TargetWithObs, Target] =
    Focus[TargetWithObs](_.target)

  val obsIds: Lens[TargetWithObs, SortedSet[Observation.Id]] =
    Focus[TargetWithObs](_.obsIds)
}

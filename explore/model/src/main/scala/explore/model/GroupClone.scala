// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*

/** The result of cloning a group: the new root group and everything inside it, at any depth. */
case class GroupClone(
  root:         Group,
  subgroups:    List[Group],
  observations: List[Observation]
) derives Eq:
  val groups: List[Group] = root :: subgroups

  val ids: GroupClone.Ids =
    GroupClone.Ids(root.id, subgroups.map(_.id), observations.map(_.id))

object GroupClone:
  case class Ids(
    root:         Group.Id,
    subgroups:    List[Group.Id],
    observations: List[Observation.Id]
  ) derives Eq:
    val groups: List[Group.Id] = root :: subgroups

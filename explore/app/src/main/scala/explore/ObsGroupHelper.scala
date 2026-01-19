// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import explore.model.Group
import explore.model.GroupList
import explore.model.Observation
import explore.model.ObservationList
import explore.undo.UndoSetter

trait ObsGroupHelper:
  protected def observations: UndoSetter[ObservationList]
  protected def groups: UndoSetter[GroupList]
  protected def focusedObsId: Option[Observation.Id]
  protected def focusedGroupId: Option[Group.Id]

  def activeGroupId: Option[Group.Id] =
    focusedGroupId.orElse:
      focusedObsId.flatMap(observations.get.get(_)).flatMap(_.groupId)

  // If it's a telluric calibration group, switch to its parent group.
  def resolveGroupId(groupId: Option[Group.Id]): Option[Group.Id] =
    val group: Option[Group] = groupId.flatMap(groups.get.get(_))
    group
      .filterNot(_.isTelluricCalibration)
      .map(_.id)
      .orElse(group.flatMap(_.parentId))

  def resolvedActiveGroupId: Option[Group.Id] =
    resolveGroupId(activeGroupId)

// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Endo
import cats.Order.given
import cats.syntax.all.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.NonNegShort
import lucuma.core.enums.CloneSequenceMode
import lucuma.ui.optics.*
import monocle.Focus
import monocle.Iso
import monocle.Lens

final case class ObservationsAndGroups(observations: ObservationList, groups: GroupList):
  private def pullUpObservations(
    groupId: Option[Group.Id],
    from:    NonNegShort,
    omit:    Option[Observation.Id]
  ): Endo[ObservationList] =
    _.map:
      case (listObsId, obs)
          if !omit.contains_(listObsId) && obs.groupId === groupId && obs.groupIndex > from =>
        listObsId -> obs.copy(groupIndex =
          NonNegShort.unsafeFrom((obs.groupIndex.value - 1).toShort)
        )
      case other => other

  private def pushDownObservations(
    groupId: Option[Group.Id],
    from:    NonNegShort,
    omit:    Option[Observation.Id]
  ): Endo[ObservationList] =
    _.map:
      case (listObsId, obs)
          if !omit.contains_(listObsId) && obs.groupId === groupId && obs.groupIndex >= from =>
        listObsId -> obs.copy(groupIndex =
          NonNegShort.unsafeFrom((obs.groupIndex.value + 1).toShort)
        )
      case other => other

  private def pullUpGroups(
    parentGroupId: Option[Group.Id],
    from:          NonNegShort,
    omit:          Option[Group.Id]
  ): Endo[GroupList] =
    _.map:
      case (listGroupId, group)
          if !omit.contains_(
            listGroupId
          ) && group.parentId === parentGroupId && group.parentIndex > from =>
        listGroupId -> group.copy(parentIndex =
          NonNegShort.unsafeFrom((group.parentIndex.value - 1).toShort)
        )
      case other => other

  private def pushDownGroups(
    parentGroupId: Option[Group.Id],
    from:          NonNegShort,
    omit:          Option[Group.Id]
  ): Endo[GroupList] =
    _.map:
      case (listGroupId, group)
          if !omit.contains_(
            listGroupId
          ) && group.parentId === parentGroupId && group.parentIndex >= from =>
        listGroupId -> group.copy(parentIndex =
          NonNegShort.unsafeFrom((group.parentIndex.value + 1).toShort)
        )
      case other => other

  private def adjustItemsFor(
    newGroupId: Option[Group.Id],
    newIndex:   NonNegShort,
    item:       Either[Observation.Id, Group.Id]
  ): ObservationsAndGroups =
    copy(
      observations = (
        pullUpObservations(newGroupId, newIndex, item.left.toOption) >>>
          pushDownObservations(newGroupId, newIndex, item.left.toOption)
      )(observations),
      groups = (
        pullUpGroups(newGroupId, newIndex, item.toOption) >>>
          pushDownGroups(newGroupId, newIndex, item.toOption)
      )(groups)
    )

  def relocateObservation(
    obsId:      Observation.Id,
    newGroupId: Option[Group.Id],
    newIndex:   NonNegShort
  ): ObservationsAndGroups =
    ObservationsAndGroups.observations
      .andThen(ObservationList.obsWithId(obsId))
      .composeOptionLens(Observation.groupInfo)
      .set((newGroupId, newIndex).some)(adjustItemsFor(newGroupId, newIndex, obsId.asLeft))

  def relocateGroup(
    groupId:    Group.Id,
    newGroupId: Option[Group.Id],
    newIndex:   NonNegShort
  ): ObservationsAndGroups =
    ObservationsAndGroups.groups
      .andThen(GroupList.groupWithId(groupId))
      .composeOptionLens(Group.parentInfo)
      .set((newGroupId, newIndex).some)(adjustItemsFor(newGroupId, newIndex, groupId.asRight))

  /** The ids of the groups below `groupId`, at any depth. */
  def descendantGroupIds(groupId: Group.Id): Set[Group.Id] =
    groups.values
      .filter(_.parentId.contains_(groupId))
      .flatMap(g => descendantGroupIds(g.id) + g.id)
      .toSet

  /** The observations inside `groupId`, at any depth. */
  def observationsUnder(groupId: Group.Id): List[Observation] =
    val groupIds: Set[Group.Id] = descendantGroupIds(groupId) + groupId
    observations.values.filter(_.groupId.exists(groupIds.contains)).toList

  /** The choices for duplicating `groupId`, empty when none of its observations has to ask. */
  def sequenceCopyChoices(groupId: Group.Id): List[CloneSequenceMode] =
    ObservationsAndGroups.mergeSequenceCopyChoices:
      observationsUnder(groupId).filterNot(_.isCalibration).map(_.execution.sequenceCopyChoices)

  def addClone(clone: GroupClone): ObservationsAndGroups =
    copy(
      observations = observations ++ clone.observations.map(o => o.id -> o),
      groups = groups ++ clone.groups.map(g => g.id -> g)
    )

  def removeClone(ids: GroupClone.Ids): ObservationsAndGroups =
    copy(observations = observations -- ids.observations, groups = groups -- ids.groups)

  /** The clone with these ids as it is now, `None` when its root is absent. */
  def cloneWithIds(ids: GroupClone.Ids): Option[GroupClone] =
    groups
      .get(ids.root)
      .map: root =>
        GroupClone(root,
                   ids.subgroups.flatMap(groups.get),
                   ids.observations.flatMap(observations.get)
        )

object ObservationsAndGroups:
  private val choicesOrder: List[CloneSequenceMode] =
    List(CloneSequenceMode.None, CloneSequenceMode.PendingSteps, CloneSequenceMode.AllSteps)

  /** The distinct union of the choices, ordered None, PendingSteps, AllSteps. */
  def mergeSequenceCopyChoices(choices: List[List[CloneSequenceMode]]): List[CloneSequenceMode] =
    val all: Set[CloneSequenceMode] = choices.flatten.toSet
    choicesOrder.filter(all.contains)

  val observations: Lens[ObservationsAndGroups, ObservationList]       =
    Focus[ObservationsAndGroups](_.observations)
  val groups: Lens[ObservationsAndGroups, GroupList]                   = Focus[ObservationsAndGroups](_.groups)
  val tupled: Iso[ObservationsAndGroups, (ObservationList, GroupList)] =
    Iso[ObservationsAndGroups, (ObservationList, GroupList)](oag => (oag.observations, oag.groups))(
      ObservationsAndGroups.apply
    )

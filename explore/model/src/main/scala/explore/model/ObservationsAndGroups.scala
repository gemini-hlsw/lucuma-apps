// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import monocle.Lens
import monocle.Iso
import monocle.Focus
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.cats.given
import cats.Order.given
import cats.Endo
import cats.syntax.all.*
import lucuma.ui.optics.*

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

object ObservationsAndGroups:
  val observations: Lens[ObservationsAndGroups, ObservationList]       =
    Focus[ObservationsAndGroups](_.observations)
  val groups: Lens[ObservationsAndGroups, GroupList]                   = Focus[ObservationsAndGroups](_.groups)
  val tupled: Iso[ObservationsAndGroups, (ObservationList, GroupList)] =
    Iso[ObservationsAndGroups, (ObservationList, GroupList)](oag => (oag.observations, oag.groups))(
      ObservationsAndGroups.apply
    )

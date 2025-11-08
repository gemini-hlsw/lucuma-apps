// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.all.*
import explore.model.extensions.*
import lucuma.core.data.Zipper
import lucuma.core.enums.TargetDisposition
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.core.model.Tracking
import lucuma.schemas.model.*
import monocle.*

import java.time.Instant

/**
 * Contains a list of targets focused on the selected one on the UI
 */
case class Asterism(private val targets: Zipper[TargetWithId]) derives Eq {
  def toSiderealAt(vizTime: Instant): List[SiderealTargetWithId] =
    targets.traverse(_.toSidereal.map(_.at(vizTime))).foldMap(_.toList)

  def toSidereal: List[SiderealTargetWithId] =
    targets.toNel.toSidereal

  def toSiderealTracking: List[SiderealTracking] =
    targets.traverse(_.toSidereal.map(_.target.tracking)).foldMap(_.toList)

  def asList: List[TargetWithId] = targets.toList

  def asNel: NonEmptyList[TargetWithId] = targets.toNel

  def add(t: TargetWithId): Asterism =
    Asterism.isoTargets.reverse.modify(_ :+ t)(this)

  def ids: NonEmptyList[Target.Id] = targets.toNel.map(_.id)

  def remove(id: Target.Id): Option[Asterism] =
    if (hasId(id)) {
      val filtered = targets.toNel.filter(_.id =!= id)
      Asterism.fromTargets(filtered)
    } else this.some

  def focus = targets.focus

  def focusOn(tid: Target.Id): Asterism =
    targets.findFocus(_.id === tid).map(Asterism.apply).getOrElse(this)

  def baseTracking: Option[Tracking] =
    Tracking.fromAsterism(targets.toNel.map(_.target))

  def hasId(id: Target.Id): Boolean = targets.exists(_.id === id)

  // Find the blind offset target in the zipper
  def blindOffsetTarget: Option[TargetWithId] =
    targets.find(_.disposition == TargetDisposition.BlindOffset)

  // Get just the blind offset target ID
  def blindOffsetTargetId: Option[Target.Id] =
    blindOffsetTarget.map(_.id)

  // Check if a given target ID is the blind offset target
  def isBlindOffsetTarget(tid: Target.Id): Boolean =
    targets.exists(t => t.id == tid && t.disposition == TargetDisposition.BlindOffset)

  // Get all non-blind-offset targets (science + calibration)
  // def nonBlindOffsetTargets: List[TargetWithId] =
  //   targets.toList.filter(_.disposition != TargetDisposition.BlindOffset)

  // Get IDs of non-blind-offset targets
  // def nonBlindOffsetTargetIds: List[Target.Id] =
  //   nonBlindOffsetTargets.map(_.id)
}

object Asterism {
  val isoTargets: Iso[NonEmptyList[TargetWithId], Asterism] =
    Iso[Asterism, NonEmptyList[TargetWithId]](_.targets.toNel)(s =>
      Asterism(Zipper.fromNel(s))
    ).reverse

  val targetsEach: Traversal[Asterism, TargetWithId] = isoTargets.reverse.each

  val targets: Lens[Asterism, Zipper[TargetWithId]] = Focus[Asterism](_.targets)

  val focus: Lens[Asterism, TargetWithId] = targets.andThen(Zipper.focus)

  def fromTargets(targets: List[TargetWithId]): Option[Asterism] =
    NonEmptyList.fromList(targets).map(s => Asterism(Zipper.fromNel(s)))

  val siderealTargetsEach: Traversal[Asterism, SiderealTargetWithId] =
    targetsEach.andThen(TargetWithId.sidereal)

  val fromTargetsList: Iso[List[TargetWithId], Option[Asterism]] =
    Iso[List[TargetWithId], Option[Asterism]](fromTargets) {
      case Some(Asterism(targets)) => targets.toList
      case _                       => Nil
    }

  def fromTargetsListOn(id: Option[Target.Id]): Iso[List[TargetWithId], Option[Asterism]] =
    Iso[List[TargetWithId], Option[Asterism]]((tl: List[TargetWithId]) =>
      fromTargets(tl).flatMap(a => id.map(a.focusOn).orElse(a.some))
    ) {
      case Some(Asterism(targets)) => targets.toList
      case _                       => Nil
    }

  def one(targets: TargetWithId): Asterism =
    Asterism(Zipper.of(targets))

  def targetOptional(targetId: Target.Id): Optional[Option[Asterism], TargetWithId] =
    Optional[Option[Asterism], TargetWithId](
      _.flatMap(_.targets.find(_.id === targetId))
    )(target =>
      _.map(
        Asterism.targetsEach.modify(twid => if (twid.id === targetId) target else twid)
      )
    )

  def fromIdsAndTargets(ids: AsterismIds, targets: TargetList): Option[Asterism] =
    fromTargets(ids.toList.map(id => targets.get(id)).flattenOption)

}

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.all.*
import explore.model.RegionOrTrackingMap.*
import lucuma.core.data.Zipper
import lucuma.core.enums.TargetDisposition
import lucuma.core.model.Target
import lucuma.core.model.Tracking
import lucuma.schemas.model.*

import scala.collection.immutable.SortedSet

/**
 * Contains a list of targets focused on the selected one on the UI
 */
case class ObservationTargets(private val targets: Zipper[TargetWithId]) derives Eq {

  val allTargets = targets.toNel

  // science targets (science + calibration)
  val science: List[TargetWithId] =
    allTargets.filter(_.disposition != TargetDisposition.BlindOffset)

  val length: Int = targets.length

  lazy val hasTargetOfOpportunity: Boolean =
    science.collect { case TargetWithId(_, Target.Opportunity(_, _, _), _, _) => true }.nonEmpty

  // checks if the science targets are of different kind
  def isMixed: Boolean =
    science
      .map {
        _.target match
          case Target.Sidereal(_, _, _, _) => 0
          case Target.Nonsidereal(_, _, _) => 1
          case Target.Opportunity(_, _, _) => 2
      }
      .distinct
      .size > 1

  def ids: NonEmptyList[Target.Id] = allTargets.map(_.id)

  def focus = targets.focus

  def focusOn(tid: Target.Id): ObservationTargets =
    targets.findFocus(_.id === tid).map(ObservationTargets.apply).getOrElse(this)

  // Will return a Left[String] if there are any ToOs
  def asterismTracking(
    trackingMap: RegionOrTrackingMap
  ): Option[Either[String, Tracking]] =
    NonEmptyList
      .fromList(science)
      .map: nel =>
        trackingMap.trackingFor(nel.map(_.id))

  // Will return a None if there are any ToOs
  def optAsterismTracking(trackingMap: RegionOrTrackingMap): Option[Tracking] =
    asterismTracking(trackingMap).flatMap(_.toOption)

  def mapScience[B](f: TargetWithId => B): List[B] =
    science.map(f)

  def map[B](f: TargetWithId => B): NonEmptyList[B] =
    allTargets.map(f)

  // all blind offset targets
  def blindOffsetTargets: List[TargetWithId] =
    allTargets.filter(_.disposition === TargetDisposition.BlindOffset)

  // in reality, there will be only one blind offset, so we'll pick the "first"
  def blindOffset: Option[TargetWithId] = blindOffsetTargets.headOption
}

object ObservationTargets:

  def fromTargets(targets: List[TargetWithId]): Option[ObservationTargets] =
    NonEmptyList.fromList(targets).map(s => ObservationTargets(Zipper.fromNel(s)))

  def one(targets: TargetWithId): ObservationTargets =
    ObservationTargets(Zipper.of(targets))

  def fromIdsAndTargets(
    ids:     SortedSet[Target.Id],
    targets: TargetList
  ): Option[ObservationTargets] =
    fromTargets(ids.toList.map(id => targets.get(id)).flattenOption)

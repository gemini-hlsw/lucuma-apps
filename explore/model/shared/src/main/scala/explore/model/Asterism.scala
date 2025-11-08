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
import lucuma.core.model.Target
import lucuma.core.model.Tracking
import lucuma.schemas.model.*

import java.time.Instant
import lucuma.core.math.Coordinates
import lucuma.core.math.Region

/**
 * Contains a list of targets focused on the selected one on the UI
 */
case class Asterism(private val targets: Zipper[TargetWithId]) derives Eq {

  private val allTargets = targets.toNel

  // science targets (science + calibration)
  private val science: List[TargetWithId] =
    allTargets.filter(_.disposition != TargetDisposition.BlindOffset)

  // This uses ObjectTracking.orRegionFromAsterism, which treats any asterism with a
  // ToO as a ToO and returns the region of the first ToO it finds. Since we "shouldn't"
  // have asterisms with multiple TtargetstargetsoOs, this is probably fine.
  def coordsOrRegionAt(vizTime: Option[Instant]): Option[Either[Coordinates, Region]] =
    NonEmptyList
      .fromList(science.map(_.target))
      .flatMap: science =>
        Tracking
          .orRegionFromAsterism(science) match
          case Left(tracking) =>
            vizTime.fold(tracking.baseCoordinates.asLeft.some)(v => tracking.at(v).map(_.asLeft))
          case Right(region)  => region.asRight.some

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

  def focusOn(tid: Target.Id): Asterism =
    targets.findFocus(_.id === tid).map(Asterism.apply).getOrElse(this)

  // Tracking of the base of science, don't consider blind offsets
  def baseTracking: Option[Tracking] =
    NonEmptyList.fromList(science.map(_.target)).flatMap(Tracking.fromAsterism)

  def mapScience[B](f: TargetWithId => B): List[B] =
    science.map(f)

  def map[B](f: TargetWithId => B): NonEmptyList[B] =
    allTargets.map(f)

  // all blind offset targets
  def blindOffsetTargets: List[TargetWithId] =
    allTargets.filter(_.disposition === TargetDisposition.BlindOffset)
}

object Asterism:

  def fromTargets(targets: List[TargetWithId]): Option[Asterism] =
    NonEmptyList.fromList(targets).map(s => Asterism(Zipper.fromNel(s)))

  def one(targets: TargetWithId): Asterism =
    Asterism(Zipper.of(targets))

  def fromIdsAndTargets(ids: AsterismIds, targets: TargetList): Option[Asterism] =
    fromTargets(ids.toList.map(id => targets.get(id)).flattenOption)

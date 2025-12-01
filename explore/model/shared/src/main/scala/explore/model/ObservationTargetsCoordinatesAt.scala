// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import explore.model.extensions.*
import lucuma.core.math.Coordinates
import lucuma.core.math.Epoch
import lucuma.core.model.Target
import lucuma.core.model.Tracking
import lucuma.schemas.model.syntax.*

import java.time.Instant

// The Coordidinates of the observation targets at a given epoch
final case class ObservationTargetsCoordinatesAt(
  at:                Epoch,
  baseCoords:        Option[Coordinates],
  blindOffsetCoords: Option[Coordinates],
  allTargetsMap:     Map[Target.Id, Coordinates],
  scienceTargetsMap: Map[Target.Id, Coordinates]
) derives Eq:
  val scienceCoords: List[Coordinates]       = scienceTargetsMap.values.toList
  // We really should have one of these two things...
  val baseOrBlindCoords: Option[Coordinates] = baseCoords.orElse(blindOffsetCoords)

  def forTarget(id: Target.Id): Option[Coordinates] = allTargetsMap.get(id)

object ObservationTargetsCoordinatesAt:
  def emptyAt(at: Instant): Either[String, ObservationTargetsCoordinatesAt] =
    Epoch.Julian
      .fromInstant(at)
      .toRight(s"Invalid epoch: $at")
      .map: epoch =>
        ObservationTargetsCoordinatesAt(epoch, None, None, Map.empty, Map.empty)

  def fromTargetsAndTracking(
    at:          Instant,
    obsTargets:  ObservationTargets,
    trackingMap: Map[Target.Id, Tracking]
  ): Either[String, ObservationTargetsCoordinatesAt] =
    val eEpoch: Either[String, Epoch]                            = Epoch.Julian.fromInstant(at).toRight(s"Invalid epoch: $at")
    val eBase: Either[String, Option[Coordinates]]               =
      obsTargets
        .asterismTracking(trackingMap)
        .traverse(_.flatMap(_.coordinatesAt(at).map(_.coordinates)))
    val eScienceMap: Either[String, Map[Target.Id, Coordinates]] =
      obsTargets
        .mapScience(t => t.at(at, trackingMap).map(ca => (t.id, ca.coordinates)))
        .sequence
        .map(_.toMap)
    val eBlindTuple: Either[String, Option[(Target.Id, Coordinates)]] =
      // only consider the first blind offset, since there can in reality be only one.
      obsTargets.blindOffsetTargets.headOption.traverse(t =>
        t.at(at, trackingMap).map(ca => (t.id, ca.coordinates))
      )
    (eEpoch, eBase, eScienceMap, eBlindTuple).mapN: (epoch, base, scienceMap, blindTuple) =>
      val allMap = blindTuple.fold(scienceMap)(scienceMap + _)
      ObservationTargetsCoordinatesAt(epoch, base, blindTuple.map(_._2), allMap, scienceMap)

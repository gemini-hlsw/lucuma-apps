// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.all.*
import explore.model.ErrorMsgOr
import explore.model.RegionOrTrackingMap.*
import lucuma.core.math.Coordinates
import lucuma.core.math.Epoch
import lucuma.core.model.Target
import lucuma.schemas.model.InstrumentSlot
import lucuma.schemas.model.SlotId

import java.time.Instant

// The Coordinates of the observation targets at a given epoch.
// This class is useful if ToOs are not being considered - as for visualization.
final case class ObservationTargetsCoordinatesAt(
  at:                Epoch,
  baseCoords:        Option[Coordinates],
  blindOffsetCoords: Option[Coordinates],
  allTargetsMap:     Map[Target.Id, Coordinates],
  scienceTargetsMap: Map[Target.Id, Coordinates],
  slotCoords:        Map[SlotId, Coordinates],
  skyCoords:         List[Coordinates]
) derives Eq:
  val scienceCoords: List[Coordinates]       = scienceTargetsMap.values.toList
  // We really should have one of these two things...
  val baseOrBlindCoords: Option[Coordinates] = baseCoords.orElse(blindOffsetCoords)

  def forTarget(id: Target.Id): Option[Coordinates] = allTargetsMap.get(id)

object ObservationTargetsCoordinatesAt:
  def emptyAt(at: Instant): ErrorMsgOr[ObservationTargetsCoordinatesAt] =
    Epoch.Julian
      .fromInstant(at)
      .toRight(s"Invalid epoch: $at")
      .map: epoch =>
        ObservationTargetsCoordinatesAt(epoch, None, None, Map.empty, Map.empty, Map.empty, Nil)

  // Will return a Left[String] if there are any ToOs
  def apply(
    at:          Instant,
    obsTargets:  ObservationTargets,
    trackingMap: RegionOrTrackingMap,
    slots:       List[InstrumentSlot]
  ): ErrorMsgOr[ObservationTargetsCoordinatesAt] =
    val eEpoch: ErrorMsgOr[Epoch] =
      Epoch.Julian.fromInstant(at).toRight(s"Invalid epoch: $at")

    val eScienceMap: ErrorMsgOr[Map[Target.Id, Coordinates]] =
      obsTargets
        .mapScience(t => trackingMap.coordinatesForAt(t.id, at).map(ca => (t.id, ca.coordinates)))
        .sequence
        .map(_.toMap)

    val eBlindTuple: ErrorMsgOr[Option[(Target.Id, Coordinates)]] =
      obsTargets.blindOffset.traverse(t =>
        trackingMap.coordinatesForAt(t.id, at).map(ca => (t.id, ca.coordinates))
      )
    (eEpoch, eScienceMap, eBlindTuple).mapN: (epoch, scienceMap, blindTuple) =>
      val allMap                                = blindTuple.fold(scienceMap)(scienceMap + _)
      val base                                  = NonEmptyList
        .fromList(scienceMap.values.toList)
        .map(Coordinates.centerOf)
      val slotCoords: Map[SlotId, Coordinates] =
        slots.flatMap {
          case InstrumentSlot.Science(tid, sid) => allMap.get(tid).map(sid -> _)
          case InstrumentSlot.Sky(c, sid)       => Some(sid -> c)
        }.toMap
      val skyCoords: List[Coordinates]                   =
        slots.collect { case InstrumentSlot.Sky(c, _) => c }
      ObservationTargetsCoordinatesAt(epoch,
                                      base,
                                      blindTuple.map(_._2),
                                      allMap,
                                      scienceMap,
                                      slotCoords,
                                      skyCoords
      )

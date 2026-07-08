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

case class SlotInfo(coordinates: Coordinates, targetId: Option[Target.Id]) derives Eq

// The Coordinates of the observation targets at a given epoch.
// This class is useful if ToOs are not being considered - as for visualization.
final case class ObservationTargetsCoordinatesAt(
  at:                Epoch,
  baseCoords:        Option[Coordinates],
  blindOffsetCoords: Option[Coordinates],
  allTargetsMap:     Map[Target.Id, Coordinates],
  scienceTargetsMap: Map[Target.Id, Coordinates],
  slots:             Map[SlotId, SlotInfo]
) derives Eq:
  val scienceCoords: List[Coordinates]       = scienceTargetsMap.values.toList
  // We really should have one of these two things...
  val baseOrBlindCoords: Option[Coordinates] = baseCoords.orElse(blindOffsetCoords)

  // Coordinates of each IFU slot
  val slotCoords: Map[SlotId, Coordinates] = slots.view.mapValues(_.coordinates).toMap

  // Sky-position slots have no associated target, paired with their slot id.
  val skySlots: List[(SlotId, Coordinates)] =
    slots.toList
      .collect:
        case (sid, SlotInfo(c, None)) => sid -> c

  // Sky-position coordinates only (used by AGS).
  val skyCoords: List[Coordinates] = skySlots.map(_._2)

  def forTarget(id: Target.Id): Option[Coordinates] = allTargetsMap.get(id)

  def slotForTarget(id: Target.Id): Option[SlotId] =
    slots.collectFirst:
      case (sid, info) if info.targetId.contains(id) => sid

object ObservationTargetsCoordinatesAt:
  def emptyAt(at: Instant): ErrorMsgOr[ObservationTargetsCoordinatesAt] =
    Epoch.Julian
      .fromInstant(at)
      .toRight(s"Invalid epoch: $at")
      .map: epoch =>
        ObservationTargetsCoordinatesAt(epoch, None, None, Map.empty, Map.empty, Map.empty)

  // Will return a Left[String] if there are any ToOs
  def apply(
    at:           Instant,
    obsTargets:   ObservationTargets,
    trackingMap:  RegionOrTrackingMap,
    slots:        List[InstrumentSlot],
    explicitBase: Option[Coordinates] = None
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
      val allMap = blindTuple.fold(scienceMap)(scienceMap + _)

      // Prefer the explicit base position if reported
      val base = explicitBase.orElse(
        NonEmptyList.fromList(scienceMap.values.toList).map(Coordinates.centerOf)
      )

      val slotsMap =
        slots.flatMap:
          case InstrumentSlot.Science(tid, sid) =>
            allMap.get(tid).map(c => sid -> SlotInfo(c, tid.some))
          case InstrumentSlot.Sky(c, sid)       =>
            (sid -> SlotInfo(c, none)).some

      ObservationTargetsCoordinatesAt(epoch,
                                      base,
                                      blindTuple.map(_._2),
                                      allMap,
                                      scienceMap,
                                      slotsMap.toMap
      )

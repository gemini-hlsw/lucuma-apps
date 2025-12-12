// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Order.*
import cats.data.NonEmptyList
import explore.model.RegionOrTracking.*
import lucuma.core.math.Region
import lucuma.core.model.CompositeTracking
import lucuma.core.model.Target
import lucuma.core.model.Tracking
import lucuma.schemas.model.CoordinatesAt

import java.time.Instant
import scala.collection.immutable.SortedMap

type RegionOrTrackingMap = SortedMap[Target.Id, RegionOrTracking]

object RegionOrTrackingMap:
  val Empty: RegionOrTrackingMap = SortedMap.empty

  def from(it: IterableOnce[(Target.Id, RegionOrTracking)]): RegionOrTrackingMap =
    SortedMap.from(it)

  private def compositeRegionOrTracking(torRs: NonEmptyList[RegionOrTracking]): RegionOrTracking =
    torRs
      .collect:
        case Left(r) => r
      .headOption
      .toLeft:
        torRs
          .traverse(_.toTracking)
          .getOrElse(sys.error("unpossible, list should only contain trackings")) match
          case NonEmptyList(t, Nil) => t
          case NonEmptyList(h, t)   => CompositeTracking(NonEmptyList(h, t))

  private def compositeRegionOrTracking(torRs: List[RegionOrTracking]): Option[RegionOrTracking] =
    NonEmptyList.fromList(torRs).map(compositeRegionOrTracking)

  extension (trm: RegionOrTrackingMap)
    inline def getFor(id: Target.Id): Either[String, RegionOrTracking] =
      trm.get(id).toRight(s"No tracking or region result for target $id")

    inline def regionOrCoordinatesAtFor(
      id: Target.Id,
      at: Instant
    ): Either[String, Either[Region, CoordinatesAt]] =
      getFor(id).flatMap(_.regionOrCoordinatesAt(at))

    inline def regionOrTrackingFor(ids: NonEmptyList[Target.Id]): Either[String, RegionOrTracking] =
      ids.traverse(id => trm.getFor(id)).map(compositeRegionOrTracking)

    inline def regionOrCoordinatesAtFor(
      ids: NonEmptyList[Target.Id],
      at:  Instant
    ): Either[String, Either[Region, CoordinatesAt]] =
      regionOrTrackingFor(ids).flatMap(_.regionOrCoordinatesAt(at))

    // if any of the targets has a region, the result is a region
    inline def regionOrTrackingForAll: Option[RegionOrTracking] =
      compositeRegionOrTracking(trm.values.toList)

    inline def regionOrCoordinateAtForAll(
      at: Instant
    ): Option[Either[String, Either[Region, CoordinatesAt]]] =
      regionOrTrackingForAll.map(_.regionOrCoordinatesAt(at))

    inline def trackingFor(id: Target.Id): Either[String, Tracking] =
      getFor(id).flatMap(_.toTracking)

    inline def trackingFor(ids: NonEmptyList[Target.Id]): Either[String, Tracking] =
      regionOrTrackingFor(ids).flatMap(_.toTracking)

    inline def coordinatesForAt(id: Target.Id, at: Instant): Either[String, CoordinatesAt] =
      getFor(id).flatMap(_.coordinatesForTrackingAt(at))

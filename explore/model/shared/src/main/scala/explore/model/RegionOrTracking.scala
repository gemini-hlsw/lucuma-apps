// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import lucuma.core.math.Region
import lucuma.core.model.Tracking
import lucuma.schemas.model.CoordinatesAt
import lucuma.schemas.model.syntax.*

import java.time.Instant

type RegionOrTracking = Either[Region, Tracking]

object RegionOrTracking:
  def fromTracking(t: Tracking): RegionOrTracking = t.asRight

  def fromRegion(r: Region): RegionOrTracking = r.asLeft

  extension (torR: RegionOrTracking)
    // If being a region is an error for your use case
    def toTracking: Either[String, Tracking] =
      torR.left.map(_ => "Targets of Opportunities have no coordinates")

    def toRegion: Option[Region] =
      // errors really only happen for ephemeris failures, so we can ignore them here
      torR.left.toOption

    def coordinatesForTrackingAt(at: Instant): Either[String, CoordinatesAt] =
      toTracking.flatMap(_.coordinatesAt(at))

    def regionOrCoordinatesAt(at: Instant): ErrorOrRegionOrCoords =
      torR.fold(_.asLeft.asRight, _.coordinatesAt(at).map(_.asRight))

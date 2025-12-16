// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import lucuma.core.math.Region
import lucuma.core.model.Target
import lucuma.schemas.model.CoordinatesAt

type RegionOrCoordinatesAt = Either[Region, CoordinatesAt]

type ErrorOrRegionOrCoords = Either[String, RegionOrCoordinatesAt]

object ErrorOrRegionOrCoords:
  def fromRegion(region: Region): ErrorOrRegionOrCoords =
    region.asLeft.asRight

  def fromCoordinatesAt(coords: CoordinatesAt): ErrorOrRegionOrCoords =
    coords.asRight.asRight

  def siderealBaseCoords(sidereal: Target.Sidereal): ErrorOrRegionOrCoords =
    fromCoordinatesAt(
      CoordinatesAt(sidereal.tracking.epoch.toInstant, sidereal.tracking.baseCoordinates)
    )

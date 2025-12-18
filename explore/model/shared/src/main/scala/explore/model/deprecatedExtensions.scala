// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import lucuma.core.math.Coordinates
import lucuma.core.model.CompositeTracking
import lucuma.core.model.ConstantTracking
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Tracking

// everything here will be removed when non-sidereal support is complete
object deprecatedExtensions:
  extension (tracking: Tracking)
    // see lucuma.schemas.model.syntax.baseCoordinates for replacement
    def baseCoordinatesDeprecated: Coordinates = tracking match
      case SiderealTracking(baseCoordinates, _, _, _, _) => baseCoordinates
      case ConstantTracking(coordinates)                 => coordinates
      case CompositeTracking(nel)                        => Coordinates.centerOf(nel.map(_.baseCoordinatesDeprecated))
      case _                                             => sys.error("Non sidereals are not supported")

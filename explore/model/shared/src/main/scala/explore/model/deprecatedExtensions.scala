// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import lucuma.core.math.Coordinates
import lucuma.core.math.Epoch
import lucuma.core.model.CompositeTracking
import lucuma.core.model.ConstantTracking
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.core.model.Tracking
import lucuma.schemas.model.TargetWithId

import java.time.Instant

// everything here will be removed when non-sidereal support is complete
object deprecatedExtensions:
  extension (target: Target.Sidereal)
    def at(i: Instant): Target.Sidereal = {
      val epoch          = Epoch.Julian.fromInstant(i).getOrElse(target.tracking.epoch)
      val trackingUpdate = (tracking: SiderealTracking) =>
        tracking.at(i).fold(tracking) { c =>
          val update = SiderealTracking.baseCoordinates.replace(c) >>> SiderealTracking.epoch
            .replace(epoch)
          update(tracking)
        }

      Target.Sidereal.tracking.modify(trackingUpdate)(target)
    }

  extension (target: Target)
    // If the target is sidereal, update it to the given instant.
    def atDeprecated(i: Instant): Target = target match
      case st @ Target.Sidereal(_, _, _, _) => st.at(i)
      case Target.Nonsidereal(_, _, _)      => target
      case Target.Opportunity(_, _, _)      => target

  extension (targetWithId: TargetWithId)
    def atDeprecated(i: Instant): TargetWithId =
      TargetWithId.target.replace(targetWithId.target.atDeprecated(i))(targetWithId)

  extension (tracking: Tracking)
    // see lucuma.schemas.model.syntax.baseCoordinates for replacement
    def baseCoordinatesDeprecated: Coordinates = tracking match
      case SiderealTracking(baseCoordinates, _, _, _, _) => baseCoordinates
      case ConstantTracking(coordinates)                 => coordinates
      case CompositeTracking(nel)                        => Coordinates.centerOf(nel.map(_.baseCoordinatesDeprecated))
      case _                                             => sys.error("Non sidereals are not supported")

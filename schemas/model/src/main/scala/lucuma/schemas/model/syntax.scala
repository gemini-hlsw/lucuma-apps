// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model

import cats.syntax.all.*
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Coordinates
import lucuma.core.math.Epoch
import lucuma.core.model.CompositeTracking
import lucuma.core.model.ConstantTracking
import lucuma.core.model.EphemerisTracking
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Tracking
import lucuma.core.util.Timestamp
import org.typelevel.cats.time.*

import java.time.Instant

object syntax:
  extension (tracking: Tracking)
    // None means there are no valid times (empty ephemeris)
    def validBounds: Option[BoundedInterval[Instant]] = tracking match
      case CompositeTracking(trackings) =>
        trackings.tail.foldLeft(trackings.head.validBounds)((acc, tr) =>
          (acc, tr.validBounds).flatMapN((b1, b2) =>
            if (b1.intersects(b2) || b1.abuts(b2)) b1.union(b2).some else none
          )
        )
      case e: EphemerisTracking         =>
        (e.first, e.last).flatMapN((f, l) => BoundedInterval.closed(f._1.toInstant, l._1.toInstant))
      case s: SiderealTracking          =>
        BoundedInterval.closed(Epoch.MinValue.toInstant, Epoch.MaxValue.toInstant)
      case c: ConstantTracking          => BoundedInterval.closed(Instant.MIN, Instant.MAX)

    def isValidAt(at: Instant): Either[String, Boolean] =
      validBounds.fold(s"No ephemeris data for $at".asLeft)(_.contains(at).asRight)

    def coordinatesAt(at: Instant): Either[String, CoordinatesAt] =
      isValidAt(at).flatMap(isValid =>
        if (isValid)
          // this shouldn't fail...
          tracking.at(at).map(CoordinatesAt(at, _)).toRight(s"Unable to get coordinates at $at")
        else s"Time $at out of range for tracking".asLeft
      )

    def baseCoordinates: Either[String, Coordinates] = tracking match
      case ConstantTracking(coordinates)                 => coordinates.asRight
      case CompositeTracking(nel)                        => nel.traverse(_.baseCoordinates).map(Coordinates.centerOf(_))
      case EphemerisTracking(toMap)                      => "Non-sidereal targets don't have base coordinates.".asLeft
      case SiderealTracking(baseCoordinates, _, _, _, _) => baseCoordinates.asRight

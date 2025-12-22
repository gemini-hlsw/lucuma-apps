// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model

import cats.Eq
import cats.derived.*
import lucuma.core.math.Coordinates
import lucuma.core.math.Epoch
import org.typelevel.cats.time.given

import java.time.Instant

case class CoordinatesAt(at: Instant, coordinates: Coordinates) derives Eq:
  lazy val epoch: Option[Epoch] = Epoch.Julian.fromInstant(at)

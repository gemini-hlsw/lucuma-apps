// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model.arb

import lucuma.core.math.Coordinates
import lucuma.core.math.arb.ArbCoordinates.given
import lucuma.schemas.model.CoordinatesAt
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

import java.time.Instant

trait ArbCoordinatesAt:
  given Arbitrary[CoordinatesAt] =
    Arbitrary:
      for
        at     <- arbitrary[Instant]
        coords <- arbitrary[Coordinates]
      yield CoordinatesAt(at, coords)

  given Cogen[CoordinatesAt] =
    Cogen[(Instant, Coordinates)].contramap(ca => (ca.at, ca.coordinates))

object ArbCoordinatesAt extends ArbCoordinatesAt

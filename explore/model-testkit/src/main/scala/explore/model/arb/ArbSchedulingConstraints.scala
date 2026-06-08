// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import cats.Order.given
import explore.model.SchedulingConstraints
import lucuma.core.model.TimingWindow
import lucuma.core.model.arb.ArbTimingWindow.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

trait ArbSchedulingConstraints:
  given Arbitrary[SchedulingConstraints] = Arbitrary(
    for {
      isSplittable  <- arbitrary[Boolean]
      timingWindows <- arbitrary[List[TimingWindow]]
    } yield SchedulingConstraints(isSplittable, timingWindows.sorted)
  )

  given Cogen[SchedulingConstraints] = Cogen[
    (Boolean, List[TimingWindow])
  ].contramap(sc => (sc.isSplittable, sc.timingWindows))

object ArbSchedulingConstraints extends ArbSchedulingConstraints

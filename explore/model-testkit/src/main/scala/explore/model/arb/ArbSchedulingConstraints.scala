// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import cats.Order.given
import explore.model.SchedulingConstraints
import lucuma.core.enums.SchedulingMode
import lucuma.core.model.TimingWindow
import lucuma.core.model.arb.ArbTimingWindow.given
import lucuma.core.util.arb.ArbEnumerated.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

trait ArbSchedulingConstraints:
  given Arbitrary[SchedulingConstraints] = Arbitrary(
    for {
      schedulingMode <- arbitrary[SchedulingMode]
      timingWindows  <- arbitrary[List[TimingWindow]]
    } yield SchedulingConstraints(schedulingMode, timingWindows.sorted)
  )

  given Cogen[SchedulingConstraints] = Cogen[
    (SchedulingMode, List[TimingWindow])
  ].contramap(sc => (sc.schedulingMode, sc.timingWindows))

object ArbSchedulingConstraints extends ArbSchedulingConstraints

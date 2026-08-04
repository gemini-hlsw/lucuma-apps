// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import cats.Order.given
import explore.model.SchedulingConstraints
import lucuma.core.enums.ExecutionRequirement
import lucuma.core.model.TimingWindow
import lucuma.core.model.arb.ArbTimingWindow.given
import lucuma.core.util.arb.ArbEnumerated.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

trait ArbSchedulingConstraints:
  given Arbitrary[SchedulingConstraints] = Arbitrary(
    for {
      executionRequirement <- arbitrary[ExecutionRequirement]
      timingWindows        <- arbitrary[List[TimingWindow]]
    } yield SchedulingConstraints(executionRequirement, timingWindows.sorted)
  )

  given Cogen[SchedulingConstraints] = Cogen[
    (ExecutionRequirement, List[TimingWindow])
  ].contramap(sc => (sc.executionRequirement, sc.timingWindows))

object ArbSchedulingConstraints extends ArbSchedulingConstraints

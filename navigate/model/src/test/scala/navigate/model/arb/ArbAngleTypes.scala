// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model.arb

import navigate.model.AzimuthAngle
import navigate.model.RotatorAngle
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

trait ArbAngleTypes {
  given Arbitrary[AzimuthAngle] =
    Arbitrary[AzimuthAngle](arbitrary[Long].map(AzimuthAngle.fromMicroarcseconds))
  given Arbitrary[RotatorAngle] =
    Arbitrary[RotatorAngle](arbitrary[Long].map(RotatorAngle.fromMicroarcseconds))
}

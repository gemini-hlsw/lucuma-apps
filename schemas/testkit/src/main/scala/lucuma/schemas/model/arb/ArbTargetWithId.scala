// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model.arb

import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.TargetDisposition
import lucuma.core.model.Target
import lucuma.core.model.arb.ArbTarget.given
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbGid.given
import lucuma.schemas.model.*
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbTargetWithId:
  given Arbitrary[TargetWithId] = Arbitrary {
    for {
      id <- arbitrary[Target.Id]
      t  <- arbitrary[Target]
      d  <- arbitrary[TargetDisposition]
      cr <- arbitrary[Option[CalibrationRole]]
    } yield TargetWithId(id, t, d, cr)
  }

  given Cogen[TargetWithId] =
    Cogen[(Target.Id, Target, TargetDisposition, Option[CalibrationRole])].contramap(x =>
      (x.id, x.target, x.disposition, x.calibrationRole)
    )

object ArbTargetWithId extends ArbTargetWithId

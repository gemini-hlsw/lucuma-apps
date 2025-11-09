// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import cats.data.NonEmptyList
import cats.laws.discipline.arbitrary.*
import explore.model.ObservationTargets
import lucuma.core.data.Zipper
import lucuma.schemas.model.*
import lucuma.schemas.model.arb.ArbTargetWithId.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbObservationTargets:
  given arbObservationTargets: Arbitrary[ObservationTargets] = Arbitrary {
    arbitrary[NonEmptyList[TargetWithId]].map(n => ObservationTargets(Zipper.fromNel(n)))
  }

  given Cogen[ObservationTargets] =
    Cogen[List[TargetWithId]].contramap(_.map(identity).toList)

object ArbObservationTargets extends ArbObservationTargets

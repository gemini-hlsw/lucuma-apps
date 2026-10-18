// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.input

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.*
import org.typelevel.discipline.Laws

object InputCodecTests extends Laws:
  def apply[A: Arbitrary](format: InputCodec[A]): RuleSet =
    val laws: InputCodecLaws[A] = InputCodecLaws(format)
    new SimpleRuleSet(
      "InputCodec",
      "canonical text is valid for the mask" -> forAll((a: A) => laws.canonicalIsValidForMask(a)),
      "canonical text round trips"           -> forAll((a: A) => laws.canonicalRoundTrips(a))
    )

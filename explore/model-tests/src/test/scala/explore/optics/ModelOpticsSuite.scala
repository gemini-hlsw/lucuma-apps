// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.optics

import eu.timepit.refined.cats.given
import eu.timepit.refined.scalacheck.all.given
import explore.model.GeminiProposalType
import explore.model.arb.ArbPartnerSplit.given
import explore.model.arb.ArbProposalType.given
import explore.optics.ModelOptics.*
import lucuma.core.math.arb.ArbRadialVelocity
import lucuma.core.model.arb.ArbTarget
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbTimeSpan.given
import monocle.law.discipline.OptionalTests
import munit.DisciplineSuite
import org.scalacheck.Arbitrary.*

class ModelOpticsSuite extends DisciplineSuite:
  import ArbRadialVelocity.given
  import ArbTarget.given

  checkAll("targetRV", OptionalTests(TargetRV))
  checkAll("toOActivation", OptionalTests(GeminiProposalType.toOActivation))
  checkAll("partnerSplits", OptionalTests(GeminiProposalType.partnerSplits))
  checkAll("minPercentTime", OptionalTests(GeminiProposalType.minPercentTime))
  checkAll("totalTime", OptionalTests(GeminiProposalType.totalTime))

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence.arb

import lucuma.core.model.Visit
import lucuma.core.model.sequence.Step
import lucuma.core.util.arb.ArbGid.given
import lucuma.core.util.arb.ArbUid.given
import lucuma.ui.sequence.SelectedRowId
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

trait ArbSelectedRowId:
  given Arbitrary[SelectedRowId] = Arbitrary:
    for
      visitId <- arbitrary[Option[Visit.Id]]
      stepId  <- arbitrary[Step.Id]
    yield SelectedRowId(visitId, stepId)

  given Cogen[SelectedRowId] = Cogen[(Option[Visit.Id], Step.Id)].contramap: selectedRowId =>
    (selectedRowId.visitId, selectedRowId.stepId)

object ArbSelectedRowId extends ArbSelectedRowId

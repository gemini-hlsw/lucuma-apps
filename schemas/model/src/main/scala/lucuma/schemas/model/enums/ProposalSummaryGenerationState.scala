// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.model.enums

import lucuma.core.util.Enumerated

enum ProposalSummaryGenerationState(val tag: String) derives Enumerated:
  case Idle    extends ProposalSummaryGenerationState("IDLE")
  case Pending extends ProposalSummaryGenerationState("PENDING")
  case Failed  extends ProposalSummaryGenerationState("FAILED")

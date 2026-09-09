// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*

/**
 * A program's attachments and the state of its Proposal Summary regeneration.
 */
case class ProgramAttachments(
  attachments:               List[Attachment],
  proposalSummaryGeneration: ProposalSummaryGeneration
) derives Eq

object ProgramAttachments:
  val Empty: ProgramAttachments = ProgramAttachments(Nil, ProposalSummaryGeneration.Idle)

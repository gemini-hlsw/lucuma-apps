// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.kernel.Order.catsKernelOrderingForOrder
import cats.syntax.all.*
import lucuma.core.enums.Partner

/**
 * The Proposal Summaries among a program's attachments. Progress and failure of a Regenerate come
 * from the ODB in [[ProposalSummaryGeneration]].
 */
object ProposalSummaries:

  /**
   * The Proposal Summaries the proposal currently calls for: one per partner it requests time from,
   * or a single partnerless one when it requests time from none.
   */
  def expectedFor(splits: List[PartnerSplit]): Set[Option[Partner]] =
    val requested = splits.filter(_.percent.value > 0).map(_.partner.some).toSet
    if (requested.isEmpty) Set(none) else requested

  /**
   * The Proposal Summaries worth showing.
   */
  def of(attachments: AttachmentList, splits: List[PartnerSplit]): List[Attachment] =
    val expected = expectedFor(splits)
    attachments.values
      .filter(a => a.isProposalSummary && expected.contains(a.summaryPartner))
      .toList
      .sortBy(_.summaryPartner)

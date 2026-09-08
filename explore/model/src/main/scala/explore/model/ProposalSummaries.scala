// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.kernel.Order.catsKernelOrderingForOrder
import cats.syntax.all.*
import lucuma.core.enums.Partner
import lucuma.core.util.Timestamp

/**
 * The Proposal Summaries among a program's attachments and the bookkeeping for a Regenerate
 * request. Rendering is asynchronous with no failure signal, so a request is considered done only
 * when every summary that existed when it was made has been replaced by a newer one.
 */
object ProposalSummaries:

  /**
   * The Proposal Summaries the proposal currently calls for: one per partner it is split between,
   * or a single partnerless one when it has no splits.
   */
  def expectedFor(splits: List[PartnerSplit]): Set[Option[Partner]] =
    if (splits.isEmpty) Set(none) else splits.map(_.partner.some).toSet

  /**
   * The Proposal Summaries worth showing. A summary rendered for a partner the proposal no longer
   * has is stale the moment the splits change, so it is dropped rather than shown until a
   * Regenerate gets around to replacing it.
   */
  def of(attachments: AttachmentList, splits: List[PartnerSplit]): List[Attachment] =
    val expected = expectedFor(splits)
    attachments.values
      .filter(a => a.isProposalSummary && expected.contains(a.summaryPartner))
      .toList
      .sortBy(_.summaryPartner)

  /**
   * A Regenerate request, remembering the summaries present when it was made. Comparing against
   * that snapshot instead of the request time avoids depending on client and server clocks
   * agreeing.
   */
  case class Request(before: Map[Option[Partner], Timestamp]) derives Eq:
    def isPending(att: Attachment): Boolean =
      before.get(att.summaryPartner).exists(_ >= att.updatedAt)

    def anyPending(summaries: List[Attachment]): Boolean =
      summaries.isEmpty || summaries.exists(isPending)

  object Request:
    def apply(summaries: List[Attachment]): Request =
      Request(summaries.map(a => a.summaryPartner -> a.updatedAt).toMap)

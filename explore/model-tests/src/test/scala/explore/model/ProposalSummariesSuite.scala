// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.kernel.Order.catsKernelOrderingForOrder
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.AttachmentType
import lucuma.core.enums.Partner
import lucuma.core.model.IntPercent
import lucuma.core.util.Timestamp
import munit.FunSuite

import java.time.Instant
import scala.collection.immutable.SortedMap

class ProposalSummariesSuite extends FunSuite:
  private val T0 = Timestamp.unsafeFromInstantTruncated(Instant.parse("2026-09-01T00:00:00Z"))
  private val T1 = Timestamp.unsafeFromInstantTruncated(Instant.parse("2026-09-01T00:01:00Z"))
  private val T2 = Timestamp.unsafeFromInstantTruncated(Instant.parse("2026-09-01T00:02:00Z"))

  test("a summary whose partner set changed ends the request once every current row is fresh"):
    val before = list(att(1, AttachmentType.Summary, T0))
    val req    =
      ProposalSummaries.Request(ProposalSummaries.of(before, splits(Partner.CL, Partner.US)))
    val split  = list(
      att(2, AttachmentType.Summary, T2, Partner.US.some),
      att(3, AttachmentType.Summary, T2, Partner.CL.some)
    )
    assert(!req.anyPending(ProposalSummaries.of(split, splits(Partner.CL, Partner.US))))

  private def att(
    id:        Long,
    t:         AttachmentType,
    updatedAt: Timestamp,
    partner:   Option[Partner] = none
  ): (Attachment.Id, Attachment) =
    val aid = Attachment.Id.fromLong(id).get
    aid -> Attachment(
      id = aid,
      attachmentType = t,
      fileName = NonEmptyString.unsafeFrom(s"$id.pdf"),
      mask = none,
      description = none,
      checked = false,
      fileSize = 1L,
      updatedAt = updatedAt,
      proposalSummary = Option.when(t === AttachmentType.Summary)(
        Attachment.ProposalSummary(partner)
      )
    )

  private def list(as: (Attachment.Id, Attachment)*): AttachmentList = SortedMap(as*)

  private def splits(ps: Partner*): List[PartnerSplit] =
    ps.toList.map(p => PartnerSplit(p, IntPercent.unsafeFrom(100 / ps.size)))

  private val noSplits: List[PartnerSplit] = Nil

  test("of keeps only the summaries for the current partners, sorted by partner"):
    val l = list(
      att(1, AttachmentType.Science, T0),
      att(2, AttachmentType.Summary, T0, Partner.US.some),
      att(3, AttachmentType.Summary, T0, Partner.CL.some)
    )
    assertEquals(
      ProposalSummaries.of(l, splits(Partner.CL, Partner.US)).map(_.summaryPartner),
      List(Partner.CL.some, Partner.US.some)
    )

  test("a request with no summaries stays pending until one arrives"):
    val req = ProposalSummaries.Request(Nil)
    assert(req.anyPending(Nil))
    assert(!req.anyPending(List(att(1, AttachmentType.Summary, T0)._2)))

  test("a summary is pending until it is newer than the snapshot for its partner"):
    val before = list(att(1, AttachmentType.Summary, T1, Partner.US.some))
    val req    =
      ProposalSummaries.Request(ProposalSummaries.of(before, splits(Partner.CL, Partner.US)))
    assert(req.isPending(before(Attachment.Id.fromLong(1).get)))
    val same   = att(1, AttachmentType.Summary, T1, Partner.US.some)._2
    assert(req.isPending(same))
    val newer  = att(1, AttachmentType.Summary, T2, Partner.US.some)._2
    assert(!req.isPending(newer))

  test("rows clear independently, request stays pending while any is stale"):
    val before = list(
      att(1, AttachmentType.Summary, T0, Partner.US.some),
      att(2, AttachmentType.Summary, T0, Partner.CL.some)
    )
    val req    =
      ProposalSummaries.Request(ProposalSummaries.of(before, splits(Partner.CL, Partner.US)))
    val half   = list(
      att(1, AttachmentType.Summary, T2, Partner.US.some),
      att(2, AttachmentType.Summary, T0, Partner.CL.some)
    )
    assert(!req.isPending(half(Attachment.Id.fromLong(1).get)))
    assert(req.isPending(half(Attachment.Id.fromLong(2).get)))
    assert(req.anyPending(ProposalSummaries.of(half, splits(Partner.CL, Partner.US))))
    val done   = list(
      att(1, AttachmentType.Summary, T2, Partner.US.some),
      att(3, AttachmentType.Summary, T2, Partner.CL.some)
    )
    assert(!req.anyPending(ProposalSummaries.of(done, splits(Partner.CL, Partner.US))))

  test("a partner not in the snapshot is never pending"):
    val req = ProposalSummaries.Request(List(att(1, AttachmentType.Summary, T0)._2))
    assert(!req.isPending(att(2, AttachmentType.Summary, T0, Partner.AR.some)._2))

  test("only the partners the proposal currently has are shown"):
    val l = list(
      att(1, AttachmentType.Summary, T0, Partner.US.some),
      att(2, AttachmentType.Summary, T0, Partner.CL.some)
    )
    // CL was dropped from the splits, so its summary is stale and hidden right away.
    assertEquals(
      ProposalSummaries.of(l, splits(Partner.US)).map(_.summaryPartner),
      List(Partner.US.some)
    )

  test("a proposal with no splits shows only the partnerless summary"):
    val l = list(
      att(1, AttachmentType.Summary, T0),
      att(2, AttachmentType.Summary, T0, Partner.US.some)
    )
    assertEquals(ProposalSummaries.of(l, noSplits).map(_.summaryPartner), List(none))

  test("a proposal with splits hides a leftover partnerless summary"):
    val l = list(
      att(1, AttachmentType.Summary, T0),
      att(2, AttachmentType.Summary, T0, Partner.US.some)
    )
    assertEquals(
      ProposalSummaries.of(l, splits(Partner.US)).map(_.summaryPartner),
      List(Partner.US.some)
    )

  test("a dropped partner's stale summary does not hold the request open"):
    val before = list(att(1, AttachmentType.Summary, T0, Partner.US.some))
    val req    = ProposalSummaries.Request(ProposalSummaries.of(before, splits(Partner.US)))
    // US is dropped in favour of CL, whose summary has landed.
    val after  = list(
      att(1, AttachmentType.Summary, T0, Partner.US.some),
      att(2, AttachmentType.Summary, T2, Partner.CL.some)
    )
    assert(!req.anyPending(ProposalSummaries.of(after, splits(Partner.CL))))

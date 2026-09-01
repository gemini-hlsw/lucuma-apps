// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.kernel.Order.catsKernelOrderingForOrder
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.ProposalType.*
import lucuma.core.data.EmailAddress
import lucuma.core.enums.*
import lucuma.core.enums.ProposalSubmissionError.*
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.model.CallCoordinatesLimits
import lucuma.core.model.CallForProposals
import lucuma.core.model.IntPercent
import lucuma.core.model.PartnerLink
import lucuma.core.model.Semester
import lucuma.core.model.SiteCoordinatesLimits
import lucuma.core.model.UserProfile
import lucuma.core.util.DateInterval
import lucuma.core.util.Enumerated
import lucuma.core.util.Timestamp
import munit.FunSuite

import java.time.LocalDate
import scala.collection.immutable.SortedMap

/**
 * The rules `Proposal.errors` applies, one test each.
 *
 * The rules themselves are enumerated in lucuma-core so that this and the ODB describe the same
 * thing; `every rule is accounted for` fails when a rule is added there without being handled here.
 */
class ProposalErrorsSuite extends FunSuite:

  // Rules Proposal.errors does not evaluate.  MissingSemester, BothTimeRequests and
  // ExchangePartnerPiMismatch reject states Explore's model cannot represent, so only
  // the ODB can see them.  PastDeadline depends on the current time and is reported by
  // ProposalSubmissionBar, which already holds a clock for its countdown; validating it
  // here would tie the whole proposal tab's re-rendering to that clock.  See the
  // comments in Proposal.scala.
  private val NotEvaluatedHere: Set[ProposalSubmissionError] =
    Set(MissingSemester, BothTimeRequests, ExchangePartnerPiMismatch, PastDeadline)

  // Coordinate limits play no part in these rules; any valid range will do.
  private val fullSky: SiteCoordinatesLimits =
    SiteCoordinatesLimits(
      RightAscension.Zero,
      RightAscension.Zero,
      Declination.Min,
      Declination.Max
    )

  private val Now: Timestamp =
    Timestamp.unsafeFromInstantTruncated(java.time.Instant.parse("2026-03-01T00:00:00Z"))

  private val Deadline: Timestamp =
    Timestamp.unsafeFromInstantTruncated(java.time.Instant.parse("2026-06-01T00:00:00Z"))

  private val Past: Timestamp =
    Timestamp.unsafeFromInstantTruncated(java.time.Instant.parse("2026-01-01T00:00:00Z"))

  private def geminiCall(
    partners:         List[(Partner, Option[Timestamp])] = List((Partner.US, Deadline.some)),
    nonPartner:       Option[Timestamp] = Deadline.some,
    exchangePartners: List[(ExchangePartner, Option[Timestamp])] = Nil
  ): CallForProposal =
    CallForProposal(
      id = CallForProposals.Id.fromLong(1L).get,
      semester = Semester.unsafeFromString("2026A"),
      title = NonEmptyString.unsafeFrom("Test Call"),
      partners = partners.map(CallPartner.apply.tupled),
      active = DateInterval.between(LocalDate.parse("2026-02-01"), LocalDate.parse("2026-07-31")),
      callProperties = CallProperties.GeminiCallProperties(
        cfpType = GeminiCallForProposalsType.RegularSemester,
        coordinateLimits = CallCoordinatesLimits(fullSky, fullSky),
        instruments = Nil,
        proprietaryMonths = NonNegInt.unsafeFrom(12),
        allowsNonPartnerPi = nonPartner.isDefined,
        nonPartnerDeadline = nonPartner,
        exchangePartners = exchangePartners.map(
          CallProperties.GeminiCallProperties.CallExchangePartner.apply.tupled
        )
      )
    )

  private def user(
    partnerLink: PartnerLink,
    role:        ProgramUserRole,
    email:       Option[String] = "pi@example.com".some,
    education:   Option[EducationalStatus] = EducationalStatus.PhD.some,
    confirmed:   Boolean = true
  ): ProgramUser =
    ProgramUser(
      id = ProgramUser.Id.fromLong(1L).get,
      user = none,
      partnerLink = partnerLink,
      role = role,
      educationalStatus = education,
      thesis = none,
      gender = none,
      affiliation = none,
      preferredProfile = UserProfile(none, none, none, email),
      invitations =
        if confirmed then
          List(
            UserInvitation(
              id = "inv",
              email = EmailAddress.unsafeFrom("coi@example.com"),
              status = InvitationStatus.Redeemed,
              emailStatus = EmailStatus.Delivered.some
            )
          )
        else Nil,
      hasDataAccess = true,
      classicalVisitor = false
    )

  private val pi: ProgramUser =
    user(PartnerLink.HasGeminiPartner(Partner.US), ProgramUserRole.Pi)

  private val queueType: ProposalType =
    GeminiProposalType.Queue(
      scienceSubtype = ScienceSubtype.Queue,
      tooActivationCeiling = TooActivationCeiling(TooActivation.None, TooActivation.None, none),
      minPercentTime = IntPercent.unsafeFrom(100),
      partnerSplits = List(PartnerSplit(Partner.US, IntPercent.unsafeFrom(100))),
      exchangePartner = none,
      aeonMultiFacility = none,
      jwstSynergy = false,
      usLongTerm = false,
      considerForBand3 = ConsiderForBand3.DoNotConsider
    )

  private def attachment(t: AttachmentType, id: Long): (Attachment.Id, Attachment) =
    val aid = Attachment.Id.fromLong(id).get
    aid -> Attachment(
      id = aid,
      attachmentType = t,
      fileName = NonEmptyString.unsafeFrom(s"${t.tag}.pdf"),
      mask = none,
      description = none,
      checked = false,
      fileSize = 1L,
      updatedAt = Now
    )

  private val bothAttachments: AttachmentList =
    SortedMap(attachment(AttachmentType.Science, 1L), attachment(AttachmentType.Team, 2L))

  /** A proposal with nothing wrong with it. */
  private def valid: Proposal =
    Proposal(geminiCall().some, TacCategory.SmallBodies.some, queueType.some, none)

  private def errorsOf(
    proposal:    Proposal = valid,
    title:       Option[NonEmptyString] = NonEmptyString.unsafeFrom("Title").some,
    abstrakt:    Option[NonEmptyString] = NonEmptyString.unsafeFrom("Abstract").some,
    users:       List[ProgramUser] = List(pi),
    attachments: AttachmentList = bothAttachments,
    defined:     Boolean = true,
    undefined:   Boolean = false
  ): List[ProposalSubmissionError] =
    proposal.errors(title, abstrakt, users, attachments, defined, undefined)

  test("a complete proposal has no errors"):
    assertEquals(errorsOf(), Nil)

  test("missing title"):
    assertEquals(errorsOf(title = none), List(MissingTitle))

  test("missing abstract"):
    assertEquals(errorsOf(abstrakt = none), List(MissingAbstract))

  test("missing category"):
    assertEquals(errorsOf(valid.copy(category = none)), List(MissingCategory))

  test("missing call for proposals"):
    assertEquals(errorsOf(valid.copy(call = none)), List(MissingCfp))

  test("missing proposal type"):
    assertEquals(errorsOf(valid.copy(proposalType = none)), List(MissingProposalType))

  test("PI partner the call does not offer"):
    val p = valid.copy(call = geminiCall(partners = List((Partner.CA, Deadline.some))).some)
    assertEquals(errorsOf(p), List(PiPartnerNotInCall, MissingDeadline))

  test("non-partner PI the call does not allow"):
    val p = valid.copy(call = geminiCall(nonPartner = none).some)
    assertEquals(
      errorsOf(p, users = List(user(PartnerLink.HasNonPartner, ProgramUserRole.Pi))),
      List(NonPartnerPiNotAllowed, MissingDeadline)
    )

  test("PI exchange partner the call does not offer"):
    assertEquals(
      errorsOf(users =
        List(user(PartnerLink.HasExchangePartner(ExchangePartner.Keck), ProgramUserRole.Pi))
      ),
      List(ExchangePartnerNotInCall, MissingDeadline)
    )

  test("band 3 consideration unset"):
    val t = GeminiProposalType.Queue.considerForBand3.replace(ConsiderForBand3.Unset)(
      queueType.asInstanceOf[GeminiProposalType.Queue]
    )
    assertEquals(errorsOf(valid.copy(proposalType = (t: ProposalType).some)),
                 List(MissingBand3Consideration)
    )

  test("an investigator with no affiliation"):
    val coi = user(PartnerLink.HasUnspecifiedPartner, ProgramUserRole.Coi)
    assertEquals(errorsOf(users = List(pi, coi)), List(UnspecifiedInvestigatorPartner))

  test("partner splits that do not sum to 100"):
    val t = GeminiProposalType.partnerSplits.replace(
      List(PartnerSplit(Partner.US, IntPercent.unsafeFrom(50)))
    )(queueType.asInstanceOf[GeminiProposalType])
    assertEquals(errorsOf(valid.copy(proposalType = (t: ProposalType).some)),
                 List(InvalidPartnerSplits)
    )

  // Keck and Subaru proposals apportion their time across Gemini partners too.
  test("external proposal splits that do not sum to 100"):
    val t: ProposalType =
      KeckProposalType(IntPercent.unsafeFrom(100),
                       List(PartnerSplit(Partner.US, IntPercent.unsafeFrom(50)))
      )
    assertEquals(
      errorsOf(valid.copy(proposalType = t.some)).filter(_ === InvalidPartnerSplits),
      List(InvalidPartnerSplits)
    )

  test("missing PI email"):
    assertEquals(
      errorsOf(users =
        List(user(PartnerLink.HasGeminiPartner(Partner.US), ProgramUserRole.Pi, email = none))
      ),
      List(MissingPiEmail)
    )

  test("unparseable PI email"):
    assertEquals(
      errorsOf(users =
        List(
          user(PartnerLink.HasGeminiPartner(Partner.US),
               ProgramUserRole.Pi,
               email = "not an email".some
          )
        )
      ),
      List(InvalidPiEmail)
    )

  test("an investigator who was never invited"):
    val coi = user(PartnerLink.HasGeminiPartner(Partner.US), ProgramUserRole.Coi, confirmed = false)
    assertEquals(errorsOf(users = List(pi, coi)), List(UninvitedInvestigator))

  test("UH time without a UH PI"):
    val t = GeminiProposalType.partnerSplits.replace(
      List(PartnerSplit(Partner.UH, IntPercent.unsafeFrom(100)))
    )(queueType.asInstanceOf[GeminiProposalType])
    val p = valid.copy(
      call =
        geminiCall(partners = List((Partner.US, Deadline.some), (Partner.UH, Deadline.some))).some,
      proposalType = (t: ProposalType).some
    )
    assertEquals(errorsOf(p), List(UhTimeWithoutUhPi))

  test("non-US partner time without a matching collaborator"):
    val t = GeminiProposalType.partnerSplits.replace(
      List(PartnerSplit(Partner.CA, IntPercent.unsafeFrom(100)))
    )(queueType.asInstanceOf[GeminiProposalType])
    val p = valid.copy(
      call =
        geminiCall(partners = List((Partner.US, Deadline.some), (Partner.CA, Deadline.some))).some,
      proposalType = (t: ProposalType).some
    )
    assertEquals(errorsOf(p), List(UnmatchedPartnerTime))

  // US is the default home for a proposal with no other affiliation, so unlike
  // every other partner a US share needs no investigator to back it.
  test("US time needs no matching collaborator"):
    assertEquals(errorsOf(), Nil)

  test("fast turnaround without a mentor for a non-PhD reviewer"):
    val t: ProposalType = GeminiProposalType.FastTurnaround(
      scienceSubtype = ScienceSubtype.FastTurnaround,
      tooActivationCeiling = TooActivationCeiling(TooActivation.None, TooActivation.None, none),
      minPercentTime = IntPercent.unsafeFrom(100),
      reviewerId = none,
      mentorId = none
    )
    val gradPi          = user(PartnerLink.HasGeminiPartner(Partner.US),
                      ProgramUserRole.Pi,
                      education = EducationalStatus.GradStudent.some
    )
    // A fast turnaround proposal is reviewed without a team attachment.
    assertEquals(
      errorsOf(valid.copy(proposalType = t.some),
               users = List(gradPi),
               attachments = SortedMap(attachment(AttachmentType.Science, 1L))
      ),
      List(MissingFtMentor)
    )

  test("missing science attachment"):
    assertEquals(
      errorsOf(attachments = SortedMap(attachment(AttachmentType.Team, 2L))),
      List(MissingScienceAttachment)
    )

  test("missing team attachment"):
    assertEquals(
      errorsOf(attachments = SortedMap(attachment(AttachmentType.Science, 1L))),
      List(MissingTeamAttachment)
    )

  test("no defined observation"):
    assertEquals(errorsOf(defined = false), List(NoDefinedObservations))

  test("undefined observations"):
    assertEquals(errorsOf(undefined = true), List(UndefinedObservations))

  test("no deadline for the PI"):
    val p = valid.copy(call = geminiCall(partners = List((Partner.US, none))).some)
    assertEquals(errorsOf(p), List(MissingDeadline))

  // Only whether a deadline resolves is checked here; a deadline that has passed is
  // reported by ProposalSubmissionBar.
  test("a passed deadline is not reported here"):
    val p = valid.copy(call = geminiCall(partners = List((Partner.US, Past.some))).some)
    assertEquals(errorsOf(p), Nil)

  test("every rule is accounted for"):
    // Every rule is either exercised above or listed as not evaluated here.  Adding a
    // rule in lucuma-core fails this until it is handled here too.
    val exercised: Set[ProposalSubmissionError] =
      Set(
        MissingTitle,
        MissingAbstract,
        MissingCategory,
        MissingCfp,
        MissingProposalType,
        PiPartnerNotInCall,
        NonPartnerPiNotAllowed,
        ExchangePartnerNotInCall,
        MissingBand3Consideration,
        UnspecifiedInvestigatorPartner,
        InvalidPartnerSplits,
        MissingPiEmail,
        InvalidPiEmail,
        UninvitedInvestigator,
        UhTimeWithoutUhPi,
        UnmatchedPartnerTime,
        MissingFtMentor,
        MissingScienceAttachment,
        MissingTeamAttachment,
        NoDefinedObservations,
        UndefinedObservations,
        MissingDeadline
      )
    assertEquals(
      Enumerated[ProposalSubmissionError].all.toSet -- exercised -- NotEvaluatedHere,
      Set.empty
    )

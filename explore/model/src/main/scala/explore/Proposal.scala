// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.ProposalType.*
import explore.model.syntax.all.*
import io.circe.Decoder
import lucuma.core.data.EmailAddress
import lucuma.core.enums.AttachmentType
import lucuma.core.enums.ConsiderForBand3
import lucuma.core.enums.Observatory
import lucuma.core.enums.Partner
import lucuma.core.enums.ProgramUserRole
import lucuma.core.enums.ProposalSubmissionError
import lucuma.core.enums.ProposalSubmissionError.*
import lucuma.core.enums.TacCategory
import lucuma.core.model.PartnerLink
import lucuma.core.model.ProposalReference
import lucuma.core.util.Timestamp
import monocle.Focus
import monocle.Iso
import monocle.Lens

case class Proposal(
  call:         Option[CallForProposal],
  category:     Option[TacCategory],
  proposalType: Option[ProposalType],
  reference:    Option[ProposalReference]
) derives Eq:
  def deadline(piPartner: Option[PartnerLink]): Option[Either[String, Timestamp]] =
    call.map(_.deadline(piPartner))

  // in reality, should always have a PI
  extension (users: List[ProgramUser])
    private def pi: Option[ProgramUser]            =
      users.find(_.role === ProgramUserRole.Pi)
    private def hasPi(partner: Partner): Boolean   =
      pi.exists(_.partnerLink.geminiPartnerOption.exists(_ === partner))
    private def hasUser(partner: Partner): Boolean =
      users.exists(_.partnerLink.geminiPartnerOption.exists(_ === partner))

  private def cfPError(users: List[ProgramUser]): List[ProposalSubmissionError] =
    call.fold(List(MissingCfp))(cfp =>
      val piAffiliation = users.pi.fold(PartnerLink.HasUnspecifiedPartner)(_.partnerLink)
      // Gemini partners, non-partner PIs and exchange communities are all properties
      // of a Gemini call, so none of this applies to a Keck or Subaru one.
      val partnerError  = if cfp.gemini.isEmpty then none
      else
        piAffiliation match
          case PartnerLink.HasGeminiPartner(partner)   =>
            Option.when(!cfp.partners.exists(_.partner === partner))(PiPartnerNotInCall)
          case PartnerLink.HasNonPartner               =>
            Option.when(cfp.gemini.exists(!_.allowsNonPartnerPi))(NonPartnerPiNotAllowed)
          // The whole request is assigned to the exchange partner community instead
          // of being apportioned across Gemini partners, so only the community itself
          // has to be one the call offers.
          case PartnerLink.HasExchangePartner(partner) =>
            Option.when(
              !cfp.gemini.exists(_.exchangePartners.exists(_.exchangePartner === partner))
            )(ExchangePartnerNotInCall)
          // This gets checked in usersAndTimesErrors
          case PartnerLink.HasUnspecifiedPartner       => none
      // A Gemini proposal must say which kind it is; an external (Keck or Subaru)
      // one takes its type from the call.
      val typeError     = Option.when(
        cfp.observatory === Observatory.Gemini && proposalType.isEmpty
      )(MissingProposalType)
      val band3Error    = Option.when(
        proposalType.exists:
          case GeminiProposalType.Queue(considerForBand3 = considerForBand3) =>
            considerForBand3 === ConsiderForBand3.Unset
          case _                                                             =>
            false
      )(MissingBand3Consideration)
      List(partnerError, typeError, band3Error).flattenOption
    )

  // if this is None, either a CfP has not been selected, they are not required for the proposal
  // type, or the time is requested on behalf of an exchange partner community instead
  //
  // The exchange partner is derived from the PI's affiliation, so a co-investigator's
  // exchange link never reaches this and leaves the splits requirement in place.  The
  // ODB enforces that same correspondence explicitly, because the API can set the
  // proposal's exchange partner and the PI's affiliation independently.
  private lazy val geminiPartnerSplits: Option[List[PartnerSplit]] =
    proposalType
      .flatMap(ProposalType.geminiProposalType.getOption)
      .filter(GeminiProposalType.exchangePartner.getOption(_).flatten.isEmpty)
      .flatMap(GeminiProposalType.partnerSplits.getOption)

  // An external (Keck or Subaru) proposal apportions its time across Gemini
  // partners, so its splits have to add up as well.
  private lazy val externalPartnerSplits: Option[List[PartnerSplit]] =
    proposalType.flatMap:
      case k: KeckProposalType   => k.partnerSplits.some
      case s: SubaruProposalType => s.partnerSplits.some
      case _                     => none

  private lazy val partnerSplits: Option[List[PartnerSplit]] =
    geminiPartnerSplits.orElse(externalPartnerSplits)

  private def usersAndTimesErrors(users: List[ProgramUser]): List[ProposalSubmissionError] =
    val partnerError       =
      Option.unless(users.forall(_.partnerLink.isSet))(UnspecifiedInvestigatorPartner)
    val partnerSplitsError = partnerSplits.flatMap(splits =>
      Option.when(splits.foldLeft(0)(_ + _.percent.value) != 100)(InvalidPartnerSplits)
    )
    val piEmail            = users.pi.flatMap(_.email)
    val piEmailError       =
      piEmail.fold(MissingPiEmail.some)(e =>
        Option.unless(EmailAddress.from(e).isRight)(InvalidPiEmail)
      )
    val notInvitedError    =
      Option
        .when(users.exists(u => !u.isConfirmed && !u.successfullyInvited))(UninvitedInvestigator)

    // only validate this if the splits are valid and all partners have been affiliated.
    val affiliationMismatches: List[ProposalSubmissionError] =
      if partnerError.isEmpty && partnerSplitsError.isEmpty then
        // Make sure every partner split requested has a matching user.
        // Only verify this if splits and users are all valid.
        partnerSplits
          .map(splits =>
            splits
              .filter(_.percent.value > 0)
              .map(ps =>
                if (ps.partner === Partner.UH && !users.hasPi(Partner.UH))
                  UhTimeWithoutUhPi.some
                else if (ps.partner =!= Partner.US && !users.hasUser(ps.partner))
                  UnmatchedPartnerTime.some
                else none
              )
              .flattenOption
              .distinct
          )
          .toList
          .flatten
      else List.empty

    List(
      partnerError.toList,
      piEmailError.toList,
      notInvitedError.toList,
      partnerSplitsError.toList,
      affiliationMismatches
    ).flatten

  private lazy val fastTurnaround: Option[GeminiProposalType.FastTurnaround] =
    proposalType
      .flatMap(ProposalType.geminiProposalType.getOption)
      .flatMap(GeminiProposalType.fastTurnaround.getOption)

  private lazy val isFastTurnaround: Boolean = fastTurnaround.isDefined

  private def attachmentErrors(attachments: AttachmentList): List[ProposalSubmissionError] =
    // only validate if there is a CfP
    call.foldMap(_ =>
      val science = Option.unless(attachments.hasForType(AttachmentType.Science))(
        MissingScienceAttachment
      )
      val team    = Option.unless(isFastTurnaround || attachments.hasForType(AttachmentType.Team))(
        MissingTeamAttachment
      )
      List(science, team).flattenOption
    )

  private def fastTurnaroundErrors(users: List[ProgramUser]): List[ProposalSubmissionError] =
    fastTurnaround.foldMap(ft =>
      // explore defaults reviewer to PI, but it can be unset via the API and the API
      // says it will default to the PI if null.
      val reviewer = ft.reviewerId.flatMap(r => users.find(_.id === r)).orElse(users.pi)
      if (reviewer.exists(_.hasPhd) || ft.mentorId.isDefined) List.empty
      else List(MissingFtMentor)
    )

  private def obsErrors(
    hasDefinedObservations:   Boolean,
    hasUndefinedObservations: Boolean
  ): List[ProposalSubmissionError] =
    List(
      Option.unless(hasDefinedObservations)(NoDefinedObservations),
      Option.when(hasUndefinedObservations)(UndefinedObservations)
    ).flattenOption

  /**
   * Whether a deadline could be worked out for this PI at all. Not being able to is a property of
   * the proposal, so it belongs here; whether a deadline that does resolve has *passed* depends on
   * the current time, and is reported by the submission bar, which already holds a clock to render
   * its countdown. Validating it here would tie the whole tab's re-rendering to that clock.
   *
   * Three ODB rules therefore have no counterpart here: `PastDeadline` as just described, the
   * semester requirement, which `CallForProposal` always carries, and the rejection of a proposal
   * holding both an exchange partner and partner splits, which `partnerSplits` makes
   * unrepresentable.
   */
  private def deadlineErrors(users: List[ProgramUser]): List[ProposalSubmissionError] =
    call.foldMap(_ =>
      deadline(users.pi.map(_.partnerLink)) match
        case None | Some(Left(_)) => List(MissingDeadline)
        case Some(Right(_))       => Nil
    )

  def errors(
    title:                    Option[NonEmptyString], // from program name
    abstrakt:                 Option[NonEmptyString], // from program description
    users:                    List[ProgramUser],
    attachments:              AttachmentList,
    hasDefinedObservations:   Boolean,
    hasUndefinedObservations: Boolean
  ): List[ProposalSubmissionError] = List(
    Option.when(title.isEmpty)(MissingTitle).toList,
    Option.when(abstrakt.isEmpty)(MissingAbstract).toList,
    Option.unless(category.isDefined)(MissingCategory).toList,
    cfPError(users),
    usersAndTimesErrors(users),
    fastTurnaroundErrors(users),
    attachmentErrors(attachments),
    obsErrors(hasDefinedObservations, hasUndefinedObservations),
    deadlineErrors(users)
  ).flatten

object Proposal:
  val call: Lens[Proposal, Option[CallForProposal]]                  =
    Focus[Proposal](_.call)
  val category: Lens[Proposal, Option[TacCategory]]                  =
    Focus[Proposal](_.category)
  val proposalType: Lens[Proposal, Option[ProposalType]]             =
    Focus[Proposal](_.proposalType)
  // Focuses the Gemini proposal type, discarding any non-Gemini type on set.
  // Only set or modify it where the proposal is known to be Gemini: `modify` on a
  // Keck or Subaru proposal clears the type entirely. Elsewhere reach the Gemini
  // type through `proposalType.some.andThen(ProposalType.geminiProposalType)`.
  val geminiProposalType: Lens[Proposal, Option[GeminiProposalType]] =
    Lens[Proposal, Option[GeminiProposalType]](
      _.proposalType.flatMap(ProposalType.geminiProposalType.getOption)
    )(optGpt => _.copy(proposalType = optGpt))

  // Focuses the Keck proposal type, discarding any non-Keck type on set.
  val keckProposalType: Lens[Proposal, Option[KeckProposalType]] =
    Lens[Proposal, Option[KeckProposalType]](
      _.proposalType.flatMap(ProposalType.keckProposalType.getOption)
    )(optKpt => _.copy(proposalType = optKpt))

  // Focuses the Subaru proposal type, discarding any non-Subaru type on set.
  val subaruProposalType: Lens[Proposal, Option[SubaruProposalType]] =
    Lens[Proposal, Option[SubaruProposalType]](
      _.proposalType.flatMap(ProposalType.subaruProposalType.getOption)
    )(optSpt => _.copy(proposalType = optSpt))

  val reference: Lens[Proposal, Option[ProposalReference]] =
    Focus[Proposal](_.reference)

  given Decoder[Proposal] = c =>
    for {
      call     <- c.downField("call").as[Option[CallForProposal]]
      category <- c.downField("category").as[Option[TacCategory]]
      gemini   <- c.downField("gemini").as[Option[GeminiProposalType]]
      keck     <- c.downField("keck").as[Option[KeckProposalType]]
      subaru   <- c.downField("subaru").as[Option[SubaruProposalType]]
      r        <-
        c.downField("reference")
          .downField("label")
          .success
          .traverse(_.as[Option[ProposalReference]])
    } yield Proposal(call,
                     category,
                     (gemini: Option[ProposalType]).orElse(keck).orElse(subaru),
                     r.flatten
    )

  val Default = Proposal(None, None, None, None)

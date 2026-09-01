// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.effect.IO
import cats.syntax.all.*
import clue.*
import clue.data.Input
import clue.data.Unassign
import clue.data.syntax.*
import crystal.react.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.Aligner
import explore.components.HelpIcon
import explore.components.SimpleTile
import explore.components.Tile
import explore.components.TileContents
import explore.components.TileController
import explore.components.ui.*
import explore.model.AppContext
import explore.model.AttachmentList
import explore.model.CallForProposal
import explore.model.Constants
import explore.model.ExploreGridLayouts
import explore.model.ProgramDetails
import explore.model.ProgramUser
import explore.model.Proposal
import explore.model.ProposalTabTileIds
import explore.model.ProposalType
import explore.model.ProposalType.GeminiProposalType
import explore.model.ProposalType.GeminiProposalType.FastTurnaround
import explore.model.enums.GridLayoutSection
import explore.model.layout.LayoutsMap
import explore.users.AddProgramUserButton
import explore.users.ProgramUsersTable
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ExchangePartner
import lucuma.core.enums.GeminiCallForProposalsType
import lucuma.core.enums.Instrument
import lucuma.core.enums.ProgramUserRole
import lucuma.core.enums.ProposalSubmissionError
import lucuma.core.enums.ScienceSubtype
import lucuma.core.enums.Site
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.floatingui.syntax.*
import lucuma.react.primereact.PrimeStyles
import lucuma.react.resizeDetector.hooks.*
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import lucuma.ui.optics.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.react.given
import lucuma.ui.reusability.given
import lucuma.ui.sso.UserVault
import lucuma.ui.syntax.all.given
import lucuma.ui.undo.*
import monocle.Iso

case class ProposalEditor(
  programId:          Program.Id,
  userVault:          Option[UserVault],
  undoCtx:            UndoContext[ProgramDetails],
  proposal:           UndoSetter[Proposal],
  users:              View[List[ProgramUser]],
  attachments:        View[AttachmentList],
  errors:             Option[List[ProposalSubmissionError]],
  authToken:          Option[NonEmptyString],
  cfps:               List[CallForProposal],
  layout:             LayoutsMap,
  proposalIsReadonly: Boolean,
  userIsReadonlyCoi:  Boolean,
  aeonInstruments:    Map[Instrument, Site]
) extends ReactFnProps(ProposalEditor):
  val optUserId: Option[User.Id]        = userVault.map(_.user.id)
  val proposalOrUserIsReadonly: Boolean = proposalIsReadonly || userIsReadonlyCoi
  val pi: Option[ProgramUser]           = undoCtx.get.pi

  val geminiProposalType: Option[GeminiProposalType] =
    proposal.get.proposalType.flatMap(ProposalType.geminiProposalType.getOption)

  // The exchange partner the proposal type currently carries. The outer `Option`
  // is empty for the subtypes that cannot have one at all.
  val proposalExchangePartner: Option[Option[ExchangePartner]] =
    geminiProposalType.flatMap(GeminiProposalType.exchangePartner.getOption)

  // On a Regular Semester call, a PI belonging to an exchange partner community
  // requests the whole time on its behalf instead of splitting it across Gemini
  // partners. Anywhere else there is no exchange partner.
  val piExchangePartner: Option[ExchangePartner] =
    proposal.get.call
      .flatMap(_.gemini)
      .filter(_.cfpType === GeminiCallForProposalsType.RegularSemester)
      .flatMap(_ => pi.flatMap(_.partnerLink.exchangePartnerOption))

object ProposalEditor
    extends ReactFnComponent[ProposalEditor](props =>
      val BaseWordLimit = 200
      val HardWordLimit = 2 * BaseWordLimit

      extension (s: String)
        inline def wordCount: Int =
          val trim = s.trim
          if (trim.isEmpty) 0
          else
            trim
              .split("\\s+", HardWordLimit + 1)
              .length // add a limit to restrict the performance hit

      // Only Queue and Classical requests can be assigned to an exchange partner.
      // Just the two mutually exclusive time request fields are sent, leaving the
      // rest of the proposal type alone.
      def timeRequestInput(
        scienceSubtype:  ScienceSubtype,
        partnerSplits:   Input[List[PartnerSplitInput]],
        exchangePartner: Input[ExchangePartner]
      ): Option[GeminiProposalTypeInput] =
        scienceSubtype match
          case ScienceSubtype.Classical =>
            GeminiProposalTypeInput
              .Classical(
                ClassicalInput(partnerSplits = partnerSplits, exchangePartner = exchangePartner)
              )
              .some
          case ScienceSubtype.Queue     =>
            GeminiProposalTypeInput
              .Queue(QueueInput(partnerSplits = partnerSplits, exchangePartner = exchangePartner))
              .some
          case _                        => none

      // The two fields always move together, since a request belongs either to an
      // exchange partner community or to Gemini partners. Empty splits are sent as
      // null; the ODB rejects an empty list, which must sum to 100 when given.
      def remoteTimeRequestUpdate(
        scienceSubtype:  ScienceSubtype,
        exchangePartner: Option[ExchangePartner]
      )(using ctx: AppContext[IO]): IO[Unit] =
        timeRequestInput(scienceSubtype, Unassign, exchangePartner.orUnassign)
          .foldMap(gemini =>
            ctx.odbApi
              .updateProposal:
                UpdateProposalInput(
                  programId = props.programId.assign,
                  SET = ProposalPropertiesInput(gemini = gemini.assign)
                )
              .void
          )

      for
        ctx             <- useContext(AppContext.ctx)
        abstractCounter <-
          useState(props.undoCtx.get.description.map(_.value).foldMap(_.wordCount))
        _               <- useEffectWithDeps(props.undoCtx.get.description.map(_.value)) {
                             case Some(t) => abstractCounter.setState(t.wordCount)
                             case None    => abstractCounter.setState(0)
                           }
        // Keep the time request aligned with the PI's partner link, which can change
        // from the investigators table or by selecting a different call. This is a
        // consequence of another edit, so it doesn't participate in undo/redo.
        _               <- useEffectWithDeps((props.piExchangePartner, props.proposalExchangePartner)):
                             (desired, current) =>
                               import ctx.given

                               // `current` is empty for the proposal types that cannot
                               // have an exchange partner at all.
                               current
                                 .filterNot(_ => props.proposalOrUserIsReadonly)
                                 .filter(_ =!= desired)
                                 .foldMap: _ =>
                                   props.geminiProposalType.foldMap: gemini =>
                                     props.proposal.model
                                       .zoom(
                                         Proposal.proposalType.some
                                           .andThen(ProposalType.geminiProposalType)
                                       )
                                       .mod(
                                         GeminiProposalType.withExchangePartner(desired)
                                       ) >>
                                       remoteTimeRequestUpdate(gemini.scienceSubtype, desired)(using
                                         ctx
                                       ).runAsyncAndForget
        resize          <- useResizeDetector
      yield
        import ctx.given

        props.userVault.map: userVault =>
          val detailsAligner: Aligner[ProgramDetails, ProgramPropertiesInput] =
            Aligner(
              props.undoCtx,
              UpdateProgramsInput(
                WHERE = props.programId.toWhereProgram.assign,
                SET = ProgramPropertiesInput()
              ),
              ctx.odbApi.updateProgram(_)
            ).zoom(Iso.id[ProgramDetails].asLens, UpdateProgramsInput.SET.modify)

          val proposalAligner: Aligner[Proposal, ProposalPropertiesInput] =
            Aligner(
              props.proposal,
              UpdateProposalInput(
                programId = props.programId.assign,
                SET = ProposalPropertiesInput()
              ),
              ctx.odbApi.updateProposal(_)
            ).zoom(Iso.id[Proposal].asLens, UpdateProposalInput.SET.modify)

          val abstractAligner: Aligner[Option[NonEmptyString], Input[NonEmptyString]] =
            detailsAligner.zoom(ProgramDetails.description,
                                ProgramPropertiesInput.description.modify
            )

          val abstractView: View[Option[NonEmptyString]] = abstractAligner
            .view(_.orUnassign)

          val defaultLayouts = ExploreGridLayouts.sectionLayout(GridLayoutSection.ProposalLayout)

          // the FT reviewer and mentor assigments don't participate in undo/redo
          val fastTurnaroundView: Option[View[FastTurnaround]] =
            props.proposal.model
              .zoom(Proposal.proposalType)
              .toOptionView
              .flatMap(
                _.zoom(
                  ProposalType.geminiProposalType.andThen(GeminiProposalType.fastTurnaround)
                ).toOptionView
              )

          val classicalView: ViewOpt[GeminiProposalType.Classical] =
            props.proposal.model
              .zoom(
                Proposal.proposalType.some
                  .andThen(ProposalType.geminiProposalType)
                  .andThen(GeminiProposalType.classical)
              )

          def reviewerMentorRemoteUpdate(
            reviewer: Input[ProgramUser.Id],
            mentor:   Input[ProgramUser.Id]
          ): Callback =
            ctx.odbApi
              .updateProposal:
                UpdateProposalInput(
                  programId = props.programId.assign,
                  SET = ProposalPropertiesInput(
                    gemini = GeminiProposalTypeInput
                      .FastTurnaround:
                        FastTurnaroundInput(reviewerId = reviewer, mentorId = mentor)
                      .assign
                  )
                )
              .void
              .runAsyncAndForget

          def setFastTurnaroundReviewerOnly(reviewer: Option[ProgramUser]): Callback =
            fastTurnaroundView.foldMap: v =>
              v.zoom(FastTurnaround.reviewerId).set(reviewer.map(_.id)) >>
                reviewerMentorRemoteUpdate(reviewer.map(_.id).orUnassign, Input.ignore)

          def setFastTurnaroundMentorOnly(mentor: Option[ProgramUser]): Callback =
            fastTurnaroundView.foldMap: v =>
              v.zoom(FastTurnaround.mentorId).set(mentor.map(_.id)) >>
                reviewerMentorRemoteUpdate(Input.ignore, mentor.map(_.id).orUnassign)

          def setFastTurnaroundReviewerAndMentor(
            reviewer: Option[ProgramUser],
            mentor:   Option[ProgramUser]
          ): Callback =
            fastTurnaroundView.foldMap: v =>
              v.zoom(FastTurnaround.reviewerId).set(reviewer.map(_.id)) >>
                v.zoom(FastTurnaround.mentorId).set(mentor.map(_.id)) >>
                reviewerMentorRemoteUpdate(reviewer.map(_.id).orUnassign,
                                           mentor.map(_.id).orUnassign
                )

          // clears mentor if reviewer has a PhD
          def setFastTurnaroundReviewer(reviewer: Option[ProgramUser]): Callback =
            // clear the mentor if the reviewer has a PhD
            if (reviewer.exists(_.hasPhd))
              setFastTurnaroundReviewerAndMentor(reviewer, none)
            else setFastTurnaroundReviewerOnly(reviewer)

          // handle updates on the user table
          def onUsersMod(users: List[ProgramUser]): Callback =
            val updateFt: FastTurnaround => FastTurnaround = ft =>
              val reviewer = ft.reviewerId.flatMap(i => users.find(_.id == i))
              val mentor   = ft.mentorId.flatMap(i => users.find(_.id == i))

              val newReviewerId = (ft.reviewerId, reviewer) match
                case (Some(_), None) =>
                  // reviewer was deleted, so we want set it to the PI
                  props.pi.map(_.id)
                case _               => ft.reviewerId

              val newMentorId =
                if (reviewer.orElse(props.pi).exists(_.hasPhd))
                  none
                else
                  (ft.mentorId, mentor) match
                    case (Some(_), None)                  =>
                      // mentor was deleted
                      none
                    case (Some(id), Some(m)) if !m.hasPhd =>
                      // The mentor has probably been downgraded from a PhD, so remove them
                      none
                    case _                                => ft.mentorId

              val afterReviewer =
                if (ft.reviewerId === newReviewerId) ft
                else FastTurnaround.reviewerId.replace(newReviewerId)(ft)
              if (ft.mentorId === newMentorId) afterReviewer
              else FastTurnaround.mentorId.replace(newMentorId)(afterReviewer)

            fastTurnaroundView.foldMap: v =>
              v.modCB(
                updateFt,
                (oldFt: FastTurnaround, newFt: FastTurnaround) =>
                  if (oldFt.reviewerId === newFt.reviewerId && oldFt.mentorId === newFt.mentorId)
                    Callback.empty
                  else
                    reviewerMentorRemoteUpdate(newFt.reviewerId.orUnassign,
                                               newFt.mentorId.orUnassign
                    )
              )

          val detailsTile =
            SimpleTile(
              ProposalTabTileIds.DetailsId.id,
              "Details",
              autoHeight = true,
              autoHeightMinRows = 4
            )(tileSize =>
              TileContents(
                title = ProposalDetailsTitle(
                  props.undoCtx,
                  tileSize,
                  props.proposalOrUserIsReadonly
                ),
                body = ProposalDetailsBody(
                  detailsAligner,
                  proposalAligner,
                  props.cfps,
                  props.users.get,
                  setFastTurnaroundReviewer,
                  setFastTurnaroundMentorOnly,
                  props.proposalOrUserIsReadonly,
                  props.aeonInstruments
                )
              )
            )

          val usersTile =
            SimpleTile(
              ProposalTabTileIds.UsersId.id,
              "Investigators",
              autoHeight = true,
              autoHeightMinRows = 4
            )(_ =>
              TileContents(
                title = <.div(
                  ExploreStyles.AddProgramUserButton,
                  Option
                    .unless[VdomNode](props.proposalOrUserIsReadonly):
                      AddProgramUserButton(props.programId, ProgramUserRole.CoiRO, props.users)
                    .orEmpty,
                  HelpIcon("proposal/main/investigators.md".refined)
                ),
                body = ProgramUsersTable(
                  props.users.withOnMod(onUsersMod),
                  ProgramUsersTable.Mode.CoIs(
                    userVault,
                    props.proposalIsReadonly,
                    props.userIsReadonlyCoi,
                    classicalView.get.isDefined
                  )
                )
              )
            )

          val absTitle: VdomNode =
            if (abstractCounter.value < 1) "Abstract"
            else if (abstractCounter.value >= HardWordLimit)
              React.Fragment(
                "Abstract ",
                <.span(ExploreStyles.AbstractTitleTooLong, s"($HardWordLimit or more words)")
              )
            else if (abstractCounter.value >= BaseWordLimit)
              React.Fragment(
                "Abstract ",
                <.span(ExploreStyles.AbstractTitleTooLong, s"(${abstractCounter.value} words)")
              )
            else s"Abstract (${abstractCounter.value} words)"

          val abstractTile =
            SimpleTile(
              ProposalTabTileIds.AbstractId.id,
              absTitle,
              bodyClass = ExploreStyles.ProposalAbstract
            )(_ =>
              TileContents:
                FormInputTextAreaView(
                  id = "abstract".refined,
                  value = abstractView.as(OptionNonEmptyStringIso),
                  onTextChange = t => abstractCounter.setState(t.wordCount).rateLimitMs(1000).void
                )(
                  ^.disabled := props.proposalOrUserIsReadonly,
                  ^.cls      := ExploreStyles.WarningInput
                    .when_(abstractView.get.isEmpty && !props.proposalOrUserIsReadonly)
                    .htmlClass
                )
            )

          val attachmentsTile =
            SimpleTile(
              ProposalTabTileIds.AttachmentsId.id,
              "Attachments",
              autoHeight = true,
              autoHeightMinRows = 4,
              tileClass = ExploreStyles.ProposalAttachmentsTile
            )(_ =>
              TileContents(
                title = // put it in a span so it doesn't take up the full width
                  <.span(
                    <.a(
                      ^.href   := Constants.P1TemplatesUrl,
                      ^.target := "_blank",
                      Icons.ArrowUpRightFromSquare,
                      PrimeStyles.Component,
                      PrimeStyles.Button,
                      PrimeStyles.ButtonIconOnly,
                      LucumaPrimeStyles.Tiny,
                      LucumaPrimeStyles.Compact,
                      PrimeStyles.ButtonSecondary
                    ).withTooltip("Download templates")
                  ),
                body = props.authToken.map(token =>
                  ProposalAttachmentsTable(
                    props.programId,
                    token,
                    props.attachments,
                    props.proposal.get.proposalType,
                    props.proposalOrUserIsReadonly,
                    props.errors.exists(_.nonEmpty)
                  )
                )
              )
            )

          val errorsTile: Tile[?] =
            props.errors.fold(Tile.Dummy(ProposalTabTileIds.ErrorsId.id))(
              ProposalErrorsTile(_)
            )

          <.div(ExploreStyles.MultiPanelTile)(
            TileController(
              props.optUserId,
              resize.width.getOrElse(1),
              defaultLayouts,
              props.layout,
              List(
                detailsTile,
                usersTile,
                abstractTile,
                attachmentsTile,
                errorsTile
              ),
              GridLayoutSection.ProposalLayout,
              storeLayout = true
            )
          ).withRef(resize.ref)
    )

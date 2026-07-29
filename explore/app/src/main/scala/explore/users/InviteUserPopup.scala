// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.users

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.cats.given
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.ExploreModelValidators
import explore.model.ProgramUser
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.data.EmailAddress
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.Message
import lucuma.react.primereact.OverlayPanel
import lucuma.react.primereact.OverlayPanelRef
import lucuma.react.primereact.PrimeStyles
import lucuma.refined.*
import lucuma.ui.components.CopyControl
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import org.typelevel.log4cats.Logger

case class InviteUserPopup(
  programUser:        View[ProgramUser],
  createInviteStatus: View[CreateInviteStatus],
  overlayRef:         OverlayPanelRef
) extends ReactFnProps(InviteUserPopup.Component):
  val programUserId                      = programUser.get.id
  val initialEmail: Option[EmailAddress] =
    programUser.get.email.flatMap(EmailAddress.from(_).toOption)

object InviteUserPopup:
  private val Component = ScalaFnComponent[InviteUserPopup](props =>
    for {
      ctx        <- useContext(AppContext.ctx)
      emailView  <- useStateView(none[EmailAddress])
      validEmail <- useState(false)
      key        <- useStateView(none[String])
      _          <- useEffectWithDeps((props.programUserId, props.programUser.get.email)): _ =>
                      emailView.set(props.initialEmail) >>
                        validEmail.setState(props.initialEmail.isDefined)
    } yield {
      import ctx.given

      val createInviteStatus: View[CreateInviteStatus] = props.createInviteStatus

      def createInvitation(
        email:   EmailAddress,
        viewKey: View[Option[String]]
      ): IO[Unit] =
        createInviteStatus.set(CreateInviteStatus.Running).to[IO] >>
          ctx.odbApi
            .createUserInvitation(props.programUserId, email)
            .flatMap: result =>
              // set the preferred email address of the user to this email if it is
              // different from the calculated email address of the user.
              val setEmail: IO[Unit] =
                if (props.programUser.get.email.exists(_ === email.value.value)) IO.unit
                else
                  props.programUser
                    .zoom(ProgramUser.preferredEmail)
                    .set(email.some)
                    .to[IO] >>
                    ctx.odbApi.updateUserPreferredEmail(props.programUserId, email.value.value.some)

              props.programUser
                .zoom(ProgramUser.invitations)
                .mod(result.invitation :: _)
                .to[IO] *>
                setEmail *>
                viewKey.set(result.key.some).to[IO] *>
                createInviteStatus.set(CreateInviteStatus.Done).to[IO]
            .handleErrorWith: e =>
              Logger[IO].error(e)("Error creating invitation") *>
                createInviteStatus.set(CreateInviteStatus.Error).to[IO]

      OverlayPanel(
        closeOnEscape = true,
        onHide = key.set(None) >>
          emailView.set(props.initialEmail) >>
          validEmail.setState(props.initialEmail.isDefined) >>
          createInviteStatus
            .set(CreateInviteStatus.Idle)
            .unless_(createInviteStatus.get === CreateInviteStatus.Running)
      )(
        <.div(PrimeStyles.Dialog)(
          <.div(PrimeStyles.DialogHeader)(s"Create invitation"),
          <.div(PrimeStyles.DialogContent)(
            <.div(LucumaPrimeStyles.FormColumnCompact)(
              FormInputTextView(
                id = "email-invite".refined,
                value = emailView,
                label = "Email",
                disabled = createInviteStatus.get === CreateInviteStatus.Running,
                validFormat = ExploreModelValidators.MailValidator.optional,
                onValidChange = v => validEmail.setState(v)
              )(^.autoComplete := "off")
            ).when(key.get.isEmpty),
            key.get.map: inviteKey =>
              val sentTo = emailView.get.foldMap(email => s" to ${email.value.value}")
              React.Fragment(
                <.div(LucumaPrimeStyles.FormColumn)(
                  <.label(
                    s"An invitation email has been sent$sentTo. If you wish to send the invitation another way, copy and send the key below to the invited user, it won't be displayed again."
                  )
                ),
                <.div(LucumaPrimeStyles.FormColumn)(
                  CopyControl("Invite key", inviteKey)
                )
              )
          ),
          <.div(PrimeStyles.DialogFooter)(
            Message(
              text = "Error submitting user invite, try later",
              severity = Message.Severity.Error
            ).when(createInviteStatus.get === CreateInviteStatus.Error),
            Button(
              icon = Icons.Close,
              onClickE =
                e => createInviteStatus.set(CreateInviteStatus.Idle) *> props.overlayRef.toggle(e),
              label = "Close"
            ).compact.when(createInviteStatus.get === CreateInviteStatus.Done),
            Button(
              icon = Icons.PaperPlaneTop,
              loading = createInviteStatus.get === CreateInviteStatus.Running,
              disabled =
                !validEmail.value || createInviteStatus.when(_ === CreateInviteStatus.Done),
              onClick = createInviteStatus.set(CreateInviteStatus.Idle) *>
                emailView.get
                  .map: email =>
                    createInvitation(email, key).runAsync
                  .getOrEmpty,
              tooltip = "Send",
              label = "Invite"
            ).compact.when(createInviteStatus.get =!= CreateInviteStatus.Done)
          )
        )
      ).addModifiers(Seq(ExploreStyles.CompactOverlayPanel, ExploreStyles.InviteUserPopup))
        .withRef(props.overlayRef.ref)
    }
  )

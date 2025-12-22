// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import explore.common.UserPreferencesQueries
import explore.components.ConnectionsStatus
import explore.components.ui.ExploreStyles
import explore.events.ExploreEvent
import explore.model.AppContext
import explore.model.Focused
import explore.model.GlobalPreferences
import explore.model.ProgramInfoList
import explore.model.ProgramSummaries
import explore.model.enums.AppTab
import explore.programs.ProgramsPopup
import explore.undo.UndoStacks
import explore.users.RedeemInvitationsPopup
import explore.users.UserPreferencesPopup
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ExecutionEnvironment
import lucuma.core.model.GuestRole
import lucuma.core.model.Program
import lucuma.core.model.StandardRole
import lucuma.core.util.NewBoolean
import lucuma.react.common.*
import lucuma.react.fa.FontAwesomeIcon
import lucuma.react.primereact.Button
import lucuma.react.primereact.Image
import lucuma.react.primereact.MenuItem
import lucuma.react.primereact.PopupTieredMenu
import lucuma.react.primereact.Toolbar
import lucuma.react.primereact.hooks.all.*
import lucuma.refined.*
import lucuma.ui.Resources
import lucuma.ui.components.About
import lucuma.ui.components.LoginStyles
import lucuma.ui.components.ThemeSubMenu
import lucuma.ui.enums.Theme
import lucuma.ui.layout.LayoutStyles
import lucuma.ui.primereact.LucumaPrimeStyles
import lucuma.ui.sso.UserVault
import lucuma.ui.syntax.all.given
import org.scalajs.dom.window
import org.typelevel.log4cats.extras.LogLevel
import org.typelevel.log4cats.extras.LogLevel.logLevelOrder

import scala.scalajs.LinkingInfo

case class TopBar(
  vault:                      View[UserVault],
  programId:                  Option[Program.Id],
  programOrProposalReference: Option[String],
  undoStacks:                 View[UndoStacks[IO, ProgramSummaries]],
  programInfos:               ViewOpt[ProgramInfoList],
  theme:                      View[Theme],
  onLogout:                   IO[Unit],
  onRoleChange:               StandardRole => IO[Unit],
  globalPreferences:          View[GlobalPreferences]
) extends ReactFnProps(TopBar.component)

object TopBar:
  private type Props = TopBar

  private object IsAboutOpen             extends NewBoolean
  private object IsProgramOpen           extends NewBoolean
  private object IsUserPropertiesOpen    extends NewBoolean
  private object IsReedemInvitationsOpen extends NewBoolean

  private val component =
    ScalaFnComponent[Props]: props =>
      for {
        ctx                     <- useContext(AppContext.ctx)
        isProgramsOpen          <- useState(IsProgramOpen(false))
        isAboutOpen             <- useStateView(IsAboutOpen(false))
        isUserPropertiesOpen    <- useState(IsUserPropertiesOpen(false))
        isReedemInvitationsOpen <- useState(IsReedemInvitationsOpen(false))
        menuRef                 <- usePopupMenuRef
      } yield
        import ctx.given

        val user  = props.vault.get.user
        val role  = user.role
        val level = props.globalPreferences.get.logLevel

        def logout: IO[Unit] = ctx.sso.logout >> props.onLogout

        def setLogLevel(l: LogLevel): Callback =
          UserPreferencesQueries.LogLevelPreference
            .updateLogLevel[IO](user.id, l)
            .runAsync

        val recentPrograms =
          val stored = props.globalPreferences.get.lastOpenPrograms
          props.programId.fold(stored)(pid => pid :: stored.filterNot(_ === pid))

        val recentProgramsItem: List[MenuItem] =
          if recentPrograms.isEmpty then List.empty
          else
            val x =
              recentPrograms
                .map: programId =>
                  val progRef = (AppTab.Observations, programId, Focused.None).some
                  val name    =
                    for {
                      pis <- props.programInfos.get
                      p   <- pis.get(programId)
                      n   <- p.name
                    } yield n.value

                  val currentProg = props.programId.exists(_ === programId)

                  MenuItem.Custom(
                    <.a(
                      LucumaPrimeStyles.MenuItemLink,
                      ^.href := ctx.pageUrl(progRef),
                      ^.onClick ==> (e =>
                        e.preventDefaultCB >> e.stopPropagationCB >>
                          (menuRef.hide(e) >> ctx.pushPage(progRef)).unless_(currentProg)
                      )
                    )(
                      <.div(ExploreStyles.RecentProgramId)(programId.show),
                      name.map(n => <.div(ExploreStyles.RecentProgramName)(n))
                    ),
                    clazz = ExploreStyles.RecentProgramLink |+|
                      LucumaPrimeStyles.Disabled.when_(currentProg)
                  )
            List(
              MenuItem.SubMenu(
                label = "Recent Progs",
                icon = Icons.ListRadio
              )(x*)
            )

        val firstItems =
          MenuItem.Item(
            label = "About Explore",
            icon = Icons.Info,
            command = isAboutOpen.set(IsAboutOpen(true))
          ) +:
            (if props.programId.isDefined then
               List(
                 MenuItem.Item(
                   label = "Manage Programs",
                   icon = Icons.ListCheck,
                   command = isProgramsOpen.setState(IsProgramOpen(true))
                 )
               ) ::: recentProgramsItem
             else recentProgramsItem)

        val lastCommonItems = List(
          MenuItem.Separator.some,
          MenuItem
            .Item(
              label = "Login with ORCID",
              icon = Image(
                src = Resources.OrcidLogo,
                clazz = ExploreStyles.OrcidIconMenu |+| LoginStyles.LoginOrcidIcon
              ),
              visible = role === GuestRole,
              command = ctx.sso.switchToORCID.runAsync
            )
            .some,
          MenuItem.Item(label = "Logout", icon = Icons.Logout, command = logout.runAsync).some
        ).flattenOption

        val lastItems =
          lastCommonItems :::
            (if LinkingInfo.developmentMode then
               List(
                 // Used to test pwa toast via message, you can't see it on the same tab
                 // Click on one but will be visible in other explore tabs
                 MenuItem
                   .Item(
                     label = "Test PWA Toast",
                     icon = Icons.CloudArrowUp,
                     command =
                       ctx.broadcastChannel.postMessage(ExploreEvent.PWATestToast).runAsyncAndForget
                   )
                   .some,
                 MenuItem
                   .SubMenu(
                     label = "Log Level",
                     icon = Icons.BarCodeRead,
                     visible =
                       ctx.environment =!= ExecutionEnvironment.Production && role =!= GuestRole
                   )(
                     MenuItem.Item(
                       label = "Info",
                       command = setLogLevel(LogLevel.Info),
                       disabled = level === LogLevel.Info,
                       icon = Icons.Info
                     ),
                     MenuItem.Item(
                       label = "Debug",
                       command = setLogLevel(LogLevel.Debug),
                       disabled = level === LogLevel.Debug,
                       icon = Icons.Bug
                     ),
                     MenuItem.Item(
                       label = "Trace",
                       command = setLogLevel(LogLevel.Trace),
                       disabled = level === LogLevel.Trace,
                       icon = Icons.Pencil
                     )
                   )
                   .some,
                 ThemeSubMenu(props.theme).some
               ).flattenOption
             else List.empty)

        val menuItems =
          if role =!= GuestRole then
            firstItems :::
              List(
                MenuItem
                  .Item(
                    label = "User Preferences",
                    icon = Icons.UserGears,
                    command = isUserPropertiesOpen.setState(IsUserPropertiesOpen(true))
                  ),
                MenuItem
                  .Item(
                    label = "Redeem invitations",
                    icon = Icons.UserGroupSimple,
                    command = isReedemInvitationsOpen.setState(IsReedemInvitationsOpen(true))
                  ),
                MenuItem
                  .Item(
                    label = "User Manual",
                    icon = Icons.BookOpen,
                    command = Callback(
                      window.open("https://www.gemini.edu/files/software/gpp/Explore_Manual.pdf",
                                  "_blank"
                      )
                    )
                  ),
                MenuItem
                  .Item(
                    label = "Help Desk",
                    icon = Icons.ArrowUpRightFromSquare,
                    command = Callback(
                      window.open("https://noirlab.atlassian.net/servicedesk/customer/portal/12",
                                  "_blank"
                      )
                    )
                  )
              ) ::: lastItems
          else firstItems ::: lastItems

        React.Fragment(
          Toolbar(
            clazz = LayoutStyles.MainHeader,
            left = React.Fragment(
              <.span(LayoutStyles.MainTitle, s"Explore"),
              props.programOrProposalReference.map: r =>
                React.Fragment(<.span(LayoutStyles.MainTitle, "- "),
                               <.span(ExploreStyles.MainTitleProgramId, r)
                )
            ),
            right = React.Fragment(
              <.span(LayoutStyles.MainUserName)(user.displayName),
              RoleSwitch(props.vault, ctx.sso, props.onRoleChange),
              ConnectionsStatus(),
              Button(
                icon = Icons.Bars,
                text = true,
                severity = Button.Severity.Secondary,
                onClickE = menuRef.toggle
              )
            )
          ),
          PopupTieredMenu(model = menuItems, clazz = ExploreStyles.TopMenu).withRef(menuRef.ref),
          if isAboutOpen.get.value then
            About(
              "Explore".refined,
              ExploreStyles.LoginTitle,
              ctx.version,
              isAboutOpen.as(IsAboutOpen.Value)
            )
          else EmptyVdom,
          if isProgramsOpen.value.value then
            ProgramsPopup(
              props.programId,
              user.id,
              props.vault.get.isStaff,
              props.programInfos,
              props.undoStacks,
              isProgramsOpen.setState(IsProgramOpen(false)).some
            )
          else EmptyVdom,
          if isUserPropertiesOpen.value.value then
            UserPreferencesPopup(
              props.vault.get,
              isUserPropertiesOpen.setState(IsUserPropertiesOpen(false)).some,
              props.globalPreferences.zoom(GlobalPreferences.wavelengthUnits)
            )
          else EmptyVdom,
          if isReedemInvitationsOpen.value.value then
            RedeemInvitationsPopup(
              props.vault.get,
              isReedemInvitationsOpen.setState(IsReedemInvitationsOpen(false)).some
            )
          else EmptyVdom
        )

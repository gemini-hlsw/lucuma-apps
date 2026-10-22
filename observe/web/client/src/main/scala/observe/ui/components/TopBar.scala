// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.components

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.StandardRole
import lucuma.core.model.StandardUser
import lucuma.core.util.NewBoolean
import lucuma.react.common.*
import lucuma.react.primereact.Button
import lucuma.react.primereact.MenuItem
import lucuma.react.primereact.PopupTieredMenu
import lucuma.react.primereact.Toolbar
import lucuma.react.primereact.hooks.all.*
import lucuma.refined.*
import lucuma.ui.components.About
import lucuma.ui.components.ThemeSubMenu
import lucuma.ui.enums.Theme
import lucuma.ui.layout.LayoutStyles
import lucuma.ui.reusability.given
import lucuma.ui.sso.UserVault
import lucuma.ui.syntax.all.given
import observe.model.ClientConfig
import observe.ui.Icons
import observe.ui.ObserveStyles
import observe.ui.model.AppContext
import observe.ui.model.IsAudioActivated
import observe.ui.utils.Audio

case class TopBar(
  clientConfig:     ClientConfig,
  isServerBusy:     Boolean,
  vault:            View[UserVault],
  theme:            View[Theme],
  isAudioActivated: View[IsAudioActivated],
  onLogout:         IO[Unit]
) extends ReactFnProps(TopBar)

object TopBar
    extends ReactFnComponent[TopBar](props =>
      object IsAboutOpen   extends NewBoolean
      object IsSystemsOpen extends NewBoolean

      // Only staff may change subsystem control modes or restart the server.
      val isStaff: Boolean = props.vault.get.user match
        case StandardUser(_, StandardRole.Staff(_), _, _) => true
        case _                                            => false

      for
        ctx           <- useContext(AppContext.ctx)
        isAboutOpen   <- useStateView(IsAboutOpen(false))
        isSystemsOpen <- useStateView(IsSystemsOpen(false))
        menuRef       <- usePopupMenuRef
        // Rebuild the menu when the theme or audio activation change.
        menuItems     <-
          useMemo((props.theme.get, props.isAudioActivated.get, isStaff)): (_, _, _) =>
            import ctx.given

            def logout: IO[Unit] = ctx.ssoClient.logout >> props.onLogout

            val audioSubMenu =
              MenuItem.SubMenu(
                label = "Sounds",
                icon = Icons.Volume
              )(
                MenuItem.Item(
                  label = "On",
                  icon = Icons.Volume,
                  disabled = props.isAudioActivated.get,
                  command = props.isAudioActivated.set(IsAudioActivated.True) >>
                    Audio.SoundActivated.play.runAsyncAndForget
                ),
                MenuItem.Item(
                  label = "Off",
                  icon = Icons.VolumeSlash,
                  disabled = !props.isAudioActivated.get,
                  command = props.isAudioActivated.set(IsAudioActivated.False)
                )
              )

            val firstItems = List(
              audioSubMenu,
              ThemeSubMenu(props.theme)
            )

            val subsystemsItem: Option[MenuItem] = Option.when(isStaff):
              MenuItem.Item(
                label = "Subsystems...",
                icon = Icons.Gears,
                command = isSystemsOpen.set(IsSystemsOpen(true))
              )

            val lastItems = subsystemsItem.toList ::: List(
              MenuItem.Item(
                label = "About Observe",
                icon = Icons.CircleInfo,
                command = isAboutOpen.set(IsAboutOpen(true))
              ),
              MenuItem.Item(
                label = "Logout",
                icon = Icons.Logout,
                command = logout.runAsync
              )
            )

            (firstItems :+ MenuItem.Separator) ::: lastItems
      yield
        val user = props.vault.get.user

        React.Fragment(
          Toolbar(
            clazz = LayoutStyles.MainHeader,
            left = <.span(LayoutStyles.MainTitle, "Observe"),
            right = React.Fragment(
              if (
                isStaff && props.clientConfig.subsystemControlModes =!= props.clientConfig.savedSubsystemControlModes
              )
                <.span(
                  ObserveStyles.RestartPending,
                  ^.title := "Restart pending: subsystem control changes are saved but not applied",
                  ^.onClick --> isSystemsOpen.set(IsSystemsOpen(true))
                )(Icons.TriangleExclamation, " Restart pending")
              else EmptyVdom,
              <.span(props.clientConfig.site.shortName),
              " - ",
              <.span(LayoutStyles.MainUserName)(user.displayName),
              Button(
                icon = Icons.Bars,
                text = true,
                severity = Button.Severity.Secondary,
                onClickE = menuRef.toggle
              )
            )
          ),
          PopupTieredMenu(model = menuItems).withRef(menuRef.ref),
          SubsystemControlModesDialog(
            props.clientConfig,
            props.isServerBusy,
            isStaff && isSystemsOpen.get.value,
            isSystemsOpen.set(IsSystemsOpen(false))
          ),
          if (isAboutOpen.get.value)
            About(
              "Observe".refined,
              ObserveStyles.LoginTitle,
              NonEmptyString
                .unsafeFrom(
                  s"${ctx.version.value} / Server: ${props.clientConfig.version.value.value}"
                ),
              isAboutOpen.as(IsAboutOpen.Value)
            )
          else
            EmptyVdom
        )
    )

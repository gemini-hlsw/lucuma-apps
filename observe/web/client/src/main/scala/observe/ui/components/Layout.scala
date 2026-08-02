// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.components

import cats.effect.IO
import cats.syntax.all.*
import clue.PersistentClientStatus
import crystal.Pot
import crystal.react.*
import crystal.react.hooks.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.*
import lucuma.react.primereact.*
import lucuma.refined.*
import lucuma.ui.components.SideTabs
import lucuma.ui.components.SolarProgress
import lucuma.ui.enums.Theme
import lucuma.ui.hooks.*
import lucuma.ui.layout.LayoutStyles
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.sso.UserVault
import observe.model.ClientConfig
import observe.model.enums.ObserveLogLevel
import observe.ui.ObserveStyles
import observe.ui.components.sequence.GuideConfigStatus
import observe.ui.model.AppContext
import observe.ui.model.Page
import observe.ui.model.RootModel
import observe.ui.model.RootModelData
import observe.ui.model.UserPreferences
import observe.ui.model.enums.AppTab

import java.time.ZoneOffset

case class Layout(c: RouterCtl[Page], resolution: ResolutionWithProps[Page, RootModel])(
  val rootModel: RootModel
) extends ReactFnProps(Layout)

object Layout
    extends ReactFnComponent[Layout](props =>
      for
        ctx          <- useContext(AppContext.ctx)
        odbStatus    <- useStreamOnMount(ctx.odbClient.statusStream)
        theme        <- useTheme(initial = props.rootModel.data.get.userPreferences.theme)
        _            <- useEffectWithDeps(theme.get): t =>
                          props.rootModel.data
                            .zoom(RootModelData.userPreferences.andThen(UserPreferences.theme))
                            .set(t)
        bootstrapped <- useState(Bootstrapped.False)
        ready         = odbStatus.contains_(PersistentClientStatus.Connected) &&
                          props.rootModel.clientConfig.isReady
        _            <- useEffectWithDeps(ready):
                          case r => bootstrapped.setState(Bootstrapped.True).when_(r)
      yield
        val userPrefs       = props.rootModel.data.zoom(RootModelData.userPreferences)
        val logDisplayLevel = userPrefs.zoom(UserPreferences.logLevel)
        val showUT          = userPrefs.zoom(UserPreferences.logTimeIsUTC)
        val appTab: AppTab  = AppTab.from(props.resolution.page)

        val appTabView: View[AppTab] =
          View(
            appTab,
            (mod, cb) =>
              val newTab = mod(appTab)
              ctx.pushPage(newTab) >> cb(appTab, newTab)
          )

        def logTimezone(clientConfig: ClientConfig) =
          if (showUT.get) ZoneOffset.UTC else clientConfig.site.timezone

        // Show the full-screen loader only until the first successful connection
        if (bootstrapped.value || ready)
          React.StrictMode(
            <.div(ObserveStyles.AppShell)(
              <.div(LayoutStyles.MainGrid)(
                props.rootModel.data
                  .zoom(RootModelData.userVault)
                  .zoom(Pot.readyPrism.some)
                  .mapValue: (userVault: View[UserVault]) =>
                    props.rootModel.clientConfig.toOption.map: clientConfig =>
                      TopBar(
                        clientConfig,
                        userVault,
                        theme,
                        props.rootModel.data.zoom(RootModelData.isAudioActivated),
                        props.rootModel.data.zoom(RootModelData.userVault).set(Pot(none)).toAsync
                      ),
                Toast(Toast.Position.BottomRight, baseZIndex = 2000).withRef(ctx.toastRef.ref),
                SideTabs(
                  "side-tabs".refined,
                  appTabView,
                  ctx.pageUrl(_),
                  separatorAfter = {
                    case AppTab.ObsList => true
                    case _              => false
                  },
                  filterPred = {
                    case AppTab.LoadedObs(instrument) =>
                      props.rootModel.data.get.readyObsByInstrument.contains(instrument)
                    case _                            => true
                  }
                ),
                <.div(LayoutStyles.MainBody)(
                  <.div(ObserveStyles.MainBodyLogWrapper)(
                    <.div(ObserveStyles.MainBodyContent)(
                      props.resolution.renderP(props.rootModel)
                    ),
                    // Always-visible log accordion, shown regardless of the active tab.
                    props.rootModel.clientConfig.toOption.map: clientConfig =>
                      Accordion(
                        clazz = ObserveStyles.LogArea,
                        tabs = List(
                          AccordionTab(
                            header = <.div(ObserveStyles.LogHeaderRow)(
                              <.span("Show Log"),
                              <.span(
                                ObserveStyles.LogHeaderControls,
                                ^.onClick ==> (_.stopPropagationCB)
                              )(
                                <.span(ObserveStyles.LogTimezoneSwitch)(
                                  <.span("Local Time"),
                                  InputSwitch(checked = showUT.get, onChange = showUT.set),
                                  <.span("UTC")
                                ),
                                FormEnumDropdownView(
                                  id = "log-level".refined,
                                  value = logDisplayLevel,
                                  exclude = Set(ObserveLogLevel.Error)
                                )
                              )
                            )
                          )(
                            <.div(ObserveStyles.LogAreaContent)(
                              LogArea(
                                logTimezone(clientConfig),
                                props.rootModel.data.get.globalLog,
                                logDisplayLevel
                              )
                            )
                          )
                        )
                      )
                  )
                ),
                // Always-visible guide-config toolbar pinned to the bottom of the app.
                Toolbar(
                  clazz = ObserveStyles.GuideBar,
                  left = GuideConfigStatus(
                    props.rootModel.data.zoom(RootModelData.guideConfig).get.tcsGuide
                  )
                )
              )
            )
          )
        else
          SolarProgress()
    )

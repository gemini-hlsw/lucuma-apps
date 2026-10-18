// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.components

import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Site
import lucuma.core.util.Display
import lucuma.react.common.*
import lucuma.react.fa.IconSize
import lucuma.react.primereact.Button
import lucuma.react.primereact.ConfirmDialog
import lucuma.react.primereact.Dialog
import lucuma.react.primereact.DialogPosition
import lucuma.react.primereact.PrimeStyles
import lucuma.react.primereact.Tooltip
import lucuma.react.primereact.tooltip.*
import lucuma.ui.primereact.EnumDropdownView
import lucuma.ui.primereact.FormLabel
import lucuma.ui.primereact.LucumaPrimeStyles
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given
import observe.model.ClientConfig
import observe.model.config.SubsystemControlModes
import observe.model.enums.SubsystemControlMode
import observe.ui.Icons
import observe.ui.ObserveStyles
import observe.ui.model.AppContext
import observe.ui.services.ConfigApi

private given Reusability[SubsystemControlModes] = Reusability.byEq

case class SubsystemControlModesDialog(
  clientConfig: ClientConfig,
  isServerBusy: Boolean,
  visible:      Boolean,
  onHide:       Callback
) extends ReactFnProps(SubsystemControlModesDialog)

object SubsystemControlModesDialog
    extends ReactFnComponent[SubsystemControlModesDialog](props =>
      for
        ctx       <- useContext(AppContext.ctx)
        configApi <- useContext(ConfigApi.ctx)
        edited    <- useStateView(props.clientConfig.savedSubsystemControlModes)
        // Reset the local copy whenever the saved configuration changes.
        _         <- useEffectWithDeps(props.clientConfig.savedSubsystemControlModes): saved =>
                       edited.set(saved)
      yield
        import ctx.given

        val config: ClientConfig                                                            = props.clientConfig
        val site: Site                                                                      = config.site
        val fields: List[SubsystemControlModes.Field]                                       =
          SubsystemControlModes.fieldsFor(site)
        val hasChanges: Boolean                                                             = edited.get =!= config.savedSubsystemControlModes
        val pendingChanges
          : List[(SubsystemControlModes.Field, SubsystemControlMode, SubsystemControlMode)] =
          config.subsystemControlModes.diff(config.savedSubsystemControlModes, site)
        val restartRequired: Boolean                                                        = pendingChanges.nonEmpty

        def label(s: SubsystemControlMode): String = Display[SubsystemControlMode].shortName(s)

        val restart: Callback =
          ConfirmDialog.confirmDialog(
            message = <.div(
              "All clients will disconnect and reconnect once the server is back. Restart the server now?"
            ),
            header = "Restart server",
            acceptLabel = "Yes, restart",
            position = DialogPosition.Top,
            accept = configApi.restartServer.runAsync,
            icon = Icons.TriangleExclamation
          )

        Dialog(
          header = "Subsystems control",
          visible = props.visible,
          onHide = props.onHide,
          position = DialogPosition.Top,
          clazz = ObserveStyles.SubsystemControlModesDialog
        )(
          <.div(
            fields.map { field =>
              val row: View[SubsystemControlMode] = edited.zoom(field.mode)
              val active: SubsystemControlMode    = field.mode.get(config.subsystemControlModes)
              val id: NonEmptyString              =
                NonEmptyString.unsafeFrom(s"subsystems-${field.configKey}")
              // Same marker Explore puts next to a customized value, as an input group addon.
              val activeAddon: VdomNode           =
                if active =!= row.get then
                  <.span(
                    PrimeStyles.InputGroupAddon,
                    Icons.DiamondExclamation
                      .withClass(ObserveStyles.SystemsControlActive)
                      .withSize(IconSize.X1)
                  ).withTooltip(
                    content = s"Active: ${label(active)}",
                    position = Tooltip.Position.Left
                  )
                else EmptyVdom
              <.div(ObserveStyles.SystemsControlRow)(
                FormLabel(htmlFor = id)(field.label),
                <.div(PrimeStyles.InputGroup |+| LucumaPrimeStyles.FormField)(
                  EnumDropdownView(id = id, value = row, clazz = LucumaPrimeStyles.FormField),
                  activeAddon
                )
              )
            }.toTagMod,
            <.div(ObserveStyles.SystemsControlActions)(
              Button(
                label = "Save",
                disabled = !hasChanges,
                onClick = configApi.saveSystemControl(edited.get).runAsync
              )
            ),
            <.div(ObserveStyles.SystemsControlRestart)(
              if (restartRequired)
                React.Fragment(
                  <.h4(ObserveStyles.SystemsControlWarning)(
                    Icons.TriangleExclamation,
                    " Restart required"
                  ),
                  <.ul(
                    pendingChanges
                      .map: (f, from, to) =>
                        <.li(s"${f.label}: ${label(from)} → ${label(to)}")
                      .toTagMod
                  )
                )
              else <.h4("Restart server"),
              <.p(
                "The restart is refused while a sequence is running. All clients disconnect and reconnect once the server is back."
              ),
              <.div(ObserveStyles.SystemsControlActions)(
                Button(
                  label = "Restart server",
                  severity = Button.Severity.Danger,
                  disabled = props.isServerBusy,
                  tooltip = if (props.isServerBusy) "A sequence is running" else "",
                  onClick = restart
                )
              )
            ),
            ConfirmDialog()
          )
        )
    )

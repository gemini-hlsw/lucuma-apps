// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.effect.IO
import crystal.react.hooks.*
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.schemas.model.ObservingMode
import lucuma.ui.syntax.all.given

object GmosMosConfigPanel:

  final case class GmosNorthMos(
    observingMode: ObservingMode.GmosNorthMos,
    revertConfig:  IO[Unit],
    permissions:   ConfigEditPermissions
  ) extends ReactFnProps(GmosNorthMos)

  object GmosNorthMos
      extends ReactFnComponent[GmosNorthMos](props =>
        TemporaryConfigPanel("GMOS-N MOS", props.revertConfig, props.permissions)
      )

  final case class GmosSouthMos(
    observingMode: ObservingMode.GmosSouthMos,
    revertConfig:  IO[Unit],
    permissions:   ConfigEditPermissions
  ) extends ReactFnProps(GmosSouthMos)

  object GmosSouthMos
      extends ReactFnComponent[GmosSouthMos](props =>
        TemporaryConfigPanel("GMOS-S MOS", props.revertConfig, props.permissions)
      )

  private case class TemporaryConfigPanel(
    label:        String,
    revertConfig: IO[Unit],
    permissions:  ConfigEditPermissions
  ) extends ReactFnProps(TemporaryConfigPanel)

  private object TemporaryConfigPanel
      extends ReactFnComponent[TemporaryConfigPanel](props =>
        useStateView(ConfigEditState.View).map: editState =>
          React.Fragment(
            <.div(ExploreStyles.VisitorUpperGrid)(
              s"${props.label} configuration placeholder"
            ),
            <.div(
              ExploreStyles.VisitorLowerGrid,
              AdvancedConfigButtons(
                editState = editState,
                isCustomized = false,
                revertConfig = props.revertConfig,
                revertCustomizations = Callback.empty,
                sequenceChanged = Callback.empty,
                readonly = !props.permissions.isFullEdit,
                showAdvancedButton = false,
                showCustomizeButton = false
              )
            )
          )
      )

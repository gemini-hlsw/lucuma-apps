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

// Temporary. MOS modes have no editor yet; this panel exists only so that an
// observation with a MOS mode assigned still offers the revert-configuration
// button. The editing ticket replaces this file wholesale.
object GmosMosConfigPanel:

  case class GmosNorthMos(
    observingMode: ObservingMode.GmosNorthMos,
    revertConfig:  IO[Unit],
    permissions:   ConfigEditPermissions
  ) extends ReactFnProps(GmosNorthMos)

  object GmosNorthMos
      extends ReactFnComponent[GmosNorthMos](props =>
        MosPlaceholder("GMOS-N MOS", props.revertConfig, props.permissions)
      )

  case class GmosSouthMos(
    observingMode: ObservingMode.GmosSouthMos,
    revertConfig:  IO[Unit],
    permissions:   ConfigEditPermissions
  ) extends ReactFnProps(GmosSouthMos)

  object GmosSouthMos
      extends ReactFnComponent[GmosSouthMos](props =>
        MosPlaceholder("GMOS-S MOS", props.revertConfig, props.permissions)
      )

  private case class MosPlaceholder(
    label:        String,
    revertConfig: IO[Unit],
    permissions:  ConfigEditPermissions
  ) extends ReactFnProps(MosPlaceholder)

  private object MosPlaceholder
      extends ReactFnComponent[MosPlaceholder](props =>
        useStateView(ConfigEditState.View).map: editState =>
          React.Fragment(
            <.div(ExploreStyles.VisitorUpperGrid)(
              s"${props.label} — editing is not yet supported"
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

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

// Read-only: the ODB does not generate a sequence for F2 MOS yet.
case class Flamingos2MosConfigPanel(
  observingMode: ObservingMode.Flamingos2Mos,
  revertConfig:  IO[Unit],
  permissions:   ConfigEditPermissions
) extends ReactFnProps(Flamingos2MosConfigPanel)

object Flamingos2MosConfigPanel
    extends ReactFnComponent[Flamingos2MosConfigPanel](props =>
      useStateView(ConfigEditState.View).map: editState =>
        val mode = props.observingMode

        React.Fragment(
          <.div(ExploreStyles.VisitorUpperGrid)(
            s"Flamingos2 MOS Under construction"
          ),
          <.div(
            ExploreStyles.VisitorLowerGrid,
            AdvancedConfigButtons(
              editState = editState,
              isCustomized = mode.isCustomized,
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

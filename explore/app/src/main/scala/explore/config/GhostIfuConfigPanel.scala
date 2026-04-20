// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import crystal.react.hooks.*
import explore.common.Aligner
import explore.model.AppContext
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.schemas.ObservationDB.Types.GhostIfuInput
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*

final case class GhostIfuConfigPanel(
  observingMode:   Aligner[ObservingMode.GhostIfu, GhostIfuInput],
  revertConfig:    Callback,
  sequenceChanged: Callback,
  permissions:     ConfigEditPermissions
) extends ReactFnProps(GhostIfuConfigPanel)

object GhostIfuConfigPanel
    extends ReactFnComponent[GhostIfuConfigPanel](props =>
      for
        ctx       <- useContext(AppContext.ctx)
        editState <- useStateView(ConfigEditState.View)
      yield
        import ctx.given

        <.div(
          <.h1("You've been GHOSTed!"),
          <.div(props.observingMode.get.toString),
          AdvancedConfigButtons(
            editState = editState,
            isCustomized = false, // props.observingMode.get.isCustomized,
            revertConfig = props.revertConfig,
            revertCustomizations = props.observingMode.view(_.toInput).mod(_.revertCustomizations),
            sequenceChanged = props.sequenceChanged,
            readonly = !props.permissions.isFullEdit,
            showAdvancedButton = false
          )
        )
    )

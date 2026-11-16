// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.effect.IO
import clue.data.syntax.*
import crystal.react.*
import crystal.react.hooks.*
import explore.common.Aligner
import explore.components.ui.ExploreStyles
import explore.config.offsets.SlitTelescopeConfigsEditor
import explore.model.AppContext
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ScorpioFpu
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given

final case class ScorpioLongSlitConfigPanel(
  observingMode:   Aligner[ObservingMode.ScorpioLongSlit, ScorpioLongSlitInput],
  revertConfig:    Option[IO[Unit]],
  sequenceChanged: Callback,
  permissions:     ConfigEditPermissions
) extends ReactFnProps(ScorpioLongSlitConfigPanel)

object ScorpioLongSlitConfigPanel
    extends ReactFnComponent[ScorpioLongSlitConfigPanel](props =>
      for
        ctx       <- useContext(AppContext.ctx)
        editState <- useStateView(ConfigEditState.View)
      yield
        import ctx.given

        val readonly: Boolean = !props.permissions.isFullEdit

        val fpuView: View[ScorpioFpu] = props.observingMode
          .zoom(ObservingMode.ScorpioLongSlit.fpu, ScorpioLongSlitInput.fpu.modify)
          .view(_.assign)

        val explicitTelescopeConfigsView: View[Option[SlitTelescopeConfigs]] = props.observingMode
          .zoom(
            ObservingMode.ScorpioLongSlit.explicitTelescopeConfigs,
            ScorpioLongSlitInput.explicitTelescopeConfigs.modify
          )
          .view(_.map(_.toInput).orUnassign)

        React.Fragment(
          <.div(ExploreStyles.Igrins2UpperGrid)(
            <.div(LucumaPrimeStyles.FormColumnCompact)(
              FormEnumDropdownView(
                id = "scorpio-fpu".refined,
                value = fpuView,
                label = "FPU",
                disabled = readonly
              )
            ),
            <.div(LucumaPrimeStyles.FormColumnCompact, ExploreStyles.SlitTelescopeConfigEditor)(
              SlitTelescopeConfigsEditor(
                explicitValue = explicitTelescopeConfigsView,
                defaultValue = props.observingMode.get.defaultTelescopeConfigs,
                presets = props.observingMode.get.telescopeConfigPresets,
                helpId = "configuration/slit-spatial-offsets.md".refined,
                presetsReadonly = readonly,
                editingReadonly = readonly
              )
            )
          ),
          <.div(ExploreStyles.Igrins2LowerGrid)(
            AdvancedConfigButtons(
              editState = editState,
              isCustomized = props.observingMode.get.isCustomized,
              revertConfig = props.revertConfig,
              revertCustomizations =
                props.observingMode.view(_.toInput).mod(_.revertCustomizations),
              sequenceChanged = props.sequenceChanged,
              readonly = readonly,
              showAdvancedButton = false,
              showCustomizeButton = false
            )
          )
        )
    )

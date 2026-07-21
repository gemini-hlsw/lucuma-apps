// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.effect.IO
import cats.syntax.all.*
import clue.data.syntax.*
import crystal.react.View
import crystal.react.hooks.*
import explore.common.Aligner
import explore.components.ui.ExploreStyles
import explore.config.offsets.SlitTelescopeConfigsEditor
import explore.model.AppContext
import explore.model.Observation
import explore.model.enums.WavelengthUnits
import explore.modes.SpectroscopyModesMatrix
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Program
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.sequence.igrins2
import lucuma.core.model.sequence.igrins2.CentralWavelength as Igrins2CentralWavelength
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given

final case class Igrins2LongslitConfigPanel(
  programId:       Program.Id,
  obsId:           Observation.Id,
  calibrationRole: Option[CalibrationRole],
  observingMode:   Aligner[ObservingMode.Igrins2LongSlit, Igrins2LongSlitInput],
  revertConfig:    IO[Unit],
  confMatrix:      SpectroscopyModesMatrix,
  sequenceChanged: Callback,
  permissions:     ConfigEditPermissions,
  units:           WavelengthUnits
) extends ReactFnProps(Igrins2LongslitConfigPanel)

object Igrins2LongslitConfigPanel
    extends ReactFnComponent[Igrins2LongslitConfigPanel](props =>
      for {
        ctx       <- useContext(AppContext.ctx)
        modeData  <- useModeData(props.confMatrix, props.observingMode.get)
        editState <- useStateView(ConfigEditState.View)
      } yield
        import ctx.given

        val disableEdit =
          editState.get =!= ConfigEditState.SimpleEdit && !props.permissions.isFullEdit

        val explicitTelescopeConfigsView: View[Option[SlitTelescopeConfigs]] = props.observingMode
          .zoom(
            ObservingMode.Igrins2LongSlit.explicitTelescopeConfigs,
            Igrins2LongSlitInput.explicitTelescopeConfigs.modify
          )
          .view(_.map(_.toInput).orUnassign)

        val exposureTimeMode: View[ExposureTimeMode] = props.observingMode
          .zoom(
            ObservingMode.Igrins2LongSlit.exposureTimeMode,
            Igrins2LongSlitInput.exposureTimeMode.modify
          )
          .view(_.toInput.assign)

        React.Fragment(
          <.div(
            ExploreStyles.Igrins2UpperGrid
          )(
            <.div(LucumaPrimeStyles.FormColumnCompact)(
              ExposureTimeModeEditor(
                instrument = props.observingMode.get.instrument,
                wavelength = none,
                exposureTimeMode = exposureTimeMode,
                coadds = none,
                scienceMode = ScienceMode.Spectroscopy,
                readonly = !props.permissions.isFullEdit,
                units = props.units,
                calibrationRole = props.calibrationRole,
                idPrefix = "ig2LongSlit".refined
              )
            ),
            <.div(LucumaPrimeStyles.FormColumnCompact)(
              LambdaAndIntervalFormValues(
                modeData = modeData,
                centralWavelength = Igrins2CentralWavelength,
                units = props.units
              )
            ),
            <.div(LucumaPrimeStyles.FormColumnCompact, ExploreStyles.SlitTelescopeConfigEditor)(
              SlitTelescopeConfigsEditor(
                explicitValue = explicitTelescopeConfigsView,
                defaultValue = props.observingMode.get.defaultTelescopeConfigs,
                defaultForPreset = igrins2.defaultSlitTelescopeConfigs,
                helpId = "configuration/igrins2/spatial-offsets.md".refined,
                presetsReadonly = !props.permissions.isFullEdit,
                editingReadonly = disableEdit
              )
            )
          ),
          <.div(
            ExploreStyles.Igrins2LowerGrid,
            AdvancedConfigButtons(
              editState = editState,
              isCustomized = props.observingMode.get.isCustomized,
              revertConfig = props.revertConfig,
              revertCustomizations =
                props.observingMode.view(_.toInput).mod(_.revertCustomizations),
              sequenceChanged = props.sequenceChanged,
              !props.permissions.isFullEdit,
              showAdvancedButton = false
            )
          )
        )
    )

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import clue.data.syntax.*
import crystal.react.View
import crystal.react.hooks.*
import explore.common.Aligner
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Observation
import explore.model.display.given
import explore.model.enums.WavelengthUnits
import explore.modes.SpectroscopyModesMatrix
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Program
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given

final case class Igrins2LongslitConfigPanel(
  programId:       Program.Id,
  obsId:           Observation.Id,
  calibrationRole: Option[CalibrationRole],
  observingMode:   Aligner[ObservingMode.Igrins2LongSlit, Igrins2LongSlitInput],
  revertConfig:    Callback,
  confMatrix:      SpectroscopyModesMatrix,
  sequenceChanged: Callback,
  permissions:     ConfigEditPermissions,
  units:           WavelengthUnits
) extends ReactFnProps(Igrins2LongslitConfigPanel)

object Igrins2LongslitConfigPanel
    extends ReactFnComponent[Igrins2LongslitConfigPanel](props =>
      for
        ctx       <- useContext(AppContext.ctx)
        modeData  <- useModeData(props.confMatrix, props.observingMode.get)
        editState <- useStateView(ConfigEditState.View)
      yield
        import ctx.given

        val disableEdit              =
          editState.get =!= ConfigEditState.SimpleEdit && !props.permissions.isFullEdit
        val showCustomization        = props.calibrationRole.isEmpty
        val allowRevertCustomization = props.permissions.isFullEdit

        val offsetModeView: View[Option[Igrins2OffsetMode]] = props.observingMode
          .zoom(
            ObservingMode.Igrins2LongSlit.explicitOffsetMode,
            Igrins2LongSlitInput.explicitOffsetMode.modify
          )
          .view(_.orUnassign)

        val defaultOffsetMode = props.observingMode.get.defaultOffsetMode

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
              CustomizableEnumSelectOptional(
                id = "offset-mode".refined,
                view = offsetModeView.withDefault(defaultOffsetMode),
                defaultValue = defaultOffsetMode.some,
                label = "Offset Mode".some,
                disabled = disableEdit,
                showCustomization = showCustomization,
                allowRevertCustomization = allowRevertCustomization,
                resetToOriginal = true
              )
            ),
            <.div(LucumaPrimeStyles.FormColumnCompact)(
              ExposureTimeModeEditor(
                props.observingMode.get.instrument.some,
                none,
                exposureTimeMode,
                ScienceMode.Spectroscopy,
                !props.permissions.isFullEdit,
                props.units,
                props.calibrationRole,
                "ig2LongSlit".refined
              )
            ),
            <.div(LucumaPrimeStyles.FormColumnCompact)(
              LambdaAndIntervalFormValues(
                modeData = modeData,
                centralWavelength = BasicConfiguration.Igrins2LongSlit.optimalWavelength,
                units = props.units
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

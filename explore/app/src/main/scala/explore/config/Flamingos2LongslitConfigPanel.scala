// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.effect.IO
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
import explore.model.syntax.all.*
import explore.modes.SpectroscopyModesMatrix
import explore.syntax.ui.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.util.Effect
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Program
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.sequence.flamingos2
import lucuma.core.util.Enumerated
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Panel
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given

final case class Flamingos2LongslitConfigPanel(
  programId:       Program.Id,
  obsId:           Observation.Id,
  calibrationRole: Option[CalibrationRole],
  observingMode:   Aligner[ObservingMode.Flamingos2LongSlit, Flamingos2LongSlitInput],
  revertConfig:    IO[Unit],
  confMatrix:      SpectroscopyModesMatrix,
  sequenceChanged: Callback,
  permissions:     ConfigEditPermissions,
  units:           WavelengthUnits,
  isStaff:         Boolean
) extends ReactFnProps(Flamingos2LongslitConfigPanel)

object Flamingos2LongslitConfigPanel
    extends ReactFnComponent[Flamingos2LongslitConfigPanel](props =>
      for
        ctx       <- useContext(AppContext.ctx)
        modeData  <-
          useModeData(props.confMatrix, props.observingMode.get)
        editState <- useStateView(ConfigEditState.View)
      yield
        import ctx.given

        val disableAdvancedEdit      =
          editState.get =!= ConfigEditState.AdvancedEdit || !props.permissions.isFullEdit
        val disableSimpleEdit        =
          disableAdvancedEdit && editState.get =!= ConfigEditState.SimpleEdit
        val disableAdvancedAcqEdit   = disableAdvancedEdit && !props.permissions.isOnlyForOngoing
        val showCustomization        = props.calibrationRole.isEmpty
        val allowRevertCustomization = props.permissions.isFullEdit
        val showAcquisitionConfig    = props.calibrationRole.needsAcquisitionConfig

        val disperserView: View[Flamingos2Disperser] = props.observingMode
          .zoom(
            ObservingMode.Flamingos2LongSlit.disperser,
            Flamingos2LongSlitInput.disperser.modify
          )
          .view(_.assign)

        val filterView: View[Flamingos2Filter] = props.observingMode
          .zoom(
            ObservingMode.Flamingos2LongSlit.filter,
            Flamingos2LongSlitInput.filter.modify
          )
          .view(_.assign)

        val fpuView: View[Flamingos2Fpu] = props.observingMode
          .zoom(
            ObservingMode.Flamingos2LongSlit.fpu,
            Flamingos2LongSlitInput.fpu.modify
          )
          .view(_.assign)

        val readModeView: View[Option[Flamingos2ReadMode]] = props.observingMode
          .zoom(
            ObservingMode.Flamingos2LongSlit.explicitReadMode,
            Flamingos2LongSlitInput.explicitReadMode.modify
          )
          .view(_.orUnassign)

        val explicitTelescopeConfigsView: View[Option[SlitTelescopeConfigs]] = props.observingMode
          .zoom(
            ObservingMode.Flamingos2LongSlit.explicitTelescopeConfigs,
            Flamingos2LongSlitInput.explicitTelescopeConfigs.modify
          )
          .view(_.map(_.toInput).orUnassign)

        val exposureTimeMode: View[ExposureTimeMode] = props.observingMode
          .zoom(
            ObservingMode.Flamingos2LongSlit.exposureTimeMode,
            Flamingos2LongSlitInput.exposureTimeMode.modify
          )
          .view(_.toInput.assign)

        val deckerView: View[Option[Flamingos2Decker]] = props.observingMode
          .zoom(
            ObservingMode.Flamingos2LongSlit.explicitDecker,
            Flamingos2LongSlitInput.explicitDecker.modify
          )
          .view(_.orUnassign)

        val acquisition: Aligner[ObservingMode.Flamingos2LongSlit.Acquisition,
                                 Flamingos2LongSlitAcquisitionInput
        ] =
          props.observingMode.zoom(
            ObservingMode.Flamingos2LongSlit.acquisition,
            forceAssign(Flamingos2LongSlitInput.acquisition.modify)(
              Flamingos2LongSlitAcquisitionInput()
            )
          )

        val acquisitionExposureTimeView: View[ExposureTimeMode] =
          acquisition
            .zoom(ObservingMode.Flamingos2LongSlit.Acquisition.exposureTimeMode,
                  Flamingos2LongSlitAcquisitionInput.exposureTimeMode.modify
            )
            .view(_.toInput.assign)

        val explicitAcquisitionFilterView: View[Option[Flamingos2Filter]] =
          acquisition
            .zoom(ObservingMode.Flamingos2LongSlit.Acquisition.explicitFilter,
                  Flamingos2LongSlitAcquisitionInput.explicitFilter.modify
            )
            .view(_.orUnassign)

        val defaultAcquisitionFilter =
          props.observingMode.get.acquisition.defaultFilter

        val excludedAcquistionFilters =
          Enumerated[Flamingos2Filter].all.toSet -- Flamingos2Filter.acquisition.toList.toSet

        React.Fragment(
          Flamingos2ConfigFields(
            fpuControl = CustomizableEnumSelect(
              id = "fpu".refined,
              view = fpuView,
              defaultValue = props.observingMode.get.initialFpu,
              label = "FPU".some,
              helpId = Some("configuration/f2/fpu.md".refined),
              disabled = disableSimpleEdit,
              showCustomization = showCustomization,
              allowRevertCustomization = allowRevertCustomization
            ),
            deckerView = deckerView,
            defaultDecker = props.observingMode.get.defaultDecker,
            filterView = filterView,
            initialFilter = props.observingMode.get.initialFilter,
            disperserView = disperserView,
            initialDisperser = props.observingMode.get.initialDisperser,
            readModeView = readModeView,
            exposureTimeMode = exposureTimeMode,
            explicitTelescopeConfigsView = explicitTelescopeConfigsView,
            defaultTelescopeConfigs = props.observingMode.get.defaultTelescopeConfigs,
            defaultForPreset = flamingos2.defaultSlitTelescopeConfigs,
            offsetsHelpId = "configuration/f2/slit-spatial-offsets.md".refined,
            instrument = props.observingMode.get.instrument,
            modeData = modeData,
            units = props.units,
            calibrationRole = props.calibrationRole,
            etmIdPrefix = "f2LongSlit".refined,
            isStaff = props.isStaff,
            disableSimpleEdit = disableSimpleEdit,
            disableAdvancedEdit = disableAdvancedEdit,
            showCustomization = showCustomization,
            allowRevertCustomization = allowRevertCustomization,
            etmReadonly = !props.permissions.isFullEdit,
            presetsReadonly = !props.permissions.isFullEdit
          ),
          <.div(
            ExploreStyles.Flamingos2LowerGrid,
            Panel(
              header = <.span("Acquisition",
                              HelpIcon("configuration/f2/acquisition-customization.md".refined)
              ),
              toggleable = true,
              collapsed = true
            )(
              <.div(
                ExploreStyles.AcquisitionCustomizationGrid,
                <.div(
                  LucumaPrimeStyles.FormColumnCompact,
                  CustomizableEnumSelectOptional(
                    id = "f2-acq-filter".refined,
                    view = explicitAcquisitionFilterView.withDefault(
                      defaultAcquisitionFilter
                    ),
                    defaultValue = defaultAcquisitionFilter.some,
                    label = "Filter".some,
                    helpId = Some("configuration/f2/acquisition-filter.md".refined),
                    exclude = excludedAcquistionFilters,
                    disabled = disableAdvancedAcqEdit,
                    showCustomization = showCustomization,
                    allowRevertCustomization =
                      allowRevertCustomization || props.permissions.isOnlyForOngoing
                  )
                ),
                <.div(
                  LucumaPrimeStyles.FormColumnCompact,
                  ExposureTimeModeEditor(
                    instrument = props.observingMode.get.instrument,
                    wavelength = none,
                    exposureTimeMode = acquisitionExposureTimeView,
                    coadds = none,
                    scienceMode = ScienceMode.Imaging,
                    readonly = props.permissions.isReadonly,
                    units = props.units,
                    calibrationRole = props.calibrationRole,
                    idPrefix = "f2Acq".refined,
                    forceCount = Some(1.refined)
                  )
                )
              )
            ).when(showAcquisitionConfig),
            AdvancedConfigButtons(
              editState = editState,
              isCustomized = props.observingMode.get.isCustomized,
              revertConfig = props.revertConfig,
              revertCustomizations =
                props.observingMode.view(_.toInput).mod(_.revertCustomizations),
              sequenceChanged = props.sequenceChanged,
              !props.permissions.isFullEdit,
              showAdvancedButton = true
            )
          )
        )
    )

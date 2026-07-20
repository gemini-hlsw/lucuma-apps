// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import explore.config.offsets.SlitTelescopeConfigsEditor
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
        import Flamingos2Givens.given

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

        val excludedSpectroscopyFilters =
          Enumerated[Flamingos2Filter].all.filterNot(_.supportsSpectroscopy).toSet

        val defaultDecker = props.observingMode.get.defaultDecker

        React.Fragment(
          <.div(
            ExploreStyles.Flamingos2UpperGrid
          )(
            <.div(LucumaPrimeStyles.FormColumnCompact)(
              CustomizableEnumSelect(
                id = "fpu".refined,
                view = fpuView,
                defaultValue = props.observingMode.get.initialFpu,
                label = "FPU".some,
                helpId = Some("configuration/f2/fpu.md".refined),
                disabled = disableSimpleEdit,
                showCustomization = showCustomization,
                allowRevertCustomization = allowRevertCustomization
              ),
              FormLabel(htmlFor = "decker".refined)("Decker",
                                                    HelpIcon("configuration/f2/decker.md".refined)
              ),
              if (props.isStaff)
                CustomizableEnumSelectOptional(
                  id = "decker".refined,
                  view = deckerView.withDefault(defaultDecker),
                  defaultValue = defaultDecker.some,
                  disabled = disableAdvancedEdit,
                  showCustomization = showCustomization,
                  allowRevertCustomization = allowRevertCustomization
                )
              else
                <.label(^.id := "decker",
                        ExploreStyles.FormValue,
                        deckerView.get.getOrElse(defaultDecker).shortName
                ),
              CustomizableEnumSelect(
                id = "filter".refined,
                view = filterView,
                defaultValue = props.observingMode.get.initialFilter,
                label = "Filter".some,
                exclude = excludedSpectroscopyFilters,
                helpId = Some("configuration/f2/filter.md".refined),
                disabled = disableSimpleEdit,
                showCustomization = showCustomization,
                allowRevertCustomization = allowRevertCustomization
              ),
              CustomizableEnumSelect(
                id = "disperser".refined,
                view = disperserView,
                defaultValue = props.observingMode.get.initialDisperser,
                label = "Disperser".some,
                helpId = Some("configuration/f2/disperser.md".refined),
                disabled = disableSimpleEdit,
                showCustomization = showCustomization,
                allowRevertCustomization = allowRevertCustomization
              ),
              CustomizableEnumSelect(
                id = "read-mode".refined,
                view = readModeView,
                defaultValue = None,
                label = "Read Mode".some,
                helpId = Some("configuration/f2/read-mode.md".refined),
                disabled = disableSimpleEdit,
                showCustomization = showCustomization,
                allowRevertCustomization = allowRevertCustomization
              )
            ),
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
                idPrefix = "f2LongSlit".refined
              ),
              // Per Andy, we'll use the wavelength of the filter as the central wavelength
              LambdaAndIntervalFormValues(
                modeData = modeData,
                centralWavelength = filterView.get.wavelength,
                units = props.units
              )
            ),
            <.div(LucumaPrimeStyles.FormColumnCompact, ExploreStyles.SlitTelescopeConfigEditor)(
              SlitTelescopeConfigsEditor(
                explicitValue = explicitTelescopeConfigsView,
                defaultValue = props.observingMode.get.defaultTelescopeConfigs,
                defaultForPreset = flamingos2.defaultSlitTelescopeConfigs,
                helpId = "configuration/f2/spatial-offsets.md".refined,
                presetsReadonly = !props.permissions.isFullEdit,
                editingReadonly = disableSimpleEdit
              )
            )
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

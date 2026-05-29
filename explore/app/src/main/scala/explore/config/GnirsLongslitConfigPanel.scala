// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.effect.IO
import cats.syntax.all.*
import clue.data.syntax.*
import crystal.react.View
import crystal.react.hooks.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.PosInt
import explore.common.Aligner
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.config.ConfigurationFormats.*
import explore.config.offsets.OffsetInput
import explore.config.offsets.SlitTelescopeConfigsEditor
import explore.model.AppContext
import explore.model.ExploreModelValidators
import explore.model.Observation
import explore.model.enums.WavelengthUnits
import explore.modes.SpectroscopyModesMatrix
import explore.syntax.ui.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.enums.GnirsDecker
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Program
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.sequence.gnirs.GnirsFocusMotorStepsValue
import lucuma.core.model.sequence.gnirs.GnirsGratingWavelength
import lucuma.core.model.sequence.gnirs.defaultSlitTelescopeConfigs
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.validation.*
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Checkbox
import lucuma.react.primereact.Panel
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given

final case class GnirsLongslitConfigPanel(
  programId:       Program.Id,
  obsId:           Observation.Id,
  calibrationRole: Option[CalibrationRole],
  observingMode:   Aligner[ObservingMode.GnirsLongSlit, GnirsLongSlitInput],
  revertConfig:    IO[Unit],
  confMatrix:      SpectroscopyModesMatrix,
  sequenceChanged: Callback,
  permissions:     ConfigEditPermissions,
  isStaffOrAdmin:  Boolean,
  units:           WavelengthUnits
) extends ReactFnProps(GnirsLongslitConfigPanel)

object GnirsLongslitConfigPanel
    extends ReactFnComponent[GnirsLongslitConfigPanel](props =>
      for
        ctx       <- useContext(AppContext.ctx)
        modeData  <- useModeData(props.confMatrix, props.observingMode.get)
        editState <- useStateView(ConfigEditState.View)
      yield
        import ctx.given

        val disableAdvancedEdit: Boolean      =
          editState.get =!= ConfigEditState.AdvancedEdit || !props.permissions.isFullEdit
        val disableSimpleEdit: Boolean        =
          disableAdvancedEdit && editState.get =!= ConfigEditState.SimpleEdit
        val disableAdvancedAcqEdit: Boolean   =
          disableAdvancedEdit && !props.permissions.isOnlyForOngoing
        val showCustomization: Boolean        = props.calibrationRole.isEmpty
        val allowRevertCustomization: Boolean = props.permissions.isFullEdit

        given readModeEnum: Enumerated[Option[GnirsReadMode]] =
          deriveOptionalEnumerated[GnirsReadMode]("Auto")
        given readModeDisplay: Display[Option[GnirsReadMode]] =
          deriveOptionalDisplay[GnirsReadMode]("Auto")

        given acquisitionTypeEnum: Enumerated[Option[GnirsAcquisitionType]] =
          deriveOptionalEnumerated[GnirsAcquisitionType]("Auto")
        given acquisitionTypeDisplay: Display[Option[GnirsAcquisitionType]] =
          deriveOptionalDisplay[GnirsAcquisitionType]("Auto")

        val filterView: View[GnirsFilter] = props.observingMode
          .zoom(
            ObservingMode.GnirsLongSlit.filter,
            GnirsLongSlitInput.filter.modify
          )
          .view(_.assign)

        val deckerView: View[Option[GnirsDecker]] = props.observingMode
          .zoom(
            ObservingMode.GnirsLongSlit.explicitDecker,
            GnirsLongSlitInput.explicitDecker.modify
          )
          .view(_.orUnassign)

        val fpuView: View[GnirsFpuSlit] = props.observingMode
          .zoom(
            ObservingMode.GnirsLongSlit.fpu,
            GnirsLongSlitInput.fpu.modify
          )
          .view(_.assign)

        val prismView: View[GnirsPrism] = props.observingMode
          .zoom(
            ObservingMode.GnirsLongSlit.prism,
            GnirsLongSlitInput.prism.modify
          )
          .view(_.assign)

        val gratingView: View[GnirsGrating] = props.observingMode
          .zoom(
            ObservingMode.GnirsLongSlit.grating,
            GnirsLongSlitInput.grating.modify
          )
          .view(_.assign)

        val gratingWavelengthView: View[Option[GnirsGratingWavelength]] = props.observingMode
          .zoom(
            ObservingMode.GnirsLongSlit.explicitGratingWavelength,
            GnirsLongSlitInput.explicitGratingWavelength.modify
          )
          .view(_.map(_.value.toInput).orUnassign)

        val cameraView: View[GnirsCamera] = props.observingMode
          .zoom(
            ObservingMode.GnirsLongSlit.camera,
            GnirsLongSlitInput.camera.modify
          )
          .view(_.assign)

        val readModeView: View[Option[GnirsReadMode]] = props.observingMode
          .zoom(
            ObservingMode.GnirsLongSlit.explicitReadMode,
            GnirsLongSlitInput.explicitReadMode.modify
          )
          .view(_.orUnassign)

        val wellDepthView: View[Option[GnirsWellDepth]] = props.observingMode
          .zoom(
            ObservingMode.GnirsLongSlit.explicitWellDepth,
            GnirsLongSlitInput.explicitWellDepth.modify
          )
          .view(_.orUnassign)

        val exposureTimeMode: View[ExposureTimeMode] = props.observingMode
          .zoom(
            ObservingMode.GnirsLongSlit.exposureTimeMode,
            GnirsLongSlitInput.exposureTimeMode.modify
          )
          .view(_.toInput.assign)

        val slitTelescopeConfigsView: View[Option[SlitTelescopeConfigs]] = props.observingMode
          .zoom(
            ObservingMode.GnirsLongSlit.explicitTelescopeConfigs,
            GnirsLongSlitInput.explicitTelescopeConfigs.modify
          )
          .view(_.map(_.toInput).orUnassign)

        val focusMotorStepsView: View[Option[GnirsFocusMotorStepsValue]] = props.observingMode
          .zoom(
            ObservingMode.GnirsLongSlit.explicitFocusMotorSteps,
            GnirsLongSlitInput.explicitFocusMotorSteps.modify
          )
          .view(_.map(_.value.value).orUnassign)

        val focusModeView: View[GnirsFocusMode] =
          focusMotorStepsView.zoom(GnirsFocusMode.fromMotorSteps(_))(mod =>
            steps => mod(GnirsFocusMode.fromMotorSteps(steps)).toMotorSteps
          )

        val focusMotorStepsViewOpt: Option[View[GnirsFocusMotorStepsValue]] =
          focusMotorStepsView.toOptionView

        val acquisition
          : Aligner[ObservingMode.GnirsLongSlit.Acquisition, GnirsLongSlitAcquisitionInput] =
          props.observingMode.zoom(
            ObservingMode.GnirsLongSlit.acquisition,
            forceAssign(GnirsLongSlitInput.acquisition.modify)(
              GnirsLongSlitAcquisitionInput()
            )
          )

        val acquisitionTypeView: View[Option[GnirsAcquisitionType]] =
          acquisition
            .zoom(
              ObservingMode.GnirsLongSlit.Acquisition.explicitAcquisitionType,
              GnirsLongSlitAcquisitionInput.explicitAcquisitionType.modify
            )
            .view(_.orUnassign)

        val acquisitionCoaddsView: View[PosInt] =
          acquisition
            .zoom(
              ObservingMode.GnirsLongSlit.Acquisition.coadds,
              GnirsLongSlitAcquisitionInput.coadds.modify
            )
            .view(_.assign)

        val acquisitionFilterView: View[Option[GnirsFilter]] =
          acquisition
            .zoom(
              ObservingMode.GnirsLongSlit.Acquisition.explicitFilter,
              GnirsLongSlitAcquisitionInput.explicitFilter.modify
            )
            .view(_.orUnassign)

        val acquisitionOptOffsetView: View[Option[Offset]] =
          acquisition
            .zoom(
              ObservingMode.GnirsLongSlit.Acquisition.offset,
              GnirsLongSlitAcquisitionInput.skyOffset.modify
            )
            .view(_.map(_.toInput).orUnassign)

        val acquisitionOffsetViewOpt: Option[View[Offset]] =
          acquisitionOptOffsetView.toOptionView

        val acquisitionExposureTimeView: View[ExposureTimeMode] =
          acquisition
            .zoom(
              ObservingMode.GnirsLongSlit.Acquisition.exposureTimeMode,
              GnirsLongSlitAcquisitionInput.exposureTimeMode.modify
            )
            .view(_.toInput.assign)

        val defaultDecker: GnirsDecker            = props.observingMode.get.defaultDecker
        val defaultWellDepth: GnirsWellDepth      = props.observingMode.get.defaultWellDepth
        val defaultAcquisitionFilter: GnirsFilter =
          props.observingMode.get.acquisition.defaultFilter

        React.Fragment(
          <.div(ExploreStyles.GnirsUpperGrid)(
            <.div(LucumaPrimeStyles.FormColumnCompact, ExploreStyles.GnirsConfigEditor)(
              CustomizableEnumSelect(
                id = "filter".refined,
                view = filterView,
                defaultValue = props.observingMode.get.initialFilter,
                label = "Filter".some,
                disabled = disableAdvancedEdit,
                showCustomization = showCustomization,
                allowRevertCustomization = allowRevertCustomization,
                useLongName = true
              ),
              CustomizableEnumSelectOptional(
                id = "decker".refined,
                view = deckerView.withDefault(defaultDecker),
                defaultValue = defaultDecker.some,
                label = "Decker".some,
                disabled = disableSimpleEdit,
                showCustomization = showCustomization,
                allowRevertCustomization = allowRevertCustomization,
                useLongName = true
              ),
              CustomizableEnumSelect(
                id = "fpu".refined,
                view = fpuView,
                defaultValue = props.observingMode.get.initialFpu,
                label = "FPU".some,
                disabled = disableAdvancedEdit,
                showCustomization = showCustomization,
                allowRevertCustomization = allowRevertCustomization,
                useLongName = true
              ),
              CustomizableEnumSelect(
                id = "prism".refined,
                view = prismView,
                defaultValue = props.observingMode.get.initialPrism,
                label = "Prism".some,
                disabled = disableAdvancedEdit,
                showCustomization = showCustomization,
                allowRevertCustomization = allowRevertCustomization
              ),
              CustomizableEnumSelect(
                id = "grating".refined,
                view = gratingView,
                defaultValue = props.observingMode.get.initialGrating,
                label = "Grating".some,
                disabled = disableAdvancedEdit,
                showCustomization = showCustomization,
                allowRevertCustomization = allowRevertCustomization,
                useLongName = true
              ),
              CustomizableInputTextOptional(
                id = "grating-wavelength".refined,
                value = gratingWavelengthView.as(GnirsGratingWavelength.Value.mapping[Option]),
                defaultValue = props.observingMode.get.defaultGratingWavelength.value,
                label = "Wavelength",
                units = props.units.symbol.some,
                validFormat = props.units.toInputWedge,
                changeAuditor = props.units.toAuditor.optional,
                disabled = disableSimpleEdit,
                showCustomization = showCustomization,
                allowRevertCustomization = allowRevertCustomization
              ),
              CustomizableEnumSelect(
                id = "camera".refined,
                view = cameraView,
                defaultValue = props.observingMode.get.initialCamera,
                label = "Camera".some,
                disabled = disableAdvancedEdit,
                showCustomization = showCustomization,
                allowRevertCustomization = allowRevertCustomization,
                useLongName = true
              ),
              CustomizableEnumSelect(
                id = "focus-mode".refined,
                view = focusModeView,
                defaultValue = GnirsFocusMode.Best,
                label = "Focus".some,
                disabled = disableSimpleEdit || !props.isStaffOrAdmin,
                showCustomization = showCustomization,
                allowRevertCustomization = allowRevertCustomization
              ),
              focusMotorStepsViewOpt.map: focusMotorStepsView =>
                FormInputTextView(
                  id = "focus-motor-steps".refined,
                  value = focusMotorStepsView.as(GnirsFocusMotorStepsValue.Value),
                  label = "Focus Motor Steps".some,
                  validFormat = ExploreModelValidators.GnirsFocusMotorStepsValidSplitEpi,
                  disabled = disableSimpleEdit || !props.isStaffOrAdmin
                ),
              CustomizableEnumSelect(
                id = "read-mode".refined,
                view = readModeView,
                defaultValue = None,
                label = "Read Mode".some,
                disabled = disableSimpleEdit,
                showCustomization = showCustomization,
                allowRevertCustomization = allowRevertCustomization
              ),
              CustomizableEnumSelectOptional(
                id = "well-depth".refined,
                view = wellDepthView.withDefault(defaultWellDepth),
                defaultValue = defaultWellDepth.some,
                label = "Well Depth".some,
                disabled = disableSimpleEdit,
                showCustomization = showCustomization,
                allowRevertCustomization = allowRevertCustomization
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
                "gnirsLongSlit".refined
              )
            ),
            <.div(LucumaPrimeStyles.FormColumnCompact, ExploreStyles.SlitTelescopeConfigEditor)(
              SlitTelescopeConfigsEditor(
                explicitValue = slitTelescopeConfigsView,
                defaultValue = props.observingMode.get.defaultTelescopeConfigs,
                defaultForMode = defaultSlitTelescopeConfigs(
                  _,
                  prismView.get,
                  cameraView.get,
                  gratingWavelengthView.get
                    .getOrElse(props.observingMode.get.defaultGratingWavelength)
                ),
                readonly = disableSimpleEdit
              )
            )
          ),
          <.div(ExploreStyles.GnirsLowerGrid)(
            Panel(
              header = <.span("Acquisition"),
              toggleable = true,
              collapsed = true
            )(
              <.div(ExploreStyles.AcquisitionCustomizationGrid)(
                <.div(LucumaPrimeStyles.FormColumnCompact)(
                  FormEnumDropdownView(
                    id = "gnirs-acq-type".refined,
                    value = acquisitionTypeView,
                    label = "Type",
                    disabled = disableAdvancedAcqEdit
                  ),
                  CustomizableEnumSelectOptional(
                    id = "acq-filter".refined,
                    view = acquisitionFilterView.withDefault(defaultAcquisitionFilter),
                    defaultValue = defaultAcquisitionFilter.some,
                    label = "Filter".some,
                    disabled = disableSimpleEdit,
                    showCustomization = showCustomization,
                    allowRevertCustomization = allowRevertCustomization
                  ),
                  <.label(^.htmlFor := "acq-offset")("Sky Offset"),
                  <.span(ExploreStyles.GnirsAcqSkyOffsetEditor)(
                    Checkbox(
                      id = "acq-offset",
                      checked = acquisitionOptOffsetView.get.isDefined,
                      // variant = Checkbox.Variant.Filled,
                      // clazz = ExploreStyles.ObsBadgeAssociatedObsCheckbox,
                      disabled = disableAdvancedAcqEdit,
                      onChange = // Default value is Zero when enabling the offset
                        case true  => acquisitionOptOffsetView.set(Offset.Zero.some)
                        case false => acquisitionOptOffsetView.set(none)
                    ),
                    acquisitionOffsetViewOpt.map: acquisitionOffsetView =>
                      OffsetInput(
                        id = "gnirs-acq-offset".refined,
                        offset = acquisitionOffsetView,
                        readonly = disableAdvancedAcqEdit,
                        clazz = LucumaPrimeStyles.FormField
                      )
                  )
                ),
                <.div(LucumaPrimeStyles.FormColumnCompact)(
                  ExposureTimeModeEditor(
                    props.observingMode.get.instrument.some,
                    none,
                    acquisitionExposureTimeView,
                    ScienceMode.Imaging,
                    props.permissions.isReadonly,
                    props.units,
                    props.calibrationRole,
                    "gnirsAcq".refined,
                    forceCount = Some(1.refined)
                  ),
                  FormInputTextView(
                    id = "gnirs-acq-coadds".refined,
                    value = acquisitionCoaddsView,
                    label = "Coadds",
                    validFormat = InputValidSplitEpi.posInt,
                    changeAuditor = ChangeAuditor.int,
                    disabled = disableAdvancedAcqEdit
                  )(^.autoComplete.off)
                )
              )
            ),
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

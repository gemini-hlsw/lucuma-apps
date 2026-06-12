// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.effect.IO
import cats.syntax.all.*
import clue.data.*
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
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMode
import lucuma.core.model.sequence.gnirs.GnirsFocusMotorStepsValue
import lucuma.core.model.sequence.gnirs.GnirsGratingWavelength
import lucuma.core.model.sequence.gnirs.defaultSlitTelescopeConfigs
import lucuma.core.optics.syntax.lens.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Panel
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.CentralWavelength
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
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

        given acquisitionFilterEnum: Enumerated[Option[GnirsFilter]] =
          deriveOptionalEnumerated[GnirsFilter]("Auto")(using
            Enumerated.fromNEL(GnirsFilter.acquisition).withTag(_.tag)
          )
        given acquisitionFilterDisplay: Display[Option[GnirsFilter]] =
          deriveOptionalDisplay[GnirsFilter]("Auto")

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

        val centralWavelengthView: View[Wavelength] = props.observingMode
          .zoom(
            ObservingMode.GnirsLongSlit.centralWavelength.andThen(CentralWavelength.Value),
            GnirsLongSlitInput.centralWavelength.modify
          )
          .view(_.toInput.assign)

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

        val coaddsView: View[PosInt] = props.observingMode
          .zoom(
            ObservingMode.GnirsLongSlit.coadds,
            GnirsLongSlitInput.coadds.modify
          )
          .view(_.assign)

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

        // In our local model, we use GnirsAcquisitionMode, which maps to 2 fields in the API.
        val acquisitionModeView: View[Option[GnirsAcquisitionMode]] =
          acquisition
            .zoom(
              ObservingMode.GnirsLongSlit.Acquisition.explicitAcquisitionMode,
              GnirsLongSlitAcquisitionInput.explicitAcquisitionType
                .disjointZip(GnirsLongSlitAcquisitionInput.skyOffset)
                .modify
            )
            .view:
              _.map: acqMode =>
                (acqMode.acquisitionType.assign,
                 GnirsAcquisitionMode.skyOffset.getOption(acqMode).map(_.toInput).orUnassign
                )
              .getOrElse((Input.unassign, Input.unassign))

        val acquisitionTypeView: View[Option[GnirsAcquisitionType]] =
          acquisitionModeView.zoom(_.map(_.acquisitionType))(mod =>
            mode =>
              mod(mode.map(_.acquisitionType))
                .map(newType => GnirsAcquisitionMode.forTypeAndOffset(newType, none))
          )

        val acquisitionSkyOffsetViewOpt: Option[View[Offset]] =
          acquisitionModeView.toOptionView
            .flatMap(_.zoom(GnirsAcquisitionMode.skyOffset).toOptionView)

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

        val acquisitionExposureTimeView: View[ExposureTimeMode] =
          acquisition
            .zoom(
              ObservingMode.GnirsLongSlit.Acquisition.exposureTimeMode,
              GnirsLongSlitAcquisitionInput.exposureTimeMode.modify
            )
            .view(_.toInput.assign)

        val defaultDecker: GnirsDecker       = props.observingMode.get.defaultDecker
        val defaultWellDepth: GnirsWellDepth = props.observingMode.get.defaultWellDepth

        React.Fragment(
          <.div(ExploreStyles.GnirsUpperGrid)(
            <.div(LucumaPrimeStyles.FormColumnCompact, ExploreStyles.GnirsConfigEditor)(
              CustomizableEnumSelect(
                id = "filter".refined,
                view = filterView,
                defaultValue = props.observingMode.get.initialFilter,
                label = "Filter".some,
                helpId = Some("configuration/gnirs/filter.md".refined),
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
                helpId = Some("configuration/gnirs/decker.md".refined),
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
                helpId = Some("configuration/gnirs/fpu.md".refined),
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
                helpId = Some("configuration/gnirs/prism.md".refined),
                disabled = disableAdvancedEdit,
                showCustomization = showCustomization,
                allowRevertCustomization = allowRevertCustomization
              ),
              CustomizableEnumSelect(
                id = "grating".refined,
                view = gratingView,
                defaultValue = props.observingMode.get.initialGrating,
                label = "Grating".some,
                helpId = Some("configuration/gnirs/grating.md".refined),
                disabled = disableAdvancedEdit,
                showCustomization = showCustomization,
                allowRevertCustomization = allowRevertCustomization,
                useLongName = true
              ),
              CustomizableInputText(
                id = "central-wavelength".refined,
                value = centralWavelengthView,
                defaultValue = props.observingMode.get.initialCentralWavelength.value,
                label = React.Fragment(
                  "Wavelength",
                  HelpIcon("configuration/gnirs/wavelength.md".refined)
                ),
                units = props.units.symbol.some,
                validFormat = props.units.toInputFormat,
                changeAuditor = props.units.toAuditor,
                disabled = disableSimpleEdit,
                showCustomization = showCustomization,
                allowRevertCustomization = allowRevertCustomization
              ),
              CustomizableEnumSelect(
                id = "camera".refined,
                view = cameraView,
                defaultValue = props.observingMode.get.initialCamera,
                label = "Camera".some,
                helpId = Some("configuration/gnirs/camera.md".refined),
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
                helpId = Some("configuration/gnirs/focus.md".refined),
                disabled = disableSimpleEdit || !props.isStaffOrAdmin,
                showCustomization = showCustomization,
                allowRevertCustomization = allowRevertCustomization
              ),
              focusMotorStepsViewOpt.map: focusMotorStepsView =>
                FormInputTextView(
                  id = "focus-motor-steps".refined,
                  value = focusMotorStepsView.as(GnirsFocusMotorStepsValue.Value),
                  label =
                    React.Fragment("Focus Motor Steps",
                                   HelpIcon("configuration/gnirs/focus-motor-steps.md".refined)
                    ),
                  validFormat = ExploreModelValidators.GnirsFocusMotorStepsValidSplitEpi,
                  disabled = disableSimpleEdit || !props.isStaffOrAdmin
                ),
              CustomizableEnumSelect(
                id = "read-mode".refined,
                view = readModeView,
                defaultValue = None,
                label = "Read Mode".some,
                helpId = Some("configuration/gnirs/read-mode.md".refined),
                disabled = disableSimpleEdit,
                showCustomization = showCustomization,
                allowRevertCustomization = allowRevertCustomization
              ),
              CustomizableEnumSelectOptional(
                id = "well-depth".refined,
                view = wellDepthView.withDefault(defaultWellDepth),
                defaultValue = defaultWellDepth.some,
                label = "Well Depth".some,
                helpId = Some("configuration/gnirs/well-depth.md".refined),
                disabled = disableSimpleEdit,
                showCustomization = showCustomization,
                allowRevertCustomization = allowRevertCustomization
              )
            ),
            <.div(LucumaPrimeStyles.FormColumnCompact)(
              ExposureTimeModeEditor(
                instrument = props.observingMode.get.instrument.some,
                wavelength = none,
                exposureTimeMode = exposureTimeMode,
                coadds = coaddsView.some,
                scienceMode = ScienceMode.Spectroscopy,
                readonly = !props.permissions.isFullEdit,
                units = props.units,
                calibrationRole = props.calibrationRole,
                idPrefix = "gnirsLongSlit".refined
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
                  GnirsGratingWavelength(centralWavelengthView.get)
                ),
                readonly = disableSimpleEdit
              )
            )
          ),
          <.div(ExploreStyles.GnirsLowerGrid)(
            Panel(
              header = <.span(
                "Acquisition",
                HelpIcon("configuration/gnirs/acquisition-customization.md".refined)
              ),
              toggleable = true,
              collapsed = true
            )(
              <.div(ExploreStyles.AcquisitionCustomizationGrid)(
                <.div(LucumaPrimeStyles.FormColumnCompact)(
                  CustomizableEnumSelect(
                    id = "acq-type".refined,
                    view = acquisitionTypeView,
                    defaultValue = none,
                    label = "Type".some,
                    helpId = Some("configuration/gnirs/acquisition-type.md".refined),
                    disabled = disableAdvancedAcqEdit,
                    showCustomization = showCustomization,
                    allowRevertCustomization = allowRevertCustomization
                  ),
                  acquisitionSkyOffsetViewOpt.map: acquisitionOffsetView =>
                    OffsetInput(
                      id = "acq-offset".refined,
                      offset = acquisitionOffsetView,
                      readonly = disableAdvancedAcqEdit,
                      clazz = LucumaPrimeStyles.FormField
                    ),
                  CustomizableEnumSelect(
                    id = "acq-filter".refined,
                    view = acquisitionFilterView,
                    defaultValue = none,
                    label = "Filter".some,
                    helpId = Some("configuration/gnirs/acquisition-filter.md".refined),
                    disabled = disableSimpleEdit,
                    showCustomization = showCustomization,
                    allowRevertCustomization = allowRevertCustomization
                  )
                ),
                <.div(LucumaPrimeStyles.FormColumnCompact)(
                  ExposureTimeModeEditor(
                    instrument = props.observingMode.get.instrument.some,
                    wavelength = none,
                    exposureTimeMode = acquisitionExposureTimeView,
                    coadds = acquisitionCoaddsView.some,
                    scienceMode = ScienceMode.Imaging,
                    readonly = props.permissions.isReadonly,
                    units = props.units,
                    calibrationRole = props.calibrationRole,
                    idPrefix = "gnirsAcq".refined,
                    forceCount = Some(1.refined)
                  )
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

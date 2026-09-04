// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.data.NonEmptyList
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
import explore.config.offsets.OffsetInput
import explore.model.AppContext
import explore.model.ExploreModelValidators
import explore.model.Observation
import explore.model.enums.WavelengthUnits
import explore.model.syntax.all.*
import explore.modes.SpectroscopyModesMatrix
import explore.syntax.ui.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.util.Effect
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.enums.GnirsDecker
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Program
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMode
import lucuma.core.model.sequence.gnirs.GnirsFocusMotorStepsValue
import lucuma.core.optics.syntax.lens.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.react.primereact.Panel
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given
import monocle.Lens

/**
 * A GNIRS spectroscopy panel's props.
 *
 * Long slit and IFU are edited by one form but are separate model and input types, so each mode
 * resolves its own zooms and reads here. The form below only ever sees plain views, and the only
 * thing it is generic in is the FPU.
 */
trait GnirsSpectroscopyPanelProps[Fpu]:
  def programId: Program.Id
  def obsId: Observation.Id
  def calibrationRole: Option[CalibrationRole]
  def revertConfig: IO[Unit]
  def confMatrix: SpectroscopyModesMatrix
  def sequenceChanged: Callback
  def permissions: ConfigEditPermissions
  def isStaffOrAdmin: Boolean
  def units: WavelengthUnits

  def mode: ObservingMode
  def isCustomized: Boolean
  def initialFilter: GnirsFilter
  def initialPrism: GnirsPrism
  def initialGrating: GnirsGrating
  def initialCamera: GnirsCamera
  def initialFpu: Fpu
  def initialCentralWavelengths: NonEmptyList[ObservingMode.GnirsCentralWavelengthConfig]
  def defaultDecker: GnirsDecker
  def defaultWellDepth: GnirsWellDepth

  /** Selecting FAINT seeds the sky offset, and the slit and the IFU place the sky differently. */
  def defaultFaintSkyOffset: Offset

  /** A zoom, so unlike the views below this needs no effect context. */
  def acquisitionAligner
    : Aligner[ObservingMode.GnirsSpectroscopyAcquisition, GnirsSpectroscopyAcquisitionInput]

  def revertCustomizations: Callback
  def filterView: View[GnirsFilter]
  def deckerView: View[Option[GnirsDecker]]
  def fpuView: View[Fpu]
  def prismView: View[GnirsPrism]
  def gratingView: View[GnirsGrating]
  def cameraView: View[GnirsCamera]
  def readModeView: View[Option[GnirsReadMode]]
  def wellDepthView: View[Option[GnirsWellDepth]]
  def focusMotorStepsView: View[Option[GnirsFocusMotorStepsValue]]

  def centralWavelengthsView: View[
    NonEmptyList[ObservingMode.GnirsCentralWavelengthConfig]
  ]

  /** The offsets editor: along-slit presets for the slit, plain p/q offsets for the IFU. */
  def telescopeConfigsEditor(
    prism:      GnirsPrism,
    camera:     GnirsCamera,
    wavelength: Wavelength
  ): VdomNode

/** The form itself, which differs between the two modes only in the FPU type. */
abstract class GnirsSpectroscopyPanelBuilder[
  Fpu: Enumerated: Display,
  Props <: GnirsSpectroscopyPanelProps[Fpu]
]:
  val component = ScalaFnComponent[Props]: props =>
    for
      ctx       <- useContext(AppContext.ctx)
      modeData  <- useModeData(props.confMatrix, props.mode)
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
      val showAcquisitionConfig: Boolean    = props.calibrationRole.needsAcquisitionConfig

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
          Enumerated.fromNEL(GnirsFilter.AcquisitionFilters).withTag(_.tag)
        )
      given acquisitionFilterDisplay: Display[Option[GnirsFilter]] =
        deriveOptionalDisplay[GnirsFilter]("Auto")

      val filterView: View[GnirsFilter] = props.filterView

      val deckerView: View[Option[GnirsDecker]] = props.deckerView

      val fpuView: View[Fpu] = props.fpuView

      val prismView: View[GnirsPrism] = props.prismView

      val gratingView: View[GnirsGrating] = props.gratingView

      val centralWavelengthsView: View[NonEmptyList[ObservingMode.GnirsCentralWavelengthConfig]] =
        props.centralWavelengthsView

      // Where a single representative wavelength is needed (the along-slit offset
      // defaults are computed from the grating setting), use the first, which is
      // the shortest and the one the sequence starts at.
      val primaryWavelength: Wavelength =
        centralWavelengthsView.get.head.centralWavelength.value

      val cameraView: View[GnirsCamera] = props.cameraView

      val readModeView: View[Option[GnirsReadMode]] = props.readModeView

      val wellDepthView: View[Option[GnirsWellDepth]] = props.wellDepthView

      val focusMotorStepsView: View[Option[GnirsFocusMotorStepsValue]] =
        props.focusMotorStepsView

      val focusModeView: View[GnirsFocusMode] =
        focusMotorStepsView.zoom(GnirsFocusMode.fromMotorSteps(_))(mod =>
          steps => mod(GnirsFocusMode.fromMotorSteps(steps)).toMotorSteps
        )

      val focusMotorStepsViewOpt: Option[View[GnirsFocusMotorStepsValue]] =
        focusMotorStepsView.toOptionView

      val acquisition
        : Aligner[ObservingMode.GnirsSpectroscopyAcquisition, GnirsSpectroscopyAcquisitionInput] =
        props.acquisitionAligner

      // In our local model, we use GnirsAcquisitionMode, which maps to 2 fields in the API.
      val acquisitionModeView: View[Option[GnirsAcquisitionMode]] =
        acquisition
          .zoom(
            ObservingMode.GnirsSpectroscopyAcquisition.explicitAcquisitionMode,
            GnirsSpectroscopyAcquisitionInput.explicitAcquisitionType
              .disjointZip(GnirsSpectroscopyAcquisitionInput.skyOffset)
              .modify
          )
          .view:
            _.map: acqMode =>
              (acqMode.acquisitionType.assign,
               GnirsAcquisitionMode.skyOffset.getOption(acqMode).map(_.toInput).orUnassign
              )
            .getOrElse((Input.unassign, Input.unassign))

      val defaultFaintSkyOffset: Offset = props.defaultFaintSkyOffset

      val acquisitionTypeView: View[Option[GnirsAcquisitionType]] =
        acquisitionModeView.zoom(_.map(_.acquisitionType))(mod =>
          mode =>
            mod(mode.map(_.acquisitionType))
              .map(newType => GnirsAcquisitionMode.forTypeAndOffset(newType, defaultFaintSkyOffset))
        )

      val acquisitionSkyOffsetViewOpt: Option[View[Offset]] =
        acquisitionModeView.toOptionView
          .flatMap(_.zoom(GnirsAcquisitionMode.skyOffset).toOptionView)

      val acquisitionCoaddsView: View[PosInt] =
        acquisition
          .zoom(
            ObservingMode.GnirsSpectroscopyAcquisition.coadds,
            GnirsSpectroscopyAcquisitionInput.coadds.modify
          )
          .view(_.assign)

      val acquisitionFilterView: View[Option[GnirsFilter]] =
        acquisition
          .zoom(
            ObservingMode.GnirsSpectroscopyAcquisition.explicitFilter,
            GnirsSpectroscopyAcquisitionInput.explicitFilter.modify
          )
          .view(_.orUnassign)

      // The editor shows the effective mode, but entering a value makes it explicit.
      val acquisitionExposureTimeView: View[ExposureTimeMode] =
        acquisition
          .zoom(
            Lens[ObservingMode.GnirsSpectroscopyAcquisition, ExposureTimeMode](
              _.exposureTimeMode
            )(etm => _.copy(exposureTimeMode = etm, explicitExposureTimeMode = etm.some)),
            GnirsSpectroscopyAcquisitionInput.explicitExposureTimeMode.modify
          )
          .view(_.toInput.assign)

      // Reverting only clears the override. The effective mode keeps showing the old value
      // until the server answers with the derived one, which it always pairs with coadds of
      // 1 -- but coadds are not rendered for a signal-to-noise mode, so that is invisible.
      val revertAcquisitionExposureTime: Callback =
        acquisition
          .zoom(
            ObservingMode.GnirsSpectroscopyAcquisition.explicitExposureTimeMode,
            GnirsSpectroscopyAcquisitionInput.explicitExposureTimeMode.modify
          )
          .view(_.map(_.toInput).orUnassign)
          .set(none)

      // Reverts every acquisition customization at once. The per-field addons all live inside
      // the Acquisition panel, which is collapsed by default, so the section needs its own.
      val revertAcquisition: Callback =
        acquisition.view(_.toInput).mod(_.revertCustomizations)

      val defaultDecker: GnirsDecker                                                          =
        props.defaultDecker
      val defaultWellDepth: GnirsWellDepth                                                    =
        props.defaultWellDepth
      val gnirsInstrument: Option[Instrument]                                                 =
        props.mode.instrument
      val isCustomized: Boolean                                                               =
        props.isCustomized
      val initialFilter: GnirsFilter                                                          =
        props.initialFilter
      val initialPrism: GnirsPrism                                                            =
        props.initialPrism
      val initialGrating: GnirsGrating                                                        =
        props.initialGrating
      val initialCamera: GnirsCamera                                                          =
        props.initialCamera
      val initialCentralWavelengths: NonEmptyList[ObservingMode.GnirsCentralWavelengthConfig] =
        props.initialCentralWavelengths
      val revertAllCustomizations: Callback                                                   =
        props.revertCustomizations

      React.Fragment(
        <.div(ExploreStyles.GnirsUpperGrid)(
          <.div(LucumaPrimeStyles.FormColumnCompact, ExploreStyles.GnirsConfigEditor)(
            CustomizableEnumSelect(
              id = "filter".refined,
              view = filterView,
              defaultValue = initialFilter,
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
              defaultValue = props.initialFpu,
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
              defaultValue = initialPrism,
              label = "Prism".some,
              helpId = Some("configuration/gnirs/prism.md".refined),
              disabled = disableAdvancedEdit,
              showCustomization = showCustomization,
              allowRevertCustomization = allowRevertCustomization
            ),
            CustomizableEnumSelect(
              id = "grating".refined,
              view = gratingView,
              defaultValue = initialGrating,
              label = "Grating".some,
              helpId = Some("configuration/gnirs/grating.md".refined),
              disabled = disableAdvancedEdit,
              showCustomization = showCustomization,
              allowRevertCustomization = allowRevertCustomization,
              useLongName = true
            ),
            CustomizableEnumSelect(
              id = "camera".refined,
              view = cameraView,
              defaultValue = initialCamera,
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
                label = React.Fragment("Focus Motor Steps",
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
          <.div(LucumaPrimeStyles.FormColumnCompact, ExploreStyles.SlitTelescopeConfigEditor)(
            props.telescopeConfigsEditor(prismView.get, cameraView.get, primaryWavelength)
          )
        ),
        GnirsWavelengthsPanel(
          instrument = gnirsInstrument,
          wavelengthsView = centralWavelengthsView,
          initialWavelengths = initialCentralWavelengths,
          // A new row with no explicit exposure time mode falls back to the
          // observation's requirements, as the imaging filters do.
          requirementsExposureTimeMode = none,
          units = props.units,
          calibrationRole = props.calibrationRole,
          allowRevertCustomization = allowRevertCustomization,
          wavelengthReadonly = disableSimpleEdit,
          exposureTimeModeReadonly = !props.permissions.isFullEdit,
          showCustomization = showCustomization
        ),
        <.div(ExploreStyles.GnirsLowerGrid)(
          Panel(
            header = <.span(
              "Acquisition",
              HelpIcon("configuration/gnirs/acquisition-customization.md".refined),
              CustomizedGroupAddon(
                "automatic",
                revertAcquisition,
                allowRevertCustomization
              ).when(showCustomization && acquisition.get.isCustomized)
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
                  instrument = gnirsInstrument,
                  wavelength = none,
                  exposureTimeMode = acquisitionExposureTimeView,
                  coadds = acquisitionCoaddsView.some,
                  scienceMode = ScienceMode.Imaging,
                  readonly = props.permissions.isReadonly,
                  units = props.units,
                  calibrationRole = props.calibrationRole,
                  idPrefix = "gnirsAcq".refined,
                  forceCount = Some(1.refined),
                  isCustomized =
                    showCustomization && acquisition.get.explicitExposureTimeMode.isDefined,
                  revertCustomization = revertAcquisitionExposureTime,
                  allowRevertCustomization = allowRevertCustomization
                )
              )
            )
          ).when(showAcquisitionConfig),
          AdvancedConfigButtons(
            editState = editState,
            isCustomized = isCustomized,
            revertConfig = props.revertConfig,
            revertCustomizations = revertAllCustomizations,
            sequenceChanged = props.sequenceChanged,
            !props.permissions.isFullEdit,
            showAdvancedButton = true
          )
        )
      )

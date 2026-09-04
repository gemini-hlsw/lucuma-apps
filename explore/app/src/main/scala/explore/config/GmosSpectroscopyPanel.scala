// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.View
import crystal.react.hooks.*
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.config.ConfigurationFormats.*
import explore.model.ExploreModelValidators
import explore.model.Help
import explore.model.Observation
import explore.model.display.given
import explore.model.enums.WavelengthUnits
import explore.model.syntax.all.*
import explore.modes.SpectroscopyModesMatrix
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDither
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Program
import lucuma.core.syntax.all.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.react.primereact.Panel
import lucuma.refined.*
import lucuma.schemas.model.ObservingMode
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.given

/**
 * Common props for the GMOS spectroscopy panels.
 *
 * Long slit and MOS, north and south, are edited by one form but are separate model and input
 * types, so each mode resolves its own zooms and reads here. The form below only ever sees plain
 * views, and the only things it is generic in are the grating, filter and FPU enums.
 */
trait GmosSpectroscopyPanelProps[Grating, Filter, Fpu]:
  def programId: Program.Id
  def obsId: Observation.Id
  def calibrationRole: Option[CalibrationRole]
  def revertConfig: IO[Unit]
  def confMatrix: SpectroscopyModesMatrix
  def sequenceChanged: Callback
  def permissions: ConfigEditPermissions
  def units: WavelengthUnits

  def mode: ObservingMode
  def instrument: Option[Instrument] = mode.instrument
  def isCustomized: Boolean

  /** Customization indicators are hidden for calibration observations. */
  def showCustomization: Boolean        = calibrationRole.isEmpty
  def allowRevertCustomization: Boolean = permissions.isFullEdit

  // Values read straight off the model, used for the "default"/"original" markers.
  def initialGrating: Grating
  def initialFilter: Option[Filter]
  def initialFpu: Fpu
  def initialCentralWavelength: Wavelength
  def defaultXBinning: GmosXBinning
  def defaultYBinning: GmosYBinning
  def defaultReadModeGain: (GmosAmpReadMode, GmosAmpGain)
  def defaultRoi: GmosRoi
  def defaultWavelengthDithers: NonEmptyList[WavelengthDither]
  def resolvedReadModeGain: (GmosAmpReadMode, GmosAmpGain)

  def excludedFpus: Set[Fpu]
  def fpuLabel: String
  def fpuHelpId: Option[Help.Id]

  /**
   * Whether the FPU value is shown as read-only information instead of an editable select. True for
   * MOS once the mask is bound, since the plate then defines the slit width.
   */
  def fpuControlReadonly: Boolean =
    false

  def revertCustomizations: Callback
  def centralWavelengthView: View[Wavelength]
  def gratingView: View[Grating]
  def filterView: View[Option[Filter]]
  def fpuView: View[Fpu]
  def explicitXBinningView: View[Option[GmosXBinning]]
  def explicitYBinningView: View[Option[GmosYBinning]]
  def explicitReadModeGainView: View[Option[(GmosAmpReadMode, GmosAmpGain)]]
  def explicitRoiView: View[Option[GmosRoi]]
  def explicitWavelengthDithersView: View[Option[NonEmptyList[WavelengthDither]]]
  def exposureTimeModeView: View[ExposureTimeMode]

  /**
   * The spatial positions editor. Long slit stores a `SlitTelescopeConfigs` and MOS a plain list of
   * offsets, so each mode supplies its own editor rather than the form knowing which shape it has.
   */
  def offsetsControl(disabled: Boolean): VdomNode

  def modeSpecificFields(@annotation.unused disabled: Boolean): VdomNode =
    EmptyVdom

  def acquisitionSection(disabled: Boolean): VdomNode

  /** The MOS mask picker, or nothing for long slit. */
  def maskControl: VdomNode

/** The form itself, generic in the grating, filter and FPU enums it renders. */
abstract class GmosSpectroscopyPanelBuilder[
  Grating: Enumerated: Display,
  Filter: Enumerated: Display,
  Fpu: Enumerated: Display,
  Props <: GmosSpectroscopyPanelProps[Grating, Filter, Fpu]
]:
  protected given Display[(GmosAmpReadMode, GmosAmpGain)] =
    Display.by( // Shortname is in lower case for some reason
      { case (r, g) => s"${r.longName}, ${g.shortName} Gain" },
      { case (r, g) => s"${r.longName}, ${g.longName} Gain" }
    )

  val component =
    ScalaFnComponent[Props]: props =>
      for
        modeData  <- useModeData(props.confMatrix, props.mode)
        editState <- useStateView(ConfigEditState.View)
      yield
        val disableAdvancedEdit      =
          editState.get =!= ConfigEditState.AdvancedEdit || !props.permissions.isFullEdit
        val disableSimpleEdit        =
          disableAdvancedEdit && editState.get =!= ConfigEditState.SimpleEdit
        val disableAdvancedAcqEdit   = disableAdvancedEdit && !props.permissions.isOnlyForOngoing
        val showCustomization        = props.showCustomization
        val allowRevertCustomization = props.allowRevertCustomization
        val showAcquisitionConfig    = props.calibrationRole.needsAcquisitionConfig

        val centralWavelengthView = props.centralWavelengthView

        val validDithers = modeData.value
          .map: mode =>
            ExploreModelValidators.dithersValidWedge(
              centralWavelengthView.get,
              mode.λmin.value,
              mode.λmax.value
            )
          .getOrElse(ExploreModelValidators.ditherValidWedge)
          .toNel(",".refined)
          .withErrorMessage(_ => "Invalid wavelength dither values".refined)
          .optional

        def dithersControl(onChange: Callback): VdomElement =
          CustomizableInputTextOptional(
            id = "dithers".refined,
            value = props.explicitWavelengthDithersView.withOnMod(_ => onChange),
            defaultValue = props.defaultWavelengthDithers,
            label =
              React.Fragment("λ Dithers", HelpIcon("configuration/gmos/lambda-dithers.md".refined)),
            validFormat = validDithers,
            changeAuditor = ChangeAuditor
              .bigDecimal(integers = 3.refined, decimals = 1.refined)
              .toSequence()
              .optional,
            units = "nm".some,
            disabled = disableSimpleEdit,
            showCustomization = showCustomization,
            allowRevertCustomization = allowRevertCustomization
          )

        React.Fragment(
          <.div(ExploreStyles.GmosSpectroscopyUpperGrid)(
            <.div(LucumaPrimeStyles.FormColumnCompact)(
              CustomizableEnumSelect(
                id = "grating".refined,
                view = props.gratingView,
                defaultValue = props.initialGrating,
                label = "Grating".some,
                helpId = Some("configuration/gmos/grating.md".refined),
                disabled = disableAdvancedEdit,
                showCustomization = showCustomization,
                allowRevertCustomization = allowRevertCustomization
              ),
              CustomizableEnumSelectOptional(
                id = "filter".refined,
                view = props.filterView,
                defaultValue = props.initialFilter,
                label = "Filter".some,
                helpId = Some("configuration/gmos/filter.md".refined),
                disabled = disableAdvancedEdit,
                showClear = true,
                resetToOriginal = true,
                showCustomization = showCustomization,
                allowRevertCustomization = allowRevertCustomization
              ),
              props.maskControl,
              if (props.fpuControlReadonly)
                React.Fragment(
                  FormLabel(htmlFor = "fpu".refined)(
                    props.fpuLabel,
                    props.fpuHelpId.map(HelpIcon(_)).whenDefined
                  ),
                  <.label(^.id := "fpu",
                          ExploreStyles.FormValue |+| ExploreStyles.FormValueRight,
                          Display[Fpu].shortName(props.fpuView.get)
                  )
                )
              else
                CustomizableEnumSelect(
                  id = "fpu".refined,
                  view = props.fpuView,
                  defaultValue = props.initialFpu,
                  label = props.fpuLabel.some,
                  helpId = props.fpuHelpId,
                  disabled = disableAdvancedEdit,
                  exclude = props.excludedFpus,
                  showCustomization = showCustomization,
                  allowRevertCustomization = allowRevertCustomization
                ),
              <.div(
                LucumaPrimeStyles.FormColumnCompact,
                ExploreStyles.SlitTelescopeConfigEditor,
                ExploreStyles.SlitTelescopeConfigEditorInline
              )(
                props.offsetsControl(props.permissions.isReadonly)
              )
            ),
            <.div(LucumaPrimeStyles.FormColumnCompact)(
              CustomizableInputText(
                id = "central-wavelength".refined,
                value = centralWavelengthView,
                label = React.Fragment(
                  "Central Wavelength",
                  HelpIcon("configuration/gmos/central=wavelength.md".refined)
                ),
                units = props.units.symbol.some,
                validFormat = props.units.toInputFormat,
                changeAuditor = props.units.toAuditor,
                defaultValue = props.initialCentralWavelength,
                disabled = disableSimpleEdit,
                showCustomization = showCustomization,
                allowRevertCustomization = allowRevertCustomization
              ),
              dithersControl(props.sequenceChanged),
              ExposureTimeModeEditor(
                instrument = props.instrument,
                wavelength = none,
                exposureTimeMode = props.exposureTimeModeView,
                coadds = none,
                scienceMode = ScienceMode.Spectroscopy,
                readonly = !props.permissions.isFullEdit,
                units = props.units,
                calibrationRole = props.calibrationRole,
                idPrefix = "gmosSpectroscopy".refined
              ),
              props.modeSpecificFields(props.permissions.isReadonly)
            ),
            <.div(LucumaPrimeStyles.FormColumnCompact)(
              // Provide better accessibility by using aria-label directly
              // on the dropdowns so X and Y binning are correctly labeled.
              <.label(
                ^.htmlFor := "explicitXBin",
                LucumaPrimeStyles.FormFieldLabel,
                "Binning",
                HelpIcon("configuration/gmos/binning.md".refined)
              ),
              <.div(ExploreStyles.GmosSpectroscopyBinning)(
                CustomizableEnumSelectOptional(
                  id = "explicitXBin".refined,
                  view = props.explicitXBinningView.withDefault(props.defaultXBinning),
                  defaultValue = props.defaultXBinning.some,
                  disabled = disableAdvancedEdit,
                  dropdownMods = ^.aria.label := "X Binning",
                  showCustomization = showCustomization,
                  allowRevertCustomization = allowRevertCustomization
                ),
                <.label(^.htmlFor := "explicitYBin", "x"),
                CustomizableEnumSelectOptional(
                  id = "explicitYBin".refined,
                  view = props.explicitYBinningView.withDefault(props.defaultYBinning),
                  defaultValue = props.defaultYBinning.some,
                  disabled = disableAdvancedEdit,
                  dropdownMods = ^.aria.label := "Y Binning",
                  showCustomization = showCustomization,
                  allowRevertCustomization = allowRevertCustomization
                )
              ),
              CustomizableEnumSelectOptional(
                id = "explicitReadMode".refined,
                view = props.explicitReadModeGainView
                  .withDefault(props.defaultReadModeGain, props.resolvedReadModeGain),
                defaultValue = props.defaultReadModeGain.some,
                label = "Read Mode".some,
                helpId = Some("configuration/gmos/read-mode.md".refined),
                disabled = disableAdvancedEdit,
                showCustomization = showCustomization,
                allowRevertCustomization = allowRevertCustomization
              ),
              CustomizableEnumSelectOptional(
                id = "explicitRoi".refined,
                view = props.explicitRoiView.withDefault(props.defaultRoi),
                defaultValue = props.defaultRoi.some,
                label = "ROI".some,
                helpId = Some("configuration/gmos/roi.md".refined),
                disabled = disableAdvancedEdit,
                showCustomization = showCustomization,
                allowRevertCustomization = allowRevertCustomization
              ),
              LambdaAndIntervalFormValues(
                modeData = modeData,
                centralWavelength = centralWavelengthView.get,
                units = props.units
              )
            )
          ),
          <.div(ExploreStyles.GmosSpectroscopyLowerGrid)(
            Panel(
              header = <.span(
                "Acquisition",
                HelpIcon("configuration/gmos/acquisition-customization.md".refined)
              ),
              toggleable = true,
              collapsed = true
            )(
              props.acquisitionSection(disableAdvancedAcqEdit)
            ).when(showAcquisitionConfig),
            AdvancedConfigButtons(
              editState = editState,
              isCustomized = props.isCustomized,
              revertConfig = props.revertConfig,
              revertCustomizations = props.revertCustomizations,
              sequenceChanged = props.sequenceChanged,
              readonly = !props.permissions.isFullEdit
            )
          )
        )

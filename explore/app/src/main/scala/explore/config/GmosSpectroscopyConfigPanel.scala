// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.MonadError
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import clue.data.syntax.*
import coulomb.Quantity
import crystal.react.View
import crystal.react.hooks.*
import explore.config.offsets.SlitTelescopeConfigsEditor
import explore.config.offsets.IfuTelescopeConfigsEditor
import explore.common.Aligner
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.config.ConfigurationFormats.*
import explore.model.AppContext
import explore.model.Attachment
import explore.model.ExploreModelValidators
import explore.model.Help
import explore.model.Observation
import explore.model.display.given
import explore.model.enums.ExposureTimeModeType
import explore.model.enums.WavelengthUnits
import explore.model.syntax.all.*
import explore.modes.ModeWavelength
import explore.modes.SpectroscopyModesMatrix
import explore.syntax.ui.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.util.Effect
import japgolly.scalajs.react.util.Effect.Dispatch
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.GnirsSlitOffsetPreset
import lucuma.core.enums.*
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDither
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Program
import lucuma.core.optics.syntax.lens.*
import lucuma.core.syntax.all.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Panel
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.CentralWavelength
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.optics.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.given
import monocle.Lens
import org.typelevel.log4cats.Logger

object GmosSpectroscopyConfigPanel {
  sealed trait GmosSpectroscopyConfigPanel[T <: ObservingMode, Input] {
    def programId: Program.Id
    def obsId: Observation.Id
    def calibrationRole: Option[CalibrationRole]
    def observingMode: Aligner[T, Input]
    def revertConfig: IO[Unit]
    def confMatrix: SpectroscopyModesMatrix
    def sequenceChanged: Callback
    def permissions: ConfigEditPermissions
    def units: WavelengthUnits
    def instrument = observingMode.get.instrument

    /** Customization indicators are hidden for calibration observations. */
    def showCustomization: Boolean        = calibrationRole.isEmpty
    def allowRevertCustomization: Boolean = permissions.isFullEdit
  }

  /**
   * Props carried only by the GMOS MOS panels to select MOS mask attachments
   */
  sealed trait GmosMosConfigPanel[T <: ObservingMode, Input]
      extends GmosSpectroscopyConfigPanel[T, Input]:
    def maskContext: MosMaskContext

  sealed abstract class GmosSpectroscopyConfigPanelBuilder[
    T <: ObservingMode,
    Input,
    Props <: GmosSpectroscopyConfigPanel[T, Input],
    Grating: Enumerated: Display,
    Filter: Enumerated: Display,
    Fpu: Enumerated: Display
  ] {
    protected type AA = Aligner[T, Input]

    inline protected def isCustomized(aligner: AA): Boolean = aligner.get.isCustomized

    protected def revertCustomizations(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): Callback

    protected def centralWavelength(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Wavelength]

    protected def grating(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Grating]

    protected def filter(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[Filter]]

    protected def fpu(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Fpu]

    protected def explicitXBinning(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosXBinning]]

    protected def explicitYBinning(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosYBinning]]

    protected def explicitReadModeGain(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[(GmosAmpReadMode, GmosAmpGain)]]

    protected def explicitRoi(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosRoi]]

    protected def explicitWavelengthDithers(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[
      Option[NonEmptyList[WavelengthDither]]
    ]

    protected def exposureTimeMode(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[ExposureTimeMode]

    /**
     * The spatial positions editor. Long slit stores a `SlitTelescopeConfigs` and MOS a plain list
     * of offsets, so each mode supplies its own editor rather than the form knowing which shape it
     * has.
     */
    protected def offsetsControl(props: Props, disabled: Boolean)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): VdomNode

    protected def acquisitionSection(props: Props, disabled: Boolean)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): VdomNode

    protected def maskControl(props: Props)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): VdomNode

    /**
     * Whether the FPU value is shown as read-only information instead of an editable select. True
     * for MOS once the mask is bound, since the plate then defines the slit width.
     */
    protected def fpuControlReadonly(props: Props)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): Boolean = false

    protected val initialGratingLens: Lens[T, Grating]
    protected val initialFilterLens: Lens[T, Option[Filter]]
    protected val initialFpuLens: Lens[T, Fpu]
    protected val initialCentralWavelengthLens: Lens[T, Wavelength]
    protected val defaultXBinningLens: Lens[T, GmosXBinning]
    protected val defaultYBinningLens: Lens[T, GmosYBinning]
    protected val defaultReadModeGainLens: Lens[T, (GmosAmpReadMode, GmosAmpGain)]
    protected val defaultRoiLens: Lens[T, GmosRoi]
    protected val defaultWavelengthDithersLens: Lens[T, NonEmptyList[WavelengthDither]]

    protected val excludedFpus: Set[Fpu]
    protected val fpuLabel: String
    protected val fpuHelpId: Option[Help.Id]

    protected def resolvedReadModeGainGetter: T => (GmosAmpReadMode, GmosAmpGain)

    protected given Display[(GmosAmpReadMode, GmosAmpGain)] =
      Display.by( // Shortname is in lower case for some reason
        { case (r, g) => s"${r.longName}, ${g.shortName} Gain" },
        { case (r, g) => s"${r.longName}, ${g.longName} Gain" }
      )

    val component =
      ScalaFnComponent[Props]: props =>
        for
          ctx       <- useContext(AppContext.ctx)
          modeData  <- useModeData(props.confMatrix, props.observingMode.get)
          editState <- useStateView(ConfigEditState.View)
        yield
          import ctx.given

          val disableAdvancedEdit      =
            editState.get =!= ConfigEditState.AdvancedEdit || !props.permissions.isFullEdit
          val disableSimpleEdit        =
            disableAdvancedEdit && editState.get =!= ConfigEditState.SimpleEdit
          val disableAdvancedAcqEdit   = disableAdvancedEdit && !props.permissions.isOnlyForOngoing
          val showCustomization        = props.showCustomization
          val allowRevertCustomization = props.allowRevertCustomization
          val showAcquisitionConfig    = props.calibrationRole.needsAcquisitionConfig

          val centralWavelengthView    = centralWavelength(props.observingMode)
          val initialCentralWavelength = initialCentralWavelengthLens.get(props.observingMode.get)

          val defaultXBinning      = defaultXBinningLens.get(props.observingMode.get)
          val defaultYBinning      = defaultYBinningLens.get(props.observingMode.get)
          val defaultReadModeGain  = defaultReadModeGainLens.get(props.observingMode.get)
          val defaultRoi           = defaultRoiLens.get(props.observingMode.get)
          val resolvedReadModeGain = resolvedReadModeGainGetter(props.observingMode.get)

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
            val default = defaultWavelengthDithersLens.get(props.observingMode.get)
            val view    = explicitWavelengthDithers(props.observingMode)
            CustomizableInputTextOptional(
              id = "dithers".refined,
              value = view.withOnMod(_ => onChange),
              defaultValue = default,
              label = React.Fragment("λ Dithers",
                                     HelpIcon("configuration/gmos/lambda-dithers.md".refined)
              ),
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
            <.div(ExploreStyles.GmosLongSlitUpperGrid)(
              <.div(LucumaPrimeStyles.FormColumnCompact)(
                CustomizableEnumSelect(
                  id = "grating".refined,
                  view = grating(props.observingMode),
                  defaultValue = initialGratingLens.get(props.observingMode.get),
                  label = "Grating".some,
                  helpId = Some("configuration/gmos/grating.md".refined),
                  disabled = disableAdvancedEdit,
                  showCustomization = showCustomization,
                  allowRevertCustomization = allowRevertCustomization
                ),
                CustomizableEnumSelectOptional(
                  id = "filter".refined,
                  view = filter(props.observingMode),
                  defaultValue = initialFilterLens.get(props.observingMode.get),
                  label = "Filter".some,
                  helpId = Some("configuration/gmos/filter.md".refined),
                  disabled = disableAdvancedEdit,
                  showClear = true,
                  resetToOriginal = true,
                  showCustomization = showCustomization,
                  allowRevertCustomization = allowRevertCustomization
                ),
                maskControl(props),
                if (fpuControlReadonly(props))
                  React.Fragment(
                    FormLabel(htmlFor = "fpu".refined)(
                      fpuLabel,
                      fpuHelpId.map(HelpIcon(_)).whenDefined
                    ),
                    <.label(^.id := "fpu",
                            ExploreStyles.FormValue |+| ExploreStyles.FormValueRight,
                            Display[Fpu].shortName(fpu(props.observingMode).get)
                    )
                  )
                else
                  CustomizableEnumSelect(
                    id = "fpu".refined,
                    view = fpu(props.observingMode),
                    defaultValue = initialFpuLens.get(props.observingMode.get),
                    label = fpuLabel.some,
                    helpId = fpuHelpId,
                    disabled = disableAdvancedEdit,
                    exclude = excludedFpus,
                    showCustomization = showCustomization,
                    allowRevertCustomization = allowRevertCustomization
                  ),
                offsetsControl(props, disableSimpleEdit)
              ),
              <.div(LucumaPrimeStyles.FormColumnCompact)(
                CustomizableInputText(
                  id = "central-wavelength".refined,
                  value = centralWavelengthView,
                  label =
                    React.Fragment("Central Wavelength",
                                   HelpIcon("configuration/gmos/central=wavelength.md".refined)
                    ),
                  units = props.units.symbol.some,
                  validFormat = props.units.toInputFormat,
                  changeAuditor = props.units.toAuditor,
                  defaultValue = initialCentralWavelength,
                  disabled = disableSimpleEdit,
                  showCustomization = showCustomization,
                  allowRevertCustomization = allowRevertCustomization
                ),
                dithersControl(props.sequenceChanged),
                ExposureTimeModeEditor(
                  instrument = props.instrument,
                  wavelength = none,
                  exposureTimeMode = exposureTimeMode(props.observingMode),
                  coadds = none,
                  scienceMode = ScienceMode.Spectroscopy,
                  readonly = !props.permissions.isFullEdit,
                  units = props.units,
                  calibrationRole = props.calibrationRole,
                  idPrefix = "gmosLongslit".refined
                )
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
                <.div(
                  ExploreStyles.GmosLongSlitBinning,
                  CustomizableEnumSelectOptional(
                    id = "explicitXBin".refined,
                    view = explicitXBinning(props.observingMode).withDefault(defaultXBinning),
                    defaultValue = defaultXBinning.some,
                    disabled = disableAdvancedEdit,
                    dropdownMods = ^.aria.label := "X Binning",
                    showCustomization = showCustomization,
                    allowRevertCustomization = allowRevertCustomization
                  ),
                  <.label(^.htmlFor := "explicitYBin", "x"),
                  CustomizableEnumSelectOptional(
                    id = "explicitYBin".refined,
                    view = explicitYBinning(props.observingMode).withDefault(defaultYBinning),
                    defaultValue = defaultYBinning.some,
                    disabled = disableAdvancedEdit,
                    dropdownMods = ^.aria.label := "Y Binning",
                    showCustomization = showCustomization,
                    allowRevertCustomization = allowRevertCustomization
                  )
                ),
                CustomizableEnumSelectOptional(
                  id = "explicitReadMode".refined,
                  view = explicitReadModeGain(props.observingMode)
                    .withDefault(defaultReadModeGain, resolvedReadModeGain),
                  defaultValue = defaultReadModeGain.some,
                  label = "Read Mode".some,
                  helpId = Some("configuration/gmos/read-mode.md".refined),
                  disabled = disableAdvancedEdit,
                  showCustomization = showCustomization,
                  allowRevertCustomization = allowRevertCustomization
                ),
                CustomizableEnumSelectOptional(
                  id = "explicitRoi".refined,
                  view = explicitRoi(props.observingMode).withDefault(defaultRoi),
                  defaultValue = defaultRoi.some,
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
            <.div(
              ExploreStyles.GmosLongSlitLowerGrid,
              Panel(
                header = <.span(
                  "Acquisition",
                  HelpIcon("configuration/gmos/acquisition-customization.md".refined)
                ),
                toggleable = true,
                collapsed = true
              )(
                acquisitionSection(props, disableAdvancedAcqEdit)
              ).when(showAcquisitionConfig),
              AdvancedConfigButtons(
                editState = editState,
                isCustomized = isCustomized(props.observingMode),
                revertConfig = props.revertConfig,
                revertCustomizations = revertCustomizations(props.observingMode),
                sequenceChanged = props.sequenceChanged,
                readonly = !props.permissions.isFullEdit
              )
            )
          )
  }

  /**
   * The long slit Acquisition panel contents for GS and GN
   */
  private def longSlitAcqPanel[Filter: Enumerated: Display](
    props:            GmosSpectroscopyConfigPanel[?, ?],
    disabled:         Boolean,
    roiView:          View[Option[GmosLongSlitAcquisitionRoi]],
    defaultRoi:       GmosLongSlitAcquisitionRoi,
    filterView:       View[Option[Filter]],
    defaultFilter:    Filter,
    excludedFilters:  Set[Filter],
    exposureTimeMode: View[ExposureTimeMode]
  ): VdomNode =
    <.div(
      ExploreStyles.AcquisitionCustomizationGrid,
      <.div(
        LucumaPrimeStyles.FormColumnCompact,
        CustomizableEnumSelectOptional(
          id = "acq-explicit-roi".refined,
          view = roiView,
          defaultValue = defaultRoi.some,
          label = "ROI".some,
          helpId = None,
          disabled = disabled,
          showCustomization = props.showCustomization,
          allowRevertCustomization =
            props.allowRevertCustomization || props.permissions.isOnlyForOngoing
        ),
        CustomizableEnumSelectOptional(
          id = "acq-explicit-filter".refined,
          view = filterView,
          defaultValue = defaultFilter.some,
          exclude = excludedFilters,
          label = "Filter".some,
          helpId = None,
          disabled = disabled,
          showCustomization = props.showCustomization,
          allowRevertCustomization =
            props.allowRevertCustomization || props.permissions.isOnlyForOngoing
        )
      ),
      <.div(
        LucumaPrimeStyles.FormColumnCompact,
        ExposureTimeModeEditor(
          instrument = props.observingMode.get.instrument,
          wavelength = none,
          exposureTimeMode = exposureTimeMode,
          coadds = none,
          scienceMode = ScienceMode.Imaging,
          readonly = props.permissions.isReadonly,
          units = props.units,
          calibrationRole = props.calibrationRole,
          idPrefix = "gmosAcq".refined,
          forceCount = Some(1.refined)
        )
      )
    )

  /**
   * The MOS Acquisition panel contents same for GN and GS
   */
  private def mosAcquisitionPanel[Filter: Enumerated: Display](
    props:            GmosSpectroscopyConfigPanel[?, ?],
    disabled:         Boolean,
    acquisitionType:  View[GmosMosAcquisitionType],
    filterView:       View[Option[Filter]],
    defaultFilter:    Filter,
    excludedFilters:  Set[Filter],
    exposureTimeMode: View[ExposureTimeMode]
  ): VdomNode =
    <.div(
      ExploreStyles.AcquisitionCustomizationGrid,
      <.div(
        LucumaPrimeStyles.FormColumnCompact,
        FormEnumDropdownView(
          id = "acq-type".refined,
          value = acquisitionType,
          label = "Acquisition Type",
          disabled = props.permissions.isReadonly
        ),
        CustomizableEnumSelectOptional(
          id = "acq-explicit-filter".refined,
          view = filterView,
          defaultValue = defaultFilter.some,
          exclude = excludedFilters,
          label = "Filter".some,
          helpId = None,
          disabled = disabled,
          showCustomization = props.showCustomization,
          allowRevertCustomization =
            props.allowRevertCustomization || props.permissions.isOnlyForOngoing
        )
      ),
      <.div(
        LucumaPrimeStyles.FormColumnCompact,
        // MOS acquisition is always a single exposure and the ODB rejects a
        // signal-to-noise mode, so only the exposure time is offered.
        ExposureTimeModeEditor(
          instrument = props.observingMode.get.instrument,
          wavelength = none,
          exposureTimeMode = exposureTimeMode,
          coadds = none,
          scienceMode = ScienceMode.Imaging,
          readonly = props.permissions.isReadonly,
          units = props.units,
          calibrationRole = props.calibrationRole,
          idPrefix = "gmosMosAcq".refined,
          forceCount = Some(1.refined),
          forceModeType = Some(ExposureTimeModeType.TimeAndCount)
        )
      )
    )

  // Gmos North Long Slit
  case class GmosNorthLongSlit(
    programId:       Program.Id,
    obsId:           Observation.Id,
    calibrationRole: Option[CalibrationRole],
    observingMode:   Aligner[ObservingMode.GmosNorthLongSlit, GmosNorthLongSlitInput],
    revertConfig:    IO[Unit],
    confMatrix:      SpectroscopyModesMatrix,
    sequenceChanged: Callback,
    permissions:     ConfigEditPermissions,
    units:           WavelengthUnits
  ) extends ReactFnProps[GmosSpectroscopyConfigPanel.GmosNorthLongSlit](
        GmosSpectroscopyConfigPanel.GmosNorthLongSlit.component
      )
      with GmosSpectroscopyConfigPanel[
        ObservingMode.GmosNorthLongSlit,
        GmosNorthLongSlitInput
      ]

  object GmosNorthLongSlit
      extends GmosSpectroscopyConfigPanelBuilder[
        ObservingMode.GmosNorthLongSlit,
        GmosNorthLongSlitInput,
        GmosSpectroscopyConfigPanel.GmosNorthLongSlit,
        GmosNorthGrating,
        GmosNorthFilter,
        GmosNorthFpu
      ] {

    override protected def maskControl(
      props: GmosNorthLongSlit
    )(using MonadError[IO, Throwable], Dispatch[IO], Logger[IO]): VdomNode = EmptyVdom

    inline override protected def revertCustomizations(
      aligner: AA
    )(using MonadError[IO, Throwable], Effect.Dispatch[IO], Logger[IO]): Callback =
      aligner.view(_.toInput).mod(_.revertCustomizations)

    inline override protected def centralWavelength(
      aligner: AA
    )(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Wavelength] =
      aligner
        .zoom(
          ObservingMode.GmosNorthLongSlit.centralWavelength.andThen(CentralWavelength.Value),
          GmosNorthLongSlitInput.centralWavelength.modify
        )
        .view(_.toInput.assign)

    inline override protected def grating(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[GmosNorthGrating] =
      aligner
        .zoom(
          ObservingMode.GmosNorthLongSlit.grating,
          GmosNorthLongSlitInput.grating.modify
        )
        .view(_.assign)

    inline override protected def filter(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosNorthFilter]] = aligner
      .zoom(
        ObservingMode.GmosNorthLongSlit.filter,
        GmosNorthLongSlitInput.filter.modify
      )
      .view(_.orUnassign)

    inline override protected def fpu(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[GmosNorthFpu] = aligner
      .zoom(
        ObservingMode.GmosNorthLongSlit.fpu,
        GmosNorthLongSlitInput.fpu.modify
      )
      .view(_.assign)

    inline override protected def explicitXBinning(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosXBinning]] = aligner
      .zoom(
        ObservingMode.GmosNorthLongSlit.explicitXBin,
        GmosNorthLongSlitInput.explicitXBin.modify
      )
      .view(_.map(_.value).orUnassign)

    inline override protected def explicitYBinning(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosYBinning]] = aligner
      .zoom(
        ObservingMode.GmosNorthLongSlit.explicitYBin,
        GmosNorthLongSlitInput.explicitYBin.modify
      )
      .view(_.map(_.value).orUnassign)

    private val explicitReadMode =
      ObservingMode.GmosNorthLongSlit.explicitAmpReadMode

    private val explicitGain =
      ObservingMode.GmosNorthLongSlit.explicitAmpGain

    private def readGainAligner(
      aligner: AA
    ): Aligner[Option[(GmosAmpReadMode, GmosAmpGain)], GmosNorthLongSlitInput] =
      aligner
        .zoom(unsafeDisjointOptionZip(explicitReadMode, explicitGain), f => i => f(i))

    inline override protected def explicitReadModeGain(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[(GmosAmpReadMode, GmosAmpGain)]] =
      readGainAligner(aligner)
        .viewMod { org =>
          val rg = org.unzip
          GmosNorthLongSlitInput.explicitAmpReadMode
            .replace(rg._1.orUnassign)
            .andThen(GmosNorthLongSlitInput.explicitAmpGain.replace(rg._2.orUnassign))
        }

    inline override protected def explicitRoi(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosRoi]] = aligner
      .zoom(
        ObservingMode.GmosNorthLongSlit.explicitRoi,
        GmosNorthLongSlitInput.explicitRoi.modify
      )
      .view(_.orUnassign)

    inline override protected def explicitWavelengthDithers(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[NonEmptyList[WavelengthDither]]] = aligner
      .zoom(
        ObservingMode.GmosNorthLongSlit.explicitWavelengthDithers,
        GmosNorthLongSlitInput.explicitWavelengthDithers.modify
      )
      .view(_.map(_.map(_.toInput).toList).orUnassign)

    inline override protected def offsetsControl(props: GmosNorthLongSlit, disabled: Boolean)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): VdomNode =
      // TODO: GMOS has no offset presets of its own.  Giving it a single "Default"
      // preset means adding a GmosLongSlitOffsetPreset to lucuma-core, since
      // SlitOffsetPreset is sealed there, and core publishing is currently blocked in
      // the pipeline by the pending "for_review" changes.  The GNIRS presets have the
      // same two shapes, so they keep the along-slit / to-sky toggle working meanwhile.
      SlitTelescopeConfigsEditor[GnirsSlitOffsetPreset](
        explicitValue = props.observingMode
          .zoom(
            ObservingMode.GmosNorthLongSlit.explicitTelescopeConfigs,
            GmosNorthLongSlitInput.explicitTelescopeConfigs.modify
          )
          .view(_.map(_.toInput).orUnassign),
        defaultValue =
          ObservingMode.GmosNorthLongSlit.defaultTelescopeConfigs.get(props.observingMode.get),
        defaultForPreset =
          _ => ObservingMode.GmosNorthLongSlit.defaultTelescopeConfigs.get(props.observingMode.get),
        helpId = "configuration/offsets.md".refined,
        presetsReadonly = disabled,
        editingReadonly = disabled
      )

    inline protected def exposureTimeMode(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[ExposureTimeMode] = aligner
      .zoom(
        ObservingMode.GmosNorthLongSlit.exposureTimeMode,
        GmosNorthLongSlitInput.exposureTimeMode.modify
      )
      .view(_.toInput.assign)

    inline private def acquisition(
      aligner: AA
    ): Aligner[ObservingMode.GmosNorthLongSlit.Acquisition, GmosNorthLongSlitAcquisitionInput] =
      aligner
        .zoom(
          ObservingMode.GmosNorthLongSlit.acquisition,
          forceAssign(GmosNorthLongSlitInput.acquisition.modify)(
            GmosNorthLongSlitAcquisitionInput()
          )
        )

    inline private def explicitAcquisitionFilter(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosNorthFilter]] = acquisition(aligner)
      .zoom(ObservingMode.GmosNorthLongSlit.Acquisition.explicitFilter,
            GmosNorthLongSlitAcquisitionInput.explicitFilter.modify
      )
      .view(_.orUnassign)

    inline private def explicitAcquisitionRoi(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosLongSlitAcquisitionRoi]] = acquisition(aligner)
      .zoom(ObservingMode.GmosNorthLongSlit.Acquisition.explicitRoi,
            GmosNorthLongSlitAcquisitionInput.explicitRoi.modify
      )
      .view(_.orUnassign)

    inline private def acquisitionExposureTimeModeView(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[ExposureTimeMode] = acquisition(aligner)
      .zoom(ObservingMode.GmosNorthLongSlit.Acquisition.exposureTimeMode,
            GmosNorthLongSlitAcquisitionInput.exposureTimeMode.modify
      )
      .view(_.toInput.assign)

    override protected def acquisitionSection(
      props:    GmosSpectroscopyConfigPanel.GmosNorthLongSlit,
      disabled: Boolean
    )(using MonadError[IO, Throwable], Effect.Dispatch[IO], Logger[IO]): VdomNode =
      val defaultAcquisitionFilter = defaultAcquisitionFilterLens.get(props.observingMode.get)
      val defaultAcquisitionRoi    = defaultAcquisitionRoiLens.get(props.observingMode.get)
      longSlitAcqPanel(
        props,
        disabled,
        explicitAcquisitionRoi(props.observingMode).withDefault(defaultAcquisitionRoi),
        defaultAcquisitionRoi,
        explicitAcquisitionFilter(props.observingMode).withDefault(defaultAcquisitionFilter),
        defaultAcquisitionFilter,
        excludedAcquisitionFilters,
        acquisitionExposureTimeModeView(props.observingMode)
      )

    override protected val initialGratingLens           =
      ObservingMode.GmosNorthLongSlit.initialGrating
    override protected val initialFilterLens            = ObservingMode.GmosNorthLongSlit.initialFilter
    override protected val initialFpuLens               = ObservingMode.GmosNorthLongSlit.initialFpu
    override protected val initialCentralWavelengthLens =
      ObservingMode.GmosNorthLongSlit.initialCentralWavelength.andThen(CentralWavelength.Value)
    protected val defaultBinningLens                    =
      (ObservingMode.GmosNorthLongSlit.defaultXBin,
       ObservingMode.GmosNorthLongSlit.defaultYBin
      ).disjointZip
    protected val defaultReadModeGainLens               =
      (ObservingMode.GmosNorthLongSlit.defaultAmpReadMode,
       ObservingMode.GmosNorthLongSlit.defaultAmpGain
      ).disjointZip
    protected val defaultXBinningLens                   = ObservingMode.GmosNorthLongSlit.defaultXBin
    protected val defaultYBinningLens                   = ObservingMode.GmosNorthLongSlit.defaultYBin
    protected val defaultRoiLens                        = ObservingMode.GmosNorthLongSlit.defaultRoi
    override protected val defaultWavelengthDithersLens =
      ObservingMode.GmosNorthLongSlit.defaultWavelengthDithers

    override protected val excludedFpus: Set[GmosNorthFpu] =
      Enumerated[GmosNorthFpu].all.filter(_.fpuType =!= GmosFpuType.LongSlit).toSet

    override protected val fpuLabel: String           = "FPU"
    override protected val fpuHelpId: Option[Help.Id] =
      Some("configuration/gmos/fpu.md".refined)

    private val excludedAcquisitionFilters: Set[GmosNorthFilter] =
      Enumerated[GmosNorthFilter].all.toSet -- GmosNorthFilter.acquisition.toList.toSet

    private val defaultAcquisitionFilterLens
      : Lens[ObservingMode.GmosNorthLongSlit, GmosNorthFilter]            =
      ObservingMode.GmosNorthLongSlit.acquisition.andThen(
        ObservingMode.GmosNorthLongSlit.Acquisition.defaultFilter
      )
    private val defaultAcquisitionRoiLens
      : Lens[ObservingMode.GmosNorthLongSlit, GmosLongSlitAcquisitionRoi] =
      ObservingMode.GmosNorthLongSlit.acquisition.andThen(
        ObservingMode.GmosNorthLongSlit.Acquisition.defaultRoi
      )

    inline override protected def resolvedReadModeGainGetter = mode =>
      val readMode = ObservingMode.GmosNorthLongSlit.explicitAmpReadMode
        .get(mode)
        .getOrElse(ObservingMode.GmosNorthLongSlit.defaultAmpReadMode.get(mode))
      val ampGain  = ObservingMode.GmosNorthLongSlit.explicitAmpGain
        .get(mode)
        .getOrElse(ObservingMode.GmosNorthLongSlit.defaultAmpGain.get(mode))
      (readMode, ampGain)
  }

  // Gmos South Long Slit

  case class GmosSouthLongSlit(
    programId:       Program.Id,
    obsId:           Observation.Id,
    calibrationRole: Option[CalibrationRole],
    observingMode:   Aligner[ObservingMode.GmosSouthLongSlit, GmosSouthLongSlitInput],
    revertConfig:    IO[Unit],
    confMatrix:      SpectroscopyModesMatrix,
    sequenceChanged: Callback,
    permissions:     ConfigEditPermissions,
    units:           WavelengthUnits
  ) extends ReactFnProps[GmosSpectroscopyConfigPanel.GmosSouthLongSlit](
        GmosSpectroscopyConfigPanel.GmosSouthLongSlit.component
      )
      with GmosSpectroscopyConfigPanel[
        ObservingMode.GmosSouthLongSlit,
        GmosSouthLongSlitInput
      ]

  object GmosSouthLongSlit
      extends GmosSpectroscopyConfigPanelBuilder[
        ObservingMode.GmosSouthLongSlit,
        GmosSouthLongSlitInput,
        GmosSpectroscopyConfigPanel.GmosSouthLongSlit,
        GmosSouthGrating,
        GmosSouthFilter,
        GmosSouthFpu
      ] {

    override protected def maskControl(
      props: GmosSouthLongSlit
    )(using MonadError[IO, Throwable], Dispatch[IO], Logger[IO]): VdomNode = EmptyVdom

    inline override protected def revertCustomizations(
      aligner: AA
    )(using MonadError[IO, Throwable], Effect.Dispatch[IO], Logger[IO]): Callback =
      aligner.view(_.toInput).mod(_.revertCustomizations)

    inline override def centralWavelength(
      aligner: AA
    )(using MonadError[IO, Throwable], Effect.Dispatch[IO], Logger[IO]): View[Wavelength] =
      aligner
        .zoom(
          ObservingMode.GmosSouthLongSlit.centralWavelength.andThen(CentralWavelength.Value),
          GmosSouthLongSlitInput.centralWavelength.modify
        )
        .view(_.toInput.assign)

    inline override protected def grating(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[GmosSouthGrating] =
      aligner
        .zoom(
          ObservingMode.GmosSouthLongSlit.grating,
          GmosSouthLongSlitInput.grating.modify
        )
        .view(_.assign)

    inline override protected def filter(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosSouthFilter]] =
      aligner
        .zoom(
          ObservingMode.GmosSouthLongSlit.filter,
          GmosSouthLongSlitInput.filter.modify
        )
        .view(_.orUnassign)

    inline override protected def fpu(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[GmosSouthFpu] = aligner
      .zoom(
        ObservingMode.GmosSouthLongSlit.fpu,
        GmosSouthLongSlitInput.fpu.modify
      )
      .view(_.assign)

    inline override protected def explicitXBinning(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosXBinning]] = aligner
      .zoom(
        ObservingMode.GmosSouthLongSlit.explicitXBin,
        GmosSouthLongSlitInput.explicitXBin.modify
      )
      .view(_.map(_.value).orUnassign)

    inline override protected def explicitYBinning(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosYBinning]] = aligner
      .zoom(
        ObservingMode.GmosSouthLongSlit.explicitYBin,
        GmosSouthLongSlitInput.explicitYBin.modify
      )
      .view(_.map(_.value).orUnassign)

    private val explicitReadMode =
      ObservingMode.GmosSouthLongSlit.explicitAmpReadMode

    private val explicitGain =
      ObservingMode.GmosSouthLongSlit.explicitAmpGain

    private def readGainAligner(
      aligner: AA
    ): Aligner[Option[(GmosAmpReadMode, GmosAmpGain)], GmosSouthLongSlitInput] =
      aligner
        .zoom(unsafeDisjointOptionZip(explicitReadMode, explicitGain), f => i => f(i))

    inline override protected def explicitReadModeGain(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[(GmosAmpReadMode, GmosAmpGain)]] =
      readGainAligner(aligner)
        .viewMod { org =>
          val rg = org.unzip
          GmosSouthLongSlitInput.explicitAmpReadMode
            .replace(rg._1.orUnassign)
            .andThen(GmosSouthLongSlitInput.explicitAmpGain.replace(rg._2.orUnassign))
        }

    inline override protected def explicitRoi(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosRoi]] = aligner
      .zoom(
        ObservingMode.GmosSouthLongSlit.explicitRoi,
        GmosSouthLongSlitInput.explicitRoi.modify
      )
      .view(_.orUnassign)

    inline override protected def explicitWavelengthDithers(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[NonEmptyList[WavelengthDither]]] = aligner
      .zoom(
        ObservingMode.GmosSouthLongSlit.explicitWavelengthDithers,
        GmosSouthLongSlitInput.explicitWavelengthDithers.modify
      )
      .view(
        _.map(
          _.map(d => WavelengthDitherInput.Picometers(d.toPicometers.value)).toList
        ).orUnassign
      )

    inline override protected def offsetsControl(props: GmosSouthLongSlit, disabled: Boolean)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): VdomNode =
      // TODO: GMOS has no offset presets of its own.  Giving it a single "Default"
      // preset means adding a GmosLongSlitOffsetPreset to lucuma-core, since
      // SlitOffsetPreset is sealed there, and core publishing is currently blocked in
      // the pipeline by the pending "for_review" changes.  The GNIRS presets have the
      // same two shapes, so they keep the along-slit / to-sky toggle working meanwhile.
      SlitTelescopeConfigsEditor[GnirsSlitOffsetPreset](
        explicitValue = props.observingMode
          .zoom(
            ObservingMode.GmosSouthLongSlit.explicitTelescopeConfigs,
            GmosSouthLongSlitInput.explicitTelescopeConfigs.modify
          )
          .view(_.map(_.toInput).orUnassign),
        defaultValue =
          ObservingMode.GmosSouthLongSlit.defaultTelescopeConfigs.get(props.observingMode.get),
        defaultForPreset =
          _ => ObservingMode.GmosSouthLongSlit.defaultTelescopeConfigs.get(props.observingMode.get),
        helpId = "configuration/offsets.md".refined,
        presetsReadonly = disabled,
        editingReadonly = disabled
      )

    inline protected def exposureTimeMode(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[ExposureTimeMode] = aligner
      .zoom(
        ObservingMode.GmosSouthLongSlit.exposureTimeMode,
        GmosSouthLongSlitInput.exposureTimeMode.modify
      )
      .view(_.toInput.assign)

    inline private def acquisition(
      aligner: AA
    ): Aligner[ObservingMode.GmosSouthLongSlit.Acquisition, GmosSouthLongSlitAcquisitionInput] =
      aligner
        .zoom(
          ObservingMode.GmosSouthLongSlit.acquisition,
          forceAssign(GmosSouthLongSlitInput.acquisition.modify)(
            GmosSouthLongSlitAcquisitionInput()
          )
        )

    override protected val excludedFpus: Set[GmosSouthFpu] =
      Enumerated[GmosSouthFpu].all.filter(_.fpuType =!= GmosFpuType.LongSlit).toSet

    override protected val fpuLabel: String           = "FPU"
    override protected val fpuHelpId: Option[Help.Id] =
      Some("configuration/gmos/fpu.md".refined)

    private val excludedAcquisitionFilters: Set[GmosSouthFilter] =
      Enumerated[GmosSouthFilter].all.toSet -- GmosSouthFilter.acquisition.toList.toSet

    inline private def explicitAcquisitionFilter(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosSouthFilter]] = acquisition(aligner)
      .zoom(ObservingMode.GmosSouthLongSlit.Acquisition.explicitFilter,
            GmosSouthLongSlitAcquisitionInput.explicitFilter.modify
      )
      .view(_.orUnassign)

    inline private def explicitAcquisitionRoi(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosLongSlitAcquisitionRoi]] = acquisition(aligner)
      .zoom(ObservingMode.GmosSouthLongSlit.Acquisition.explicitRoi,
            GmosSouthLongSlitAcquisitionInput.explicitRoi.modify
      )
      .view(_.orUnassign)

    inline private def acquisitionExposureTimeModeView(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[ExposureTimeMode] = acquisition(aligner)
      .zoom(ObservingMode.GmosSouthLongSlit.Acquisition.exposureTimeMode,
            GmosSouthLongSlitAcquisitionInput.exposureTimeMode.modify
      )
      .view(_.toInput.assign)

    override protected def acquisitionSection(
      props:    GmosSpectroscopyConfigPanel.GmosSouthLongSlit,
      disabled: Boolean
    )(using MonadError[IO, Throwable], Effect.Dispatch[IO], Logger[IO]): VdomNode =
      val defaultAcquisitionFilter = defaultAcquisitionFilterLens.get(props.observingMode.get)
      val defaultAcquisitionRoi    = defaultAcquisitionRoiLens.get(props.observingMode.get)
      longSlitAcqPanel(
        props,
        disabled,
        explicitAcquisitionRoi(props.observingMode).withDefault(defaultAcquisitionRoi),
        defaultAcquisitionRoi,
        explicitAcquisitionFilter(props.observingMode).withDefault(defaultAcquisitionFilter),
        defaultAcquisitionFilter,
        excludedAcquisitionFilters,
        acquisitionExposureTimeModeView(props.observingMode)
      )

    override protected val initialGratingLens           =
      ObservingMode.GmosSouthLongSlit.initialGrating
    override protected val initialFilterLens            = ObservingMode.GmosSouthLongSlit.initialFilter
    override protected val initialFpuLens               = ObservingMode.GmosSouthLongSlit.initialFpu
    override protected val initialCentralWavelengthLens =
      ObservingMode.GmosSouthLongSlit.initialCentralWavelength.andThen(CentralWavelength.Value)
    protected val defaultBinningLens                    =
      (ObservingMode.GmosSouthLongSlit.defaultXBin,
       ObservingMode.GmosSouthLongSlit.defaultYBin
      ).disjointZip
    protected val defaultXBinningLens                   = ObservingMode.GmosSouthLongSlit.defaultXBin
    protected val defaultYBinningLens                   = ObservingMode.GmosSouthLongSlit.defaultYBin
    protected val defaultReadModeGainLens               =
      (ObservingMode.GmosSouthLongSlit.defaultAmpReadMode,
       ObservingMode.GmosSouthLongSlit.defaultAmpGain
      ).disjointZip
    protected val defaultRoiLens                        = ObservingMode.GmosSouthLongSlit.defaultRoi
    override protected val defaultWavelengthDithersLens =
      ObservingMode.GmosSouthLongSlit.defaultWavelengthDithers

    private val defaultAcquisitionFilterLens
      : Lens[ObservingMode.GmosSouthLongSlit, GmosSouthFilter]            =
      ObservingMode.GmosSouthLongSlit.acquisition.andThen(
        ObservingMode.GmosSouthLongSlit.Acquisition.defaultFilter
      )
    private val defaultAcquisitionRoiLens
      : Lens[ObservingMode.GmosSouthLongSlit, GmosLongSlitAcquisitionRoi] =
      ObservingMode.GmosSouthLongSlit.acquisition.andThen(
        ObservingMode.GmosSouthLongSlit.Acquisition.defaultRoi
      )

    inline override protected def resolvedReadModeGainGetter = mode =>
      val readMode = ObservingMode.GmosSouthLongSlit.explicitAmpReadMode
        .get(mode)
        .getOrElse(ObservingMode.GmosSouthLongSlit.defaultAmpReadMode.get(mode))
      val ampGain  = ObservingMode.GmosSouthLongSlit.explicitAmpGain
        .get(mode)
        .getOrElse(ObservingMode.GmosSouthLongSlit.defaultAmpGain.get(mode))
      (readMode, ampGain)
  }

  /**
   * Intermediate builder for the two MOS panels.
   */
  sealed abstract class GmosMosConfigPanelBuilder[
    T <: ObservingMode,
    Input,
    Props <: GmosMosConfigPanel[T, Input],
    Grating: Enumerated: Display,
    Filter: Enumerated: Display
  ] extends GmosSpectroscopyConfigPanelBuilder[
        T,
        Input,
        Props,
        Grating,
        Filter,
        GmosCustomSlitWidth
      ] {
    override protected val excludedFpus: Set[GmosCustomSlitWidth] = Set.empty

    override protected val fpuLabel: String = "Custom Slit Width"

    override protected val fpuHelpId: Option[Help.Id] =
      Some("configuration/gmos/mos-slit-width.md".refined)

    protected val maskInstrument: Instrument

    protected def customMaskAttachmentId(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[Attachment.Id]]

    private def maskIsBound(props: Props)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): Boolean = customMaskAttachmentId(props.observingMode).get.isDefined

    override protected def maskControl(props: Props)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): VdomNode =
      // Only shown once the proposal is accepted
      if (props.maskContext.pickerActive)
        MosMaskPicker(
          instrument = maskInstrument,
          attachmentIdView = customMaskAttachmentId(props.observingMode),
          attachments = props.maskContext.attachments,
          obsAttachmentIds = props.maskContext.obsAttachmentIds,
          helpId = "configuration/gmos/mos-mask.md".refined,
          disabled = !props.permissions.isFullEdit
        )
      else EmptyVdom

    override protected def fpuControlReadonly(props: Props)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): Boolean =
      props.maskContext.pickerActive && maskIsBound(props)
  }

  // Gmos North MOS
  case class GmosNorthMos(
    programId:       Program.Id,
    obsId:           Observation.Id,
    calibrationRole: Option[CalibrationRole],
    observingMode:   Aligner[ObservingMode.GmosNorthMos, GmosNorthMosInput],
    revertConfig:    IO[Unit],
    confMatrix:      SpectroscopyModesMatrix,
    sequenceChanged: Callback,
    permissions:     ConfigEditPermissions,
    units:           WavelengthUnits,
    maskContext:     MosMaskContext
  ) extends ReactFnProps[GmosSpectroscopyConfigPanel.GmosNorthMos](
        GmosSpectroscopyConfigPanel.GmosNorthMos.component
      )
      with GmosMosConfigPanel[
        ObservingMode.GmosNorthMos,
        GmosNorthMosInput
      ]

  object GmosNorthMos
      extends GmosMosConfigPanelBuilder[
        ObservingMode.GmosNorthMos,
        GmosNorthMosInput,
        GmosSpectroscopyConfigPanel.GmosNorthMos,
        GmosNorthGrating,
        GmosNorthFilter
      ] {

    override protected val maskInstrument: Instrument = Instrument.GmosNorth

    inline override protected def revertCustomizations(
      aligner: AA
    )(using MonadError[IO, Throwable], Effect.Dispatch[IO], Logger[IO]): Callback =
      aligner.view(_.toInput).mod(_.revertCustomizations)

    inline override protected def centralWavelength(
      aligner: AA
    )(using MonadError[IO, Throwable], Effect.Dispatch[IO], Logger[IO]): View[Wavelength] =
      aligner
        .zoom(
          ObservingMode.GmosNorthMos.centralWavelength.andThen(CentralWavelength.Value),
          GmosNorthMosInput.centralWavelength.modify
        )
        .view(_.toInput.assign)

    inline override protected def grating(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[GmosNorthGrating] =
      aligner
        .zoom(
          ObservingMode.GmosNorthMos.grating,
          GmosNorthMosInput.grating.modify
        )
        .view(_.assign)

    inline override protected def filter(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosNorthFilter]] = aligner
      .zoom(
        ObservingMode.GmosNorthMos.filter,
        GmosNorthMosInput.filter.modify
      )
      .view(_.orUnassign)

    // The Input's customMask is optional and its slitWidth required.
    inline private def customMask(
      aligner: AA
    ): Aligner[ObservingMode.GmosCustomMask, GmosCustomMaskInput] =
      aligner
        .zoom(
          ObservingMode.GmosNorthMos.customMask,
          forceAssign(GmosNorthMosInput.customMask.modify)(aligner.get.customMask.toInput)
        )

    inline override protected def fpu(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[GmosCustomSlitWidth] = customMask(aligner)
      .zoom(
        ObservingMode.GmosCustomMask.slitWidth,
        GmosCustomMaskInput.slitWidth.modify
      )
      .view(identity)

    override protected def customMaskAttachmentId(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[Attachment.Id]] = customMask(aligner)
      .zoom(
        ObservingMode.GmosCustomMask.attachmentId,
        GmosCustomMaskInput.attachmentId.modify
      )
      .view(_.orUnassign)

    inline private def acquisitionType(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[GmosMosAcquisitionType] = aligner
      .zoom(
        ObservingMode.GmosNorthMos.acquisitionType,
        GmosNorthMosInput.acquisitionType.modify
      )
      .view(_.assign)

    inline private def acquisition(
      aligner: AA
    ): Aligner[ObservingMode.GmosNorthMos.Acquisition, GmosNorthMosAcquisitionInput] =
      aligner
        .zoom(
          ObservingMode.GmosNorthMos.acquisition,
          forceAssign(GmosNorthMosInput.acquisition.modify)(
            GmosNorthMosAcquisitionInput()
          )
        )

    inline private def explicitAcquisitionFilter(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosNorthFilter]] = acquisition(aligner)
      .zoom(ObservingMode.GmosNorthMos.Acquisition.explicitFilter,
            GmosNorthMosAcquisitionInput.explicitFilter.modify
      )
      .view(_.orUnassign)

    inline private def acquisitionExposureTimeModeView(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[ExposureTimeMode] = acquisition(aligner)
      .zoom(ObservingMode.GmosNorthMos.Acquisition.exposureTimeMode,
            GmosNorthMosAcquisitionInput.exposureTimeMode.modify
      )
      .view(_.toInput.assign)

    private val excludedAcquisitionFilters: Set[GmosNorthFilter] =
      Enumerated[GmosNorthFilter].all.toSet -- GmosNorthFilter.acquisition.toList.toSet

    private val defaultAcquisitionFilterLens: Lens[ObservingMode.GmosNorthMos, GmosNorthFilter] =
      ObservingMode.GmosNorthMos.acquisition.andThen(
        ObservingMode.GmosNorthMos.Acquisition.defaultFilter
      )

    inline override protected def explicitXBinning(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosXBinning]] = aligner
      .zoom(
        ObservingMode.GmosNorthMos.explicitXBin,
        GmosNorthMosInput.explicitXBin.modify
      )
      .view(_.map(_.value).orUnassign)

    inline override protected def explicitYBinning(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosYBinning]] = aligner
      .zoom(
        ObservingMode.GmosNorthMos.explicitYBin,
        GmosNorthMosInput.explicitYBin.modify
      )
      .view(_.map(_.value).orUnassign)

    private def readGainAligner(
      aligner: AA
    ): Aligner[Option[(GmosAmpReadMode, GmosAmpGain)], GmosNorthMosInput] =
      aligner
        .zoom(
          unsafeDisjointOptionZip(ObservingMode.GmosNorthMos.explicitAmpReadMode,
                                  ObservingMode.GmosNorthMos.explicitAmpGain
          ),
          f => i => f(i)
        )

    inline override protected def explicitReadModeGain(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[(GmosAmpReadMode, GmosAmpGain)]] =
      readGainAligner(aligner)
        .viewMod { org =>
          val rg = org.unzip
          GmosNorthMosInput.explicitAmpReadMode
            .replace(rg._1.orUnassign)
            .andThen(GmosNorthMosInput.explicitAmpGain.replace(rg._2.orUnassign))
        }

    inline override protected def explicitRoi(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosRoi]] = aligner
      .zoom(
        ObservingMode.GmosNorthMos.explicitRoi,
        GmosNorthMosInput.explicitRoi.modify
      )
      .view(_.orUnassign)

    inline override protected def explicitWavelengthDithers(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[NonEmptyList[WavelengthDither]]] = aligner
      .zoom(
        ObservingMode.GmosNorthMos.explicitWavelengthDithers,
        GmosNorthMosInput.explicitWavelengthDithers.modify
      )
      .view(_.map(_.map(_.toInput).toList).orUnassign)

    inline override protected def offsetsControl(props: GmosNorthMos, disabled: Boolean)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): VdomNode =
      // A MOS mask has no single slit to nod along, so the positions are a plain list
      // and the only preset is the default.
      IfuTelescopeConfigsEditor(
        telescopeConfigs = props.observingMode
          .zoom(
            ObservingMode.GmosNorthMos.explicitTelescopeConfigs,
            GmosNorthMosInput.explicitTelescopeConfigs.modify
          )
          .view(_.map(_.toList.map(_.toInput)).orUnassign)
          .removeOptionality(
            ObservingMode.GmosNorthMos.defaultTelescopeConfigs.get(props.observingMode.get)
          ),
        presets = NonEmptyList.one(
          "Default" -> ObservingMode.GmosNorthMos.defaultTelescopeConfigs
            .get(props.observingMode.get)
        ),
        defaultConfigs =
          ObservingMode.GmosNorthMos.defaultTelescopeConfigs.get(props.observingMode.get),
        helpId = "configuration/offsets.md".refined,
        presetsReadonly = disabled,
        editingReadonly = disabled
      )

    inline protected def exposureTimeMode(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[ExposureTimeMode] = aligner
      .zoom(
        ObservingMode.GmosNorthMos.exposureTimeMode,
        GmosNorthMosInput.exposureTimeMode.modify
      )
      .view(_.toInput.assign)

    override protected def acquisitionSection(
      props:    GmosSpectroscopyConfigPanel.GmosNorthMos,
      disabled: Boolean
    )(using MonadError[IO, Throwable], Effect.Dispatch[IO], Logger[IO]): VdomNode =
      val defaultAcquisitionFilter = defaultAcquisitionFilterLens.get(props.observingMode.get)
      mosAcquisitionPanel(
        props,
        disabled,
        acquisitionType(props.observingMode),
        explicitAcquisitionFilter(props.observingMode).withDefault(defaultAcquisitionFilter),
        defaultAcquisitionFilter,
        excludedAcquisitionFilters,
        acquisitionExposureTimeModeView(props.observingMode)
      )

    override protected val initialGratingLens           = ObservingMode.GmosNorthMos.initialGrating
    override protected val initialFilterLens            = ObservingMode.GmosNorthMos.initialFilter
    override protected val initialFpuLens               = ObservingMode.GmosNorthMos.initialSlitWidth
    override protected val initialCentralWavelengthLens =
      ObservingMode.GmosNorthMos.initialCentralWavelength.andThen(CentralWavelength.Value)
    override protected val defaultXBinningLens          = ObservingMode.GmosNorthMos.defaultXBin
    override protected val defaultYBinningLens          = ObservingMode.GmosNorthMos.defaultYBin
    override protected val defaultReadModeGainLens      =
      (ObservingMode.GmosNorthMos.defaultAmpReadMode,
       ObservingMode.GmosNorthMos.defaultAmpGain
      ).disjointZip
    override protected val defaultRoiLens               = ObservingMode.GmosNorthMos.defaultRoi
    override protected val defaultWavelengthDithersLens =
      ObservingMode.GmosNorthMos.defaultWavelengthDithers

    inline override protected def resolvedReadModeGainGetter = mode =>
      val readMode = ObservingMode.GmosNorthMos.explicitAmpReadMode
        .get(mode)
        .getOrElse(ObservingMode.GmosNorthMos.defaultAmpReadMode.get(mode))
      val ampGain  = ObservingMode.GmosNorthMos.explicitAmpGain
        .get(mode)
        .getOrElse(ObservingMode.GmosNorthMos.defaultAmpGain.get(mode))
      (readMode, ampGain)
  }

  // Gmos South MOS
  case class GmosSouthMos(
    programId:       Program.Id,
    obsId:           Observation.Id,
    calibrationRole: Option[CalibrationRole],
    observingMode:   Aligner[ObservingMode.GmosSouthMos, GmosSouthMosInput],
    revertConfig:    IO[Unit],
    confMatrix:      SpectroscopyModesMatrix,
    sequenceChanged: Callback,
    permissions:     ConfigEditPermissions,
    units:           WavelengthUnits,
    maskContext:     MosMaskContext
  ) extends ReactFnProps[GmosSpectroscopyConfigPanel.GmosSouthMos](
        GmosSpectroscopyConfigPanel.GmosSouthMos.component
      )
      with GmosMosConfigPanel[
        ObservingMode.GmosSouthMos,
        GmosSouthMosInput
      ]

  object GmosSouthMos
      extends GmosMosConfigPanelBuilder[
        ObservingMode.GmosSouthMos,
        GmosSouthMosInput,
        GmosSpectroscopyConfigPanel.GmosSouthMos,
        GmosSouthGrating,
        GmosSouthFilter
      ] {

    override protected val maskInstrument: Instrument = Instrument.GmosSouth

    inline override protected def revertCustomizations(
      aligner: AA
    )(using MonadError[IO, Throwable], Effect.Dispatch[IO], Logger[IO]): Callback =
      aligner.view(_.toInput).mod(_.revertCustomizations)

    inline override protected def centralWavelength(
      aligner: AA
    )(using MonadError[IO, Throwable], Effect.Dispatch[IO], Logger[IO]): View[Wavelength] =
      aligner
        .zoom(
          ObservingMode.GmosSouthMos.centralWavelength.andThen(CentralWavelength.Value),
          GmosSouthMosInput.centralWavelength.modify
        )
        .view(_.toInput.assign)

    inline override protected def grating(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[GmosSouthGrating] =
      aligner
        .zoom(
          ObservingMode.GmosSouthMos.grating,
          GmosSouthMosInput.grating.modify
        )
        .view(_.assign)

    inline override protected def filter(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosSouthFilter]] = aligner
      .zoom(
        ObservingMode.GmosSouthMos.filter,
        GmosSouthMosInput.filter.modify
      )
      .view(_.orUnassign)

    // See the note on the North equivalent.
    inline private def customMask(
      aligner: AA
    ): Aligner[ObservingMode.GmosCustomMask, GmosCustomMaskInput] =
      aligner
        .zoom(
          ObservingMode.GmosSouthMos.customMask,
          forceAssign(GmosSouthMosInput.customMask.modify)(aligner.get.customMask.toInput)
        )

    inline override protected def fpu(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[GmosCustomSlitWidth] = customMask(aligner)
      .zoom(
        ObservingMode.GmosCustomMask.slitWidth,
        GmosCustomMaskInput.slitWidth.modify
      )
      .view(identity)

    override protected def customMaskAttachmentId(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[Attachment.Id]] = customMask(aligner)
      .zoom(
        ObservingMode.GmosCustomMask.attachmentId,
        GmosCustomMaskInput.attachmentId.modify
      )
      .view(_.orUnassign)

    inline private def acquisitionType(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[GmosMosAcquisitionType] = aligner
      .zoom(
        ObservingMode.GmosSouthMos.acquisitionType,
        GmosSouthMosInput.acquisitionType.modify
      )
      .view(_.assign)

    inline private def acquisition(
      aligner: AA
    ): Aligner[ObservingMode.GmosSouthMos.Acquisition, GmosSouthMosAcquisitionInput] =
      aligner
        .zoom(
          ObservingMode.GmosSouthMos.acquisition,
          forceAssign(GmosSouthMosInput.acquisition.modify)(
            GmosSouthMosAcquisitionInput()
          )
        )

    inline private def explicitAcquisitionFilter(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosSouthFilter]] = acquisition(aligner)
      .zoom(ObservingMode.GmosSouthMos.Acquisition.explicitFilter,
            GmosSouthMosAcquisitionInput.explicitFilter.modify
      )
      .view(_.orUnassign)

    inline private def acquisitionExposureTimeModeView(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[ExposureTimeMode] = acquisition(aligner)
      .zoom(ObservingMode.GmosSouthMos.Acquisition.exposureTimeMode,
            GmosSouthMosAcquisitionInput.exposureTimeMode.modify
      )
      .view(_.toInput.assign)

    private val excludedAcquisitionFilters: Set[GmosSouthFilter] =
      Enumerated[GmosSouthFilter].all.toSet -- GmosSouthFilter.acquisition.toList.toSet

    private val defaultAcquisitionFilterLens: Lens[ObservingMode.GmosSouthMos, GmosSouthFilter] =
      ObservingMode.GmosSouthMos.acquisition.andThen(
        ObservingMode.GmosSouthMos.Acquisition.defaultFilter
      )

    inline override protected def explicitXBinning(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosXBinning]] = aligner
      .zoom(
        ObservingMode.GmosSouthMos.explicitXBin,
        GmosSouthMosInput.explicitXBin.modify
      )
      .view(_.map(_.value).orUnassign)

    inline override protected def explicitYBinning(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosYBinning]] = aligner
      .zoom(
        ObservingMode.GmosSouthMos.explicitYBin,
        GmosSouthMosInput.explicitYBin.modify
      )
      .view(_.map(_.value).orUnassign)

    private def readGainAligner(
      aligner: AA
    ): Aligner[Option[(GmosAmpReadMode, GmosAmpGain)], GmosSouthMosInput] =
      aligner
        .zoom(
          unsafeDisjointOptionZip(ObservingMode.GmosSouthMos.explicitAmpReadMode,
                                  ObservingMode.GmosSouthMos.explicitAmpGain
          ),
          f => i => f(i)
        )

    inline override protected def explicitReadModeGain(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[(GmosAmpReadMode, GmosAmpGain)]] =
      readGainAligner(aligner)
        .viewMod { org =>
          val rg = org.unzip
          GmosSouthMosInput.explicitAmpReadMode
            .replace(rg._1.orUnassign)
            .andThen(GmosSouthMosInput.explicitAmpGain.replace(rg._2.orUnassign))
        }

    inline override protected def explicitRoi(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosRoi]] = aligner
      .zoom(
        ObservingMode.GmosSouthMos.explicitRoi,
        GmosSouthMosInput.explicitRoi.modify
      )
      .view(_.orUnassign)

    inline override protected def explicitWavelengthDithers(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[NonEmptyList[WavelengthDither]]] = aligner
      .zoom(
        ObservingMode.GmosSouthMos.explicitWavelengthDithers,
        GmosSouthMosInput.explicitWavelengthDithers.modify
      )
      .view(_.map(_.map(_.toInput).toList).orUnassign)

    inline override protected def offsetsControl(props: GmosSouthMos, disabled: Boolean)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): VdomNode =
      // A MOS mask has no single slit to nod along, so the positions are a plain list
      // and the only preset is the default.
      IfuTelescopeConfigsEditor(
        telescopeConfigs = props.observingMode
          .zoom(
            ObservingMode.GmosSouthMos.explicitTelescopeConfigs,
            GmosSouthMosInput.explicitTelescopeConfigs.modify
          )
          .view(_.map(_.toList.map(_.toInput)).orUnassign)
          .removeOptionality(
            ObservingMode.GmosSouthMos.defaultTelescopeConfigs.get(props.observingMode.get)
          ),
        presets = NonEmptyList.one(
          "Default" -> ObservingMode.GmosSouthMos.defaultTelescopeConfigs
            .get(props.observingMode.get)
        ),
        defaultConfigs =
          ObservingMode.GmosSouthMos.defaultTelescopeConfigs.get(props.observingMode.get),
        helpId = "configuration/offsets.md".refined,
        presetsReadonly = disabled,
        editingReadonly = disabled
      )

    inline protected def exposureTimeMode(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[ExposureTimeMode] = aligner
      .zoom(
        ObservingMode.GmosSouthMos.exposureTimeMode,
        GmosSouthMosInput.exposureTimeMode.modify
      )
      .view(_.toInput.assign)

    override protected def acquisitionSection(
      props:    GmosSpectroscopyConfigPanel.GmosSouthMos,
      disabled: Boolean
    )(using MonadError[IO, Throwable], Effect.Dispatch[IO], Logger[IO]): VdomNode =
      val defaultAcquisitionFilter = defaultAcquisitionFilterLens.get(props.observingMode.get)
      mosAcquisitionPanel(
        props,
        disabled,
        acquisitionType(props.observingMode),
        explicitAcquisitionFilter(props.observingMode).withDefault(defaultAcquisitionFilter),
        defaultAcquisitionFilter,
        excludedAcquisitionFilters,
        acquisitionExposureTimeModeView(props.observingMode)
      )

    override protected val initialGratingLens           = ObservingMode.GmosSouthMos.initialGrating
    override protected val initialFilterLens            = ObservingMode.GmosSouthMos.initialFilter
    override protected val initialFpuLens               = ObservingMode.GmosSouthMos.initialSlitWidth
    override protected val initialCentralWavelengthLens =
      ObservingMode.GmosSouthMos.initialCentralWavelength.andThen(CentralWavelength.Value)
    override protected val defaultXBinningLens          = ObservingMode.GmosSouthMos.defaultXBin
    override protected val defaultYBinningLens          = ObservingMode.GmosSouthMos.defaultYBin
    override protected val defaultReadModeGainLens      =
      (ObservingMode.GmosSouthMos.defaultAmpReadMode,
       ObservingMode.GmosSouthMos.defaultAmpGain
      ).disjointZip
    override protected val defaultRoiLens               = ObservingMode.GmosSouthMos.defaultRoi
    override protected val defaultWavelengthDithersLens =
      ObservingMode.GmosSouthMos.defaultWavelengthDithers

    inline override protected def resolvedReadModeGainGetter = mode =>
      val readMode = ObservingMode.GmosSouthMos.explicitAmpReadMode
        .get(mode)
        .getOrElse(ObservingMode.GmosSouthMos.defaultAmpReadMode.get(mode))
      val ampGain  = ObservingMode.GmosSouthMos.explicitAmpGain
        .get(mode)
        .getOrElse(ObservingMode.GmosSouthMos.defaultAmpGain.get(mode))
      (readMode, ampGain)
  }
}

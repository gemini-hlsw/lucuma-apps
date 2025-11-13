// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.Eq
import cats.MonadError
import cats.Order.given
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import clue.data.syntax.*
import crystal.react.View
import crystal.react.hooks.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.Aligner
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.config.offsets.OffsetEditor
import explore.model.AppContext
import explore.model.Observation
import explore.model.display.given
import explore.model.enums.WavelengthUnits
import explore.model.reusability.given
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.util.Effect
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.math.Offset
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.math.syntax.int.*
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Program
import lucuma.core.optics.syntax.lens.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.util.NewBoolean
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.Dialog
import lucuma.react.primereact.Panel
import lucuma.react.primereact.tooltip.*
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import lucuma.ui.optics.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.FormLabel
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.*
import lucuma.ui.utils.given
import monocle.Lens
import org.typelevel.log4cats.Logger

object GmosImagingConfigPanel {
  // These displays are allowing the display of the short name in the chips of the
  // filter multi-select, but long names in the dropdown list. If we decide to only
  // use the long name everywhere, remove these givens and the itemTemplate in the multi-select.
  given Display[GmosNorthFilter] = Display.by(_.shortName, _.longName)
  given Display[GmosSouthFilter] = Display.by(_.shortName, _.longName)

  private object OffsetDialogOpen extends NewBoolean

  sealed trait GmosImagingConfigPanel[T <: ObservingMode, Input]:
    def programId: Program.Id
    def obsId: Observation.Id
    def calibrationRole: Option[CalibrationRole]
    def observingMode: Aligner[T, Input]
    def requirementsExposureTimeMode: Option[ExposureTimeMode]
    def revertConfig: Callback
    def sequenceChanged: Callback
    def readonly: Boolean
    def units: WavelengthUnits
    def instrument = observingMode.get.instrument

  sealed abstract class GmosImagingConfigPanelBuilder[
    T <: ObservingMode,
    Input,
    Props <: GmosImagingConfigPanel[T, Input],
    ImagingFilter: Reusability: Eq,
    Filter: Enumerated: Display
  ] {
    protected type AA = Aligner[T, Input]

    inline protected def isCustomized(aligner: AA): Boolean = aligner.get.isCustomized

    protected def revertCustomizations(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): Callback

    protected def filters(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[NonEmptyList[ImagingFilter]]

    protected def offsets(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[List[Offset]]

    protected def explicitMultipleFiltersMode(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[MultipleFiltersMode]]

    protected def explicitBinning(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosBinning]]

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

    protected val filtersLens: Lens[T, NonEmptyList[ImagingFilter]]
    protected val filtersFilterLens: Lens[ImagingFilter, Filter]
    protected val filtersEtmLens: Lens[ImagingFilter, ExposureTimeMode]
    protected val offsetLens: Lens[T, List[Offset]]
    protected val initialFiltersLens: Lens[T, NonEmptyList[ImagingFilter]]
    protected val filterTypeGetter: Filter => FilterType
    protected val defaultMultipleFiltersModeLens: Lens[T, MultipleFiltersMode]
    protected val defaultBinningLens: Lens[T, GmosBinning]
    protected val defaultReadModeGainLens: Lens[T, (GmosAmpReadMode, GmosAmpGain)]
    protected val defaultRoiLens: Lens[T, GmosRoi]

    protected def resolvedReadModeGainGetter: T => (GmosAmpReadMode, GmosAmpGain)
    protected def makeImagingFilter(filter: Filter, etm: ExposureTimeMode): ImagingFilter

    protected given Display[(GmosAmpReadMode, GmosAmpGain)] =
      Display.by( // Shortname is in lower case for some reason
        { case (r, g) => s"${r.longName}, ${g.shortName} Gain" },
        { case (r, g) => s"${r.longName}, ${g.longName} Gain" }
      )

    private val OffsetRadius = 30.arcseconds

    val component =
      ScalaFnComponent[Props]: props =>
        for {
          ctx                 <- useContext(AppContext.ctx)
          editState           <- useStateView(ConfigEditState.View)
          unModdedFiltersView <- useStateView(List.empty[ImagingFilter])
          offsetDialogOpen    <- useStateView(OffsetDialogOpen(false))
          localOffsets        <- useStateView {
                                   import ctx.given
                                   offsets(props.observingMode).get
                                 }
          _                   <- useEffectWithDeps(offsetLens.get(props.observingMode.get))(localOffsets.set)
          _                   <-
            useEffectWithDeps(filtersLens.get(props.observingMode.get).toList)(
              unModdedFiltersView.set
            )
          excludeFilters      <-
            // exclude already used filters and spectroscopic filters from the dropdowns
            useMemo(filtersLens.get(props.observingMode.get))(
              _.toList.map(filtersFilterLens.get).toSet ++
                Enumerated[Filter].all
                  .filter(f => filterTypeGetter(f) === FilterType.Spectroscopic)
                  .toSet
            )
          newFilterView       <- useStateView(Enumerated[Filter].all.headOption)
          _                   <- useEffectWithDeps(excludeFilters.value): efs =>
                                   if newFilterView.get.forall(efs.contains) then
                                     newFilterView.set(Enumerated[Filter].all.filterNot(efs.contains).headOption)
                                   else Callback.empty
        } yield
          import ctx.given

          val disableAdvancedEdit      = editState.get =!= ConfigEditState.AdvancedEdit || props.readonly
          val disableSimpleEdit        =
            disableAdvancedEdit && editState.get =!= ConfigEditState.SimpleEdit
          val showCustomization        = props.calibrationRole.isEmpty
          val allowRevertCustomization = !props.readonly

          val defaultMultipleFiltersMode =
            defaultMultipleFiltersModeLens.get(props.observingMode.get)
          val defaultBinning             = defaultBinningLens.get(props.observingMode.get)
          val defaultReadModeGain        = defaultReadModeGainLens.get(props.observingMode.get)
          val defaultRoi                 = defaultRoiLens.get(props.observingMode.get)
          val resolvedReadModeGain       = resolvedReadModeGainGetter(props.observingMode.get)

          val filtersView = filters(props.observingMode)

          val localFiltersView = unModdedFiltersView.withOnMod(l =>
            NonEmptyList
              .fromList(l.sortBy(filtersFilterLens.get))
              .fold(Callback.empty)(filtersView.set)
          )

          val initialFilters = initialFiltersLens.get(props.observingMode.get)
          val offsetReadOnly = props.readonly || editState.get === ConfigEditState.View
          val offsetsCount   = offsets(props.observingMode).get.size
          val offsetsText    =
            if (offsetsCount == 0) "No offsets"
            else if (offsetsCount == 1) "1 offset"
            else s"$offsetsCount offsets"

          React.Fragment(
            <.div(
              ExploreStyles.GmosImagingUpperGrid
            )(
              <.div(LucumaPrimeStyles.FormColumnCompact)(
                CustomizableEnumSelectOptional(
                  id = "explicitMultipleFiltersMode".refined,
                  view = explicitMultipleFiltersMode(props.observingMode)
                    .withDefault(defaultMultipleFiltersMode),
                  defaultValue = defaultMultipleFiltersMode.some,
                  label = "Multiple Filters".some,
                  helpId = Some("configuration/imaging/multiple-filters-mode.md".refined),
                  disabled = disableSimpleEdit,
                  showCustomization = showCustomization,
                  allowRevertCustomization = allowRevertCustomization
                )
              ),
              <.div(LucumaPrimeStyles.FormColumnCompact)(
                FormLabel(htmlFor = "spatial-offsets-button".refined)("Offsets"),
                <.div(
                  ExploreStyles.FlexContainer,
                  Button(
                    icon = if (disableSimpleEdit) Icons.Eye else Icons.Edit,
                    text = true,
                    severity =
                      if (disableSimpleEdit) Button.Severity.Info else Button.Severity.Secondary,
                    clazz = ExploreStyles.OffsetEditorButton,
                    onClickE = _ => offsetDialogOpen.set(OffsetDialogOpen(true))
                  ).mini.compact
                    .withMods(^.id          := "spatial-offsets-button",
                              ^.title := (if (disableSimpleEdit) "View Offsets"
                                          else "Edit Offsets")
                    ),
                  <.span(
                    ExploreStyles.OffsetsCount,
                    s"($offsetsText)"
                  )
                )
              ),
              <.div(LucumaPrimeStyles.FormColumnCompact)(
                CustomizableEnumSelectOptional(
                  id = "explicitBin".refined,
                  view = explicitBinning(props.observingMode).withDefault(defaultBinning),
                  defaultValue = defaultBinning.some,
                  label = "Binning".some,
                  helpId = Some("configuration/gmos/binning.md".refined),
                  disabled = disableAdvancedEdit,
                  dropdownMods = ^.aria.label := "Binning",
                  showCustomization = showCustomization,
                  allowRevertCustomization = allowRevertCustomization
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
                )
              )
            ),
            <.div(
              ExploreStyles.GmosImagingLowerGrid,
              Panel(
                header = <.span(
                  "Filters",
                  HelpIcon("configuration/gmos/imaging-filters.md".refined),
                  CustomizedGroupAddon(
                    "original",
                    filtersView.set(initialFilters),
                    allowRevertCustomization
                  ).unless(initialFilters === filtersView.get),
                  <.span(
                    Icons.ErrorIcon
                  ).withTooltip(
                    content = "At least one filter is required."
                  ).when(localFiltersView.get.length === 0)
                ),
                toggleable = true,
                collapsed = false
              )(
                <.div(ExploreStyles.GmosImagingFilterGrid)(
                  <.span(), // the action button
                  <.span("Filter", ExploreStyles.GmosImagingFilterGridHeader),
                  <.span("Exposure Mode", ExploreStyles.GmosImagingFilterGridHeader),
                  <.span("Signal/Noise", ExploreStyles.GmosImagingFilterGridHeader),
                  <.span("Exp. Time", ExploreStyles.GmosImagingFilterGridHeader),
                  <.span("Number of Exp.", ExploreStyles.GmosImagingFilterGridHeader),
                  localFiltersView.toListOfViews.zipWithIndex
                    .toReactFragment(using
                      (imagingFilterView, idx) =>
                        val filter = filtersFilterLens.get(imagingFilterView.get)
                        React.Fragment(
                          Button(
                            icon = Icons.Trash,
                            clazz = ExploreStyles.GmosImagingFilterGridAction,
                            text = true,
                            disabled = disableSimpleEdit,
                            onClick = localFiltersView.mod(
                              _.filterNot(imf => filtersFilterLens.get(imf) === filter)
                            )
                          ).tiny.compact,
                          <.span(
                            ExploreStyles.GmosImagingFilter,
                            FormEnumDropdownView(
                              id = NonEmptyString.unsafeFrom(s"imgFilterFilter$idx"),
                              value = imagingFilterView.zoom(filtersFilterLens),
                              label = "Filter",
                              clazz = ExploreStyles.GmosImagingFilter,
                              labelClass = ExploreStyles.HiddenLabel,
                              disabled = disableSimpleEdit,
                              exclude = excludeFilters - filter
                            )
                          ),
                          ExposureTimeModeEditor(
                            props.instrument.some,
                            none,
                            imagingFilterView.zoom(filtersEtmLens),
                            ScienceMode.Imaging,
                            disableSimpleEdit,
                            props.units,
                            props.calibrationRole,
                            NonEmptyString.unsafeFrom(s"imgFilter$idx"),
                            forGridRow = true
                          )
                        )
                    ),
                  newFilterView.mapValue((fv: View[Filter]) =>
                    React.Fragment(
                      Button(
                        icon = Icons.ThinPlus,
                        severity = Button.Severity.Success,
                        clazz = ExploreStyles.GmosImagingFilterGridAction,
                        text = true,
                        disabled = disableSimpleEdit,
                        onClick = localFiltersView.mod(
                          _ :+ makeImagingFilter(
                            fv.get,
                            // there should always be one, but...
                            props.requirementsExposureTimeMode.getOrElse(
                              ExposureTimeMode.SignalToNoiseMode(SignalToNoise.Min, Wavelength.Min)
                            )
                          )
                        )
                      ).tiny.compact,
                      FormEnumDropdownView(
                        id = "imgFilterAdd".refined,
                        value = fv,
                        label = "Add Filter",
                        clazz = ExploreStyles.GmosImagingFilter,
                        labelClass = ExploreStyles.HiddenLabel,
                        disabled = disableSimpleEdit,
                        exclude = excludeFilters.value
                      )
                    )
                  )
                )
              ),
              AdvancedConfigButtons(
                editState = editState,
                isCustomized = isCustomized(props.observingMode),
                revertConfig = props.revertConfig,
                revertCustomizations = revertCustomizations(props.observingMode),
                sequenceChanged = props.sequenceChanged,
                readonly = props.readonly
              )
            ),
            Dialog(
              visible = OffsetDialogOpen.value(offsetDialogOpen.get),
              onHide = offsetDialogOpen.set(OffsetDialogOpen(false)),
              header = if (offsetReadOnly) "View Offsets" else "Offsets",
              modal = true,
              resizable = false,
              clazz = ExploreStyles.OffsetsEditorDialog,
              footer =
                if (offsetReadOnly)
                  <.div(
                    Button(
                      label = "Close",
                      severity = Button.Severity.Secondary,
                      onClick = offsetDialogOpen.set(OffsetDialogOpen(false))
                    ).small.compact
                  )
                else
                  <.div(
                    Button(
                      label = "Cancel",
                      severity = Button.Severity.Danger,
                      onClick = offsetDialogOpen.set(OffsetDialogOpen(false))
                    ).small.compact,
                    Button(
                      label = "Save",
                      severity = Button.Severity.Success,
                      onClick = {
                        import ctx.given
                        offsets(props.observingMode).set(localOffsets.get) >>
                          offsetDialogOpen.set(OffsetDialogOpen(false))
                      }
                    ).small.compact
                  )
            )(
              OffsetEditor(
                if (offsetReadOnly) offsets(props.observingMode) else localOffsets,
                offsets => localOffsets.set(offsets).unless_(offsetReadOnly),
                props.requirementsExposureTimeMode match {
                  case Some(ExposureTimeMode.TimeAndCountMode(_, c, _)) => c
                  case Some(ExposureTimeMode.SignalToNoiseMode(_, _))   => 1.refined // fixme
                  case _                                                => 1.refined
                },
                OffsetRadius,
                readOnly = offsetReadOnly
              )
            )
          )
  }

  // Gmos North Imaging
  case class GmosNorthImaging(
    programId:                    Program.Id,
    obsId:                        Observation.Id,
    calibrationRole:              Option[CalibrationRole],
    observingMode:                Aligner[ObservingMode.GmosNorthImaging, GmosNorthImagingInput],
    requirementsExposureTimeMode: Option[ExposureTimeMode],
    revertConfig:                 Callback,
    sequenceChanged:              Callback,
    readonly:                     Boolean,
    units:                        WavelengthUnits
  ) extends ReactFnProps[GmosImagingConfigPanel.GmosNorthImaging](
        GmosImagingConfigPanel.GmosNorthImaging.component
      )
      with GmosImagingConfigPanel[
        ObservingMode.GmosNorthImaging,
        GmosNorthImagingInput
      ]

  object GmosNorthImaging
      extends GmosImagingConfigPanelBuilder[
        ObservingMode.GmosNorthImaging,
        GmosNorthImagingInput,
        GmosImagingConfigPanel.GmosNorthImaging,
        ObservingMode.GmosNorthImaging.ImagingFilter,
        GmosNorthFilter
      ] {

    inline override protected def revertCustomizations(
      aligner: AA
    )(using MonadError[IO, Throwable], Effect.Dispatch[IO], Logger[IO]): Callback =
      aligner.view(_.toInput).mod(_.revertCustomizations)

    inline override protected def filters(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[NonEmptyList[ObservingMode.GmosNorthImaging.ImagingFilter]] = aligner
      .zoom(
        ObservingMode.GmosNorthImaging.filters,
        GmosNorthImagingInput.filters.modify
      )
      .view(_.toList.map(_.toInput).assign)

    override protected def offsets(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[List[Offset]] = aligner
      .zoom(
        ObservingMode.GmosNorthImaging.offsets,
        GmosNorthImagingInput.offsets.modify
      )
      .view(_.map(_.toInput).assign)

    inline override protected def explicitMultipleFiltersMode(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[MultipleFiltersMode]] = aligner
      .zoom(
        ObservingMode.GmosNorthImaging.explicitMultipleFiltersMode,
        GmosNorthImagingInput.explicitMultipleFiltersMode.modify
      )
      .view(_.orUnassign)

    inline override protected def explicitBinning(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosBinning]] = aligner
      .zoom(
        ObservingMode.GmosNorthImaging.explicitBin,
        GmosNorthImagingInput.explicitBin.modify
      )
      .view(_.orUnassign)

    private val explicitReadMode =
      ObservingMode.GmosNorthImaging.explicitAmpReadMode

    private val explicitGain =
      ObservingMode.GmosNorthImaging.explicitAmpGain

    private def readGainAligner(
      aligner: AA
    ): Aligner[Option[(GmosAmpReadMode, GmosAmpGain)], GmosNorthImagingInput] =
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
          GmosNorthImagingInput.explicitAmpReadMode
            .replace(rg._1.orUnassign)
            .andThen(GmosNorthImagingInput.explicitAmpGain.replace(rg._2.orUnassign))
        }

    inline override protected def explicitRoi(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosRoi]] = aligner
      .zoom(
        ObservingMode.GmosNorthImaging.explicitRoi,
        GmosNorthImagingInput.explicitRoi.modify
      )
      .view(_.orUnassign)

    override protected val filtersLens                           = ObservingMode.GmosNorthImaging.filters
    override protected val filtersFilterLens                     = ObservingMode.GmosNorthImaging.ImagingFilter.filter
    override protected val filtersEtmLens                        =
      ObservingMode.GmosNorthImaging.ImagingFilter.exposureTimeMode
    override protected val offsetLens                            = ObservingMode.GmosNorthImaging.offsets
    override protected val initialFiltersLens                    =
      ObservingMode.GmosNorthImaging.initialFilters
    override val filterTypeGetter: GmosNorthFilter => FilterType = _.filterType
    protected val defaultMultipleFiltersModeLens                 =
      ObservingMode.GmosNorthImaging.defaultMultipleFiltersMode
    protected val defaultBinningLens                             = ObservingMode.GmosNorthImaging.defaultBin
    protected val defaultReadModeGainLens                        =
      (ObservingMode.GmosNorthImaging.defaultAmpReadMode,
       ObservingMode.GmosNorthImaging.defaultAmpGain
      ).disjointZip
    protected val defaultRoiLens                                 = ObservingMode.GmosNorthImaging.defaultRoi

    inline override protected def resolvedReadModeGainGetter = mode =>
      val readMode = ObservingMode.GmosNorthImaging.explicitAmpReadMode
        .get(mode)
        .getOrElse(ObservingMode.GmosNorthImaging.defaultAmpReadMode.get(mode))
      val ampGain  = ObservingMode.GmosNorthImaging.explicitAmpGain
        .get(mode)
        .getOrElse(ObservingMode.GmosNorthImaging.defaultAmpGain.get(mode))
      (readMode, ampGain)

    inline override protected def makeImagingFilter(
      filter: GmosNorthFilter,
      etm:    ExposureTimeMode
    ) = ObservingMode.GmosNorthImaging.ImagingFilter(filter, etm)
  }

  // Gmos South Imaging
  case class GmosSouthImaging(
    programId:                    Program.Id,
    obsId:                        Observation.Id,
    calibrationRole:              Option[CalibrationRole],
    observingMode:                Aligner[ObservingMode.GmosSouthImaging, GmosSouthImagingInput],
    requirementsExposureTimeMode: Option[ExposureTimeMode],
    revertConfig:                 Callback,
    sequenceChanged:              Callback,
    readonly:                     Boolean,
    units:                        WavelengthUnits
  ) extends ReactFnProps[GmosImagingConfigPanel.GmosSouthImaging](
        GmosImagingConfigPanel.GmosSouthImaging.component
      )
      with GmosImagingConfigPanel[
        ObservingMode.GmosSouthImaging,
        GmosSouthImagingInput
      ]

  object GmosSouthImaging
      extends GmosImagingConfigPanelBuilder[
        ObservingMode.GmosSouthImaging,
        GmosSouthImagingInput,
        GmosImagingConfigPanel.GmosSouthImaging,
        ObservingMode.GmosSouthImaging.ImagingFilter,
        GmosSouthFilter
      ] {

    inline override protected def revertCustomizations(
      aligner: AA
    )(using MonadError[IO, Throwable], Effect.Dispatch[IO], Logger[IO]): Callback =
      aligner.view(_.toInput).mod(_.revertCustomizations)

    inline override protected def filters(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[NonEmptyList[ObservingMode.GmosSouthImaging.ImagingFilter]] =
      aligner
        .zoom(
          ObservingMode.GmosSouthImaging.filters,
          GmosSouthImagingInput.filters.modify
        )
        .view(_.toList.map(_.toInput).assign)

    inline override protected def offsets(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[List[Offset]] = aligner
      .zoom(
        ObservingMode.GmosSouthImaging.offsets,
        GmosSouthImagingInput.offsets.modify
      )
      .view(_.map(_.toInput).assign)

    inline override protected def explicitMultipleFiltersMode(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[MultipleFiltersMode]] = aligner
      .zoom(
        ObservingMode.GmosSouthImaging.explicitMultipleFiltersMode,
        GmosSouthImagingInput.explicitMultipleFiltersMode.modify
      )
      .view(_.orUnassign)

    inline override protected def explicitBinning(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosBinning]] = aligner
      .zoom(
        ObservingMode.GmosSouthImaging.explicitBin,
        GmosSouthImagingInput.explicitBin.modify
      )
      .view(_.orUnassign)

    private val explicitReadMode =
      ObservingMode.GmosSouthImaging.explicitAmpReadMode

    private val explicitGain =
      ObservingMode.GmosSouthImaging.explicitAmpGain

    private def readGainAligner(
      aligner: AA
    ): Aligner[Option[(GmosAmpReadMode, GmosAmpGain)], GmosSouthImagingInput] =
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
          GmosSouthImagingInput.explicitAmpReadMode
            .replace(rg._1.orUnassign)
            .andThen(GmosSouthImagingInput.explicitAmpGain.replace(rg._2.orUnassign))
        }

    inline override protected def explicitRoi(aligner: AA)(using
      MonadError[IO, Throwable],
      Effect.Dispatch[IO],
      Logger[IO]
    ): View[Option[GmosRoi]] = aligner
      .zoom(
        ObservingMode.GmosSouthImaging.explicitRoi,
        GmosSouthImagingInput.explicitRoi.modify
      )
      .view(_.orUnassign)

    override protected val filtersLens           = ObservingMode.GmosSouthImaging.filters
    override protected val filtersFilterLens     = ObservingMode.GmosSouthImaging.ImagingFilter.filter
    override protected val filtersEtmLens        =
      ObservingMode.GmosSouthImaging.ImagingFilter.exposureTimeMode
    override protected val offsetLens            = ObservingMode.GmosSouthImaging.offsets
    override protected val initialFiltersLens    =
      ObservingMode.GmosSouthImaging.initialFilters
    override protected val filterTypeGetter      = _.filterType
    protected val defaultMultipleFiltersModeLens =
      ObservingMode.GmosSouthImaging.defaultMultipleFiltersMode
    protected val defaultBinningLens             = ObservingMode.GmosSouthImaging.defaultBin
    protected val defaultReadModeGainLens        =
      (ObservingMode.GmosSouthImaging.defaultAmpReadMode,
       ObservingMode.GmosSouthImaging.defaultAmpGain
      ).disjointZip
    protected val defaultRoiLens                 = ObservingMode.GmosSouthImaging.defaultRoi

    inline override protected def resolvedReadModeGainGetter = mode =>
      val readMode = ObservingMode.GmosSouthImaging.explicitAmpReadMode
        .get(mode)
        .getOrElse(ObservingMode.GmosSouthImaging.defaultAmpReadMode.get(mode))
      val ampGain  = ObservingMode.GmosSouthImaging.explicitAmpGain
        .get(mode)
        .getOrElse(ObservingMode.GmosSouthImaging.defaultAmpGain.get(mode))
      (readMode, ampGain)

    inline override protected def makeImagingFilter(
      filter: GmosSouthFilter,
      etm:    ExposureTimeMode
    ) = ObservingMode.GmosSouthImaging.ImagingFilter(filter, etm)
  }
}

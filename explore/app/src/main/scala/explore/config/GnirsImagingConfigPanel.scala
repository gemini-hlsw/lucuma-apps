// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import clue.data.*
import clue.data.syntax.*
import crystal.react.View
import crystal.react.hooks.*
import eu.timepit.refined.types.numeric.PosInt
import explore.common.Aligner
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.config.offsets.OffsetInput
import explore.model.AppContext
import explore.model.Observation
import explore.model.enums.WavelengthUnits
import explore.model.reusability.given
import explore.model.syntax.all.*
import explore.syntax.ui.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.math.Offset
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Program
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMode
import lucuma.core.optics.syntax.lens.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Panel
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.ImagingVariant
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given

case class GnirsImagingConfigPanel(
  programId:                    Program.Id,
  obsId:                        Observation.Id,
  calibrationRole:              Option[CalibrationRole],
  observingMode:                Aligner[ObservingMode.GnirsImaging, GnirsImagingInput],
  requirementsExposureTimeMode: Option[ExposureTimeMode],
  revertConfig:                 IO[Unit],
  sequenceChanged:              Callback,
  permissions:                  ConfigEditPermissions,
  units:                        WavelengthUnits,
  isStaff:                      Boolean
) extends ReactFnProps(GnirsImagingConfigPanel):
  val mode: ObservingMode.GnirsImaging = observingMode.get
  // GNIRS imaging filter set (matches the phase-0 imaging config options): the small
  // MK filters Y/J/K plus the full-size X and H order-blockers.
  val allowedFilters: Set[GnirsFilter] =
    Set(GnirsFilter.Y, GnirsFilter.Order6, GnirsFilter.J, GnirsFilter.Order4, GnirsFilter.K)

object GnirsImagingConfigPanel
    extends ReactFnComponent[GnirsImagingConfigPanel](props =>
      for
        ctx       <- useContext(AppContext.ctx)
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

        given Enumerated[Option[GnirsReadMode]] =
          deriveOptionalEnumerated[GnirsReadMode]("Auto")
        given Display[Option[GnirsReadMode]]    =
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

        val variantView: View[ImagingVariant] =
          props.observingMode
            .zoom(ObservingMode.GnirsImaging.variant, GnirsImagingInput.variant.modify)
            .view(_.toInput.assign)

        val cameraView: View[GnirsCamera] =
          props.observingMode
            .zoom(ObservingMode.GnirsImaging.camera, GnirsImagingInput.camera.modify)
            .view(_.assign)

        val readModeView: View[Option[GnirsReadMode]] =
          props.observingMode
            .zoom(
              ObservingMode.GnirsImaging.explicitReadMode,
              GnirsImagingInput.explicitReadMode.modify
            )
            .view(_.orUnassign)

        val wellDepthView: View[Option[GnirsWellDepth]] =
          props.observingMode
            .zoom(
              ObservingMode.GnirsImaging.explicitWellDepth,
              GnirsImagingInput.explicitWellDepth.modify
            )
            .view(_.orUnassign)

        val filtersView: View[NonEmptyList[ObservingMode.GnirsImaging.ImagingFilter]] =
          props.observingMode
            .zoom(ObservingMode.GnirsImaging.filters, GnirsImagingInput.filters.modify)
            .view(_.toList.map(_.toInput).assign)

        val acquisition
          : Aligner[ObservingMode.GnirsImaging.Acquisition, GnirsImagingAcquisitionInput] =
          props.observingMode.zoom(
            ObservingMode.GnirsImaging.acquisition,
            forceAssign(GnirsImagingInput.acquisition.modify)(GnirsImagingAcquisitionInput())
          )

        // In our local model, we use GnirsAcquisitionMode, which maps to 2 fields in the API.
        val acquisitionModeView: View[Option[GnirsAcquisitionMode]] =
          acquisition
            .zoom(
              ObservingMode.GnirsImaging.Acquisition.explicitAcquisitionMode,
              GnirsImagingAcquisitionInput.explicitAcquisitionType
                .disjointZip(GnirsImagingAcquisitionInput.skyOffset)
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
                .map(newType =>
                  GnirsAcquisitionMode
                    .forTypeAndOffset(newType, GnirsAcquisitionMode.Faint.DefaultImagingSkyOffset)
                )
          )

        val acquisitionSkyOffsetViewOpt: Option[View[Offset]] =
          acquisitionModeView.toOptionView
            .flatMap(_.zoom(GnirsAcquisitionMode.skyOffset).toOptionView)

        val acquisitionCoaddsView: View[PosInt] =
          acquisition
            .zoom(
              ObservingMode.GnirsImaging.Acquisition.coadds,
              GnirsImagingAcquisitionInput.coadds.modify
            )
            .view(_.assign)

        val acquisitionFilterView: View[Option[GnirsFilter]] =
          acquisition
            .zoom(
              ObservingMode.GnirsImaging.Acquisition.explicitFilter,
              GnirsImagingAcquisitionInput.explicitFilter.modify
            )
            .view(_.orUnassign)

        val acquisitionExposureTimeView: View[ExposureTimeMode] =
          acquisition
            .zoom(
              ObservingMode.GnirsImaging.Acquisition.exposureTimeMode,
              GnirsImagingAcquisitionInput.exposureTimeMode.modify
            )
            .view(_.toInput.assign)

        React.Fragment(
          <.div(ExploreStyles.ImagingUpperGrid)(
            <.div(LucumaPrimeStyles.FormColumnCompact)(
              FormEnumDropdownView(
                id = "camera".refined,
                value = cameraView,
                label = React.Fragment("Camera", HelpIcon("configuration/gnirs/camera.md".refined)),
                disabled = disableSimpleEdit
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
                view = wellDepthView.withDefault(props.mode.defaultWellDepth),
                defaultValue = props.mode.defaultWellDepth.some,
                label = "Well Depth".some,
                helpId = Some("configuration/gnirs/well-depth.md".refined),
                disabled = disableSimpleEdit,
                showCustomization = showCustomization,
                allowRevertCustomization = allowRevertCustomization
              )
            ),
            <.div(LucumaPrimeStyles.FormColumnCompact)(
              ImagingVariantEditor(variantView, !props.permissions.isFullEdit)
            )
          ),
          <.div(ExploreStyles.ImagingLowerGrid)(
            <.div(ExploreStyles.ImagingLowerGridStack)(
              ImagingFiltersPanel(
                instrument = props.mode.instrument,
                filtersView = filtersView,
                filterLens = ObservingMode.GnirsImaging.ImagingFilter.filter,
                etmLens = ObservingMode.GnirsImaging.ImagingFilter.exposureTimeMode,
                // `Some`, not `.some`: Monocle's optic `.some` shadows the cats syntax here.
                coaddsLens = Some(ObservingMode.GnirsImaging.ImagingFilter.coadds),
                initialFilters = props.mode.initialFilters,
                allowedFilters = props.allowedFilters,
                // A new filter starts at 1 coadd: it inherits the observation's
                // exposure time mode, which is usually signal-to-noise, and that
                // doesn't support coadds anyway.
                makeImagingFilter =
                  (f, e) => ObservingMode.GnirsImaging.ImagingFilter(f, e, 1.refined),
                requirementsExposureTimeMode = props.requirementsExposureTimeMode,
                units = props.units,
                calibrationRole = props.calibrationRole,
                allowRevertCustomization = allowRevertCustomization,
                readonly = disableSimpleEdit,
                showCustomization = showCustomization
              ),
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
                      instrument = props.mode.instrument,
                      wavelength = none,
                      exposureTimeMode = acquisitionExposureTimeView,
                      coadds = acquisitionCoaddsView.some,
                      scienceMode = ScienceMode.Imaging,
                      readonly = props.permissions.isReadonly,
                      units = props.units,
                      calibrationRole = props.calibrationRole,
                      idPrefix = "gnirsImgAcq".refined,
                      forceCount = Some(1.refined)
                    )
                  )
                )
              ).when(showAcquisitionConfig)
            ),
            AdvancedConfigButtons(
              editState = editState,
              isCustomized = props.mode.isCustomized,
              revertConfig = props.revertConfig,
              revertCustomizations =
                props.observingMode.view(_.toInput).mod(_.revertCustomizations),
              sequenceChanged = props.sequenceChanged,
              readonly = !props.permissions.isFullEdit
            )
          )
        )
    )

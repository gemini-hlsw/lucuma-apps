// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.data.NonEmptyList
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
import explore.model.AppContext
import explore.model.Observation
import explore.model.enums.WavelengthUnits
import explore.model.reusability.given
import explore.syntax.ui.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Program
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.validation.InputValidSplitEpi
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.ImagingVariant
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import lucuma.ui.input.ChangeAuditor
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
  readonly:                     Boolean,
  units:                        WavelengthUnits,
  isStaff:                      Boolean
) extends ReactFnProps(GnirsImagingConfigPanel):
  val mode: ObservingMode.GnirsImaging = observingMode.get
  // Keyhole imaging filter set (matches the phase-0 imaging config options): the small
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
          editState.get =!= ConfigEditState.AdvancedEdit || props.readonly
        val disableSimpleEdit: Boolean        =
          disableAdvancedEdit && editState.get =!= ConfigEditState.SimpleEdit
        val showCustomization: Boolean        = props.calibrationRole.isEmpty
        val allowRevertCustomization: Boolean = !props.readonly

        given Enumerated[Option[GnirsReadMode]] =
          deriveOptionalEnumerated[GnirsReadMode]("Auto")
        given Display[Option[GnirsReadMode]]    =
          deriveOptionalDisplay[GnirsReadMode]("Auto")

        val variantView: View[ImagingVariant] =
          props.observingMode
            .zoom(ObservingMode.GnirsImaging.variant, GnirsImagingInput.variant.modify)
            .view(_.toInput.assign)

        val cameraView: View[GnirsCamera] =
          props.observingMode
            .zoom(ObservingMode.GnirsImaging.camera, GnirsImagingInput.camera.modify)
            .view(_.assign)

        val coaddsView: View[PosInt] =
          props.observingMode
            .zoom(ObservingMode.GnirsImaging.coadds, GnirsImagingInput.coadds.modify)
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
              ),
              FormInputTextView(
                id = "coadds".refined,
                value = coaddsView,
                label = "Coadds",
                validFormat = InputValidSplitEpi.posInt,
                changeAuditor = ChangeAuditor.int,
                disabled = disableSimpleEdit
              )(^.autoComplete.off)
            ),
            <.div(LucumaPrimeStyles.FormColumnCompact)(
              ImagingVariantEditor(variantView, props.readonly)
            )
          ),
          <.div(ExploreStyles.ImagingLowerGrid)(
            ImagingFiltersPanel(
              instrument = props.mode.instrument,
              filtersView = filtersView,
              filterLens = ObservingMode.GnirsImaging.ImagingFilter.filter,
              etmLens = ObservingMode.GnirsImaging.ImagingFilter.exposureTimeMode,
              initialFilters = props.mode.initialFilters,
              allowedFilters = props.allowedFilters,
              makeImagingFilter = (f, e) => ObservingMode.GnirsImaging.ImagingFilter(f, e),
              requirementsExposureTimeMode = props.requirementsExposureTimeMode,
              units = props.units,
              calibrationRole = props.calibrationRole,
              allowRevertCustomization = allowRevertCustomization,
              readonly = disableSimpleEdit,
              showCustomization = showCustomization
            ),
            AdvancedConfigButtons(
              editState = editState,
              isCustomized = props.mode.isCustomized,
              revertConfig = props.revertConfig,
              revertCustomizations =
                props.observingMode.view(_.toInput).mod(_.revertCustomizations),
              sequenceChanged = props.sequenceChanged,
              readonly = props.readonly
            )
          )
        )
    )

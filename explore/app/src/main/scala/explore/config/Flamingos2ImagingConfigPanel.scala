// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.data.NonEmptyList
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
import explore.model.reusability.given
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.util.Effect
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Program
import lucuma.core.util.Enumerated
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.ImagingVariant
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given

case class Flamingos2ImagingConfigPanel(
  programId:                    Program.Id,
  obsId:                        Observation.Id,
  calibrationRole:              Option[CalibrationRole],
  observingMode:                Aligner[ObservingMode.Flamingos2Imaging, Flamingos2ImagingInput],
  requirementsExposureTimeMode: Option[ExposureTimeMode],
  revertConfig:                 IO[Unit],
  sequenceChanged:              Callback,
  readonly:                     Boolean,
  units:                        WavelengthUnits,
  isStaff:                      Boolean
) extends ReactFnProps(Flamingos2ImagingConfigPanel):
  val mode: ObservingMode.Flamingos2Imaging = observingMode.get
  val allowedFilters: Set[Flamingos2Filter] = Enumerated[Flamingos2Filter].all.toSet

object Flamingos2ImagingConfigPanel
    extends ReactFnComponent[Flamingos2ImagingConfigPanel](props =>
      for
        ctx       <- useContext(AppContext.ctx)
        editState <- useStateView(ConfigEditState.View)
      yield
        import ctx.given
        import Flamingos2Givens.given

        val disableAdvancedEdit: Boolean      =
          editState.get =!= ConfigEditState.AdvancedEdit || props.readonly
        val disableSimpleEdit: Boolean        =
          disableAdvancedEdit && editState.get =!= ConfigEditState.SimpleEdit
        val showCustomization: Boolean        = props.calibrationRole.isEmpty
        val allowRevertCustomization: Boolean = !props.readonly

        val variantView: View[ImagingVariant] =
          props.observingMode
            .zoom(ObservingMode.Flamingos2Imaging.variant, Flamingos2ImagingInput.variant.modify)
            .view(_.toInput.assign)

        val readModeView: View[Option[Flamingos2ReadMode]] = props.observingMode
          .zoom(
            ObservingMode.Flamingos2Imaging.explicitReadMode,
            Flamingos2ImagingInput.explicitReadMode.modify
          )
          .view(_.orUnassign)

        val deckerView: View[Option[Flamingos2Decker]] = props.observingMode
          .zoom(
            ObservingMode.Flamingos2Imaging.explicitDecker,
            Flamingos2ImagingInput.explicitDecker.modify
          )
          .view(_.orUnassign)

        val filtersView: View[NonEmptyList[ObservingMode.Flamingos2Imaging.ImagingFilter]] =
          props.observingMode
            .zoom(ObservingMode.Flamingos2Imaging.filters, Flamingos2ImagingInput.filters.modify)
            .view(_.toList.map(_.toInput).assign)

        React.Fragment(
          <.div(ExploreStyles.ImagingUpperGrid)(
            <.div(LucumaPrimeStyles.FormColumnCompact)(
              FormLabel(htmlFor = "decker".refined)("Decker",
                                                    HelpIcon("configuration/f2/decker.md".refined)
              ),
              if (props.isStaff)
                CustomizableEnumSelectOptional(
                  id = "decker".refined,
                  view = deckerView.withDefault(props.mode.defaultDecker),
                  defaultValue = props.mode.defaultDecker.some,
                  disabled = disableAdvancedEdit,
                  showCustomization = showCustomization,
                  allowRevertCustomization = allowRevertCustomization
                )
              else
                <.label(^.id := "decker",
                        ExploreStyles.FormValue,
                        deckerView.get.getOrElse(props.mode.defaultDecker).shortName
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
              ImagingVariantEditor(variantView, props.readonly)
            )
          ),
          <.div(ExploreStyles.ImagingLowerGrid)(
            ImagingFiltersPanel(
              instrument = props.mode.instrument,
              filtersView = filtersView,
              filterLens = ObservingMode.Flamingos2Imaging.ImagingFilter.filter,
              etmLens = ObservingMode.Flamingos2Imaging.ImagingFilter.exposureTimeMode,
              initialFilters = props.mode.initialFilters,
              allowedFilters = props.allowedFilters,
              makeImagingFilter = (f, e) => ObservingMode.Flamingos2Imaging.ImagingFilter(f, e),
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

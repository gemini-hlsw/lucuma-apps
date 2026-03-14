// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.data.Nested
import cats.data.NonEmptyList
import cats.syntax.all.*
import clue.data.Input
import clue.data.syntax.*
import crystal.react.View
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.common.Aligner
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.config.offsets.OffsetInput
import explore.model.AppContext
import explore.model.Observation
import explore.model.display.given
import explore.model.enums.WavelengthUnits
import explore.modes.SpectroscopyModesMatrix
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.math.Offset
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Program
import lucuma.core.model.sequence.igrins2.defaultOffsetsFor
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.toNelOfViews

final case class Igrins2LongslitConfigPanel(
  programId:       Program.Id,
  obsId:           Observation.Id,
  calibrationRole: Option[CalibrationRole],
  observingMode:   Aligner[ObservingMode.Igrins2LongSlit, Igrins2LongSlitInput],
  revertConfig:    Callback,
  confMatrix:      SpectroscopyModesMatrix,
  sequenceChanged: Callback,
  permissions:     ConfigEditPermissions,
  units:           WavelengthUnits
) extends ReactFnProps(Igrins2LongslitConfigPanel)

object Igrins2LongslitConfigPanel
    extends ReactFnComponent[Igrins2LongslitConfigPanel](props =>
      for {
        ctx               <- useContext(AppContext.ctx)
        modeData          <- useModeData(props.confMatrix, props.observingMode.get)
        editState         <- useStateView(ConfigEditState.View)
        reverting         <- useStateView(false)
        offsets            = props.observingMode.get.explicitOffsets.getOrElse(
                               NonEmptyList.fromListUnsafe(
                                 defaultOffsetsFor(props.observingMode.get.offsetMode)
                               )
                             )
        localOffsetsState <- useStateView(offsets)
        _                 <- useEffectWithDeps(offsets):
                               localOffsetsState.set
      } yield
        import ctx.given

        val disableEdit              =
          editState.get =!= ConfigEditState.SimpleEdit && !props.permissions.isFullEdit || reverting.get
        val showCustomization        = props.calibrationRole.isEmpty
        val allowRevertCustomization = props.permissions.isFullEdit

        val defaultOffsetMode = props.observingMode.get.defaultOffsetMode

        val explicitOffsetsView: View[Option[NonEmptyList[Offset]]] = props.observingMode
          .zoom(
            ObservingMode.Igrins2LongSlit.explicitOffsets,
            Igrins2LongSlitInput.explicitOffsets.modify
          )
          .view(_.map(_.toList.map(_.toInput)).orUnassign)

        val offsetModeView: View[Option[Igrins2OffsetMode]] =
          props.observingMode
            .zoom(
              ObservingMode.Igrins2LongSlit.explicitOffsetMode.get,
              (f: Option[Igrins2OffsetMode] => Option[Igrins2OffsetMode]) =>
                ObservingMode.Igrins2LongSlit.explicitOffsetMode
                  .modify(f)
                  .andThen(ObservingMode.Igrins2LongSlit.explicitOffsets.replace(none)),
              (f: Input[Igrins2OffsetMode] => Input[Igrins2OffsetMode]) =>
                Igrins2LongSlitInput.explicitOffsetMode
                  .modify(f)
                  .andThen(
                    Igrins2LongSlitInput.explicitOffsets.replace(none.orUnassign)
                  )
            )
            .view(_.orUnassign)

        val defaultOffsets: NonEmptyList[Offset] =
          NonEmptyList.fromListUnsafe(defaultOffsetsFor(props.observingMode.get.offsetMode))

        val localOffsetsView: View[NonEmptyList[Offset]] =
          localOffsetsState.withOnMod: nel =>
            // reset offsets if the same as the default
            val newOffsets =
              if nel === defaultOffsets
              then none
              else nel.some
            explicitOffsetsView.set(newOffsets)

        val isNodAlongSlit = props.observingMode.get.offsetMode === Igrins2OffsetMode.NodAlongSlit

        val exposureTimeMode: View[ExposureTimeMode] = props.observingMode
          .zoom(
            ObservingMode.Igrins2LongSlit.exposureTimeMode,
            Igrins2LongSlitInput.exposureTimeMode.modify
          )
          .view(_.toInput.assign)

        React.Fragment(
          <.div(
            ExploreStyles.Igrins2UpperGrid
          )(
            <.div(LucumaPrimeStyles.FormColumnCompact)(
              CustomizableEnumSelectOptional(
                id = "offset-mode".refined,
                view = offsetModeView.withDefault(defaultOffsetMode),
                defaultValue = defaultOffsetMode.some,
                label = "Offset Mode".some,
                disabled = disableEdit,
                showCustomization = showCustomization,
                allowRevertCustomization = allowRevertCustomization,
                resetToOriginal = true
              ),
              if isNodAlongSlit then
                // View to map the explicit offsets to the q component
                val qOffsetsView: View[Option[NonEmptyList[Offset.Q]]] =
                  explicitOffsetsView.zoom(Nested(_).map(_.q).value)(f =>
                    o => f(Nested(o).map(_.q).value).map(_.map(q => Offset.Zero.copy(q = q)))
                  )
                OffsetsControl(
                  view = qOffsetsView,
                  defaultValue = defaultOffsets.map(_.q),
                  onChange = props.sequenceChanged,
                  disabled = disableEdit,
                  showCustomization = showCustomization,
                  allowRevertCustomization = allowRevertCustomization
                )
              else
                React.Fragment(
                  <.span(
                    "Spatial Offsets",
                    HelpIcon("configuration/spatial-offsets.md".refined),
                    CustomizedGroupAddon(
                      "original",
                      explicitOffsetsView.set(none),
                      allowRevertCustomization
                    ).when(explicitOffsetsView.get.isDefined)
                  ),
                  React.Fragment(
                    localOffsetsView.toNelOfViews.toList.zipWithIndex
                      .map: (offsetView, idx) =>
                        OffsetInput(
                          id = NonEmptyString.unsafeFrom(s"spatial-offsets-$idx"),
                          offset = offsetView,
                          readonly = disableEdit,
                          clazz = LucumaPrimeStyles.FormField
                        )
                      .toVdomArray
                  )
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
                "ig2-etm".refined
              )
            ),
            <.div(LucumaPrimeStyles.FormColumnCompact)(
              LambdaAndIntervalFormValues(
                modeData = modeData,
                centralWavelength = BasicConfiguration.Igrins2LongSlit.optimalWavelength,
                units = props.units
              )
            )
          ),
          <.div(
            ExploreStyles.Igrins2LowerGrid,
            AdvancedConfigButtons(
              editState = editState,
              isCustomized = props.observingMode.get.isCustomized,
              revertConfig = props.revertConfig,
              revertCustomizations =
                props.observingMode.view(_.toInput).mod(_.revertCustomizations),
              sequenceChanged = props.sequenceChanged,
              !props.permissions.isFullEdit,
              showAdvancedButton = false
            )
          )
        )
    )

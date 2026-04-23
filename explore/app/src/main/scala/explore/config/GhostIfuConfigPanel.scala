// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import clue.data.Input
import clue.data.syntax.*
import crystal.react.View
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.common.Aligner
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Observation
import explore.model.enums.WavelengthUnits
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostIfu1FiberAgitator
import lucuma.core.enums.GhostIfu2FiberAgitator
import lucuma.core.enums.GhostReadMode
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Program
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.common.style.Css
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.model.ObservingMode.GhostIfu
import lucuma.schemas.odb.input.*
import lucuma.ui.display.given
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given
import monocle.Lens

final case class GhostIfuConfigPanel(
  programId:       Program.Id,
  obsId:           Observation.Id,
  calibrationRole: Option[CalibrationRole],
  observingMode:   Aligner[ObservingMode.GhostIfu, GhostIfuInput],
  revertConfig:    Callback,
  sequenceChanged: Callback,
  permissions:     ConfigEditPermissions,
  units:           WavelengthUnits
) extends ReactFnProps(GhostIfuConfigPanel)

object GhostIfuConfigPanel
    extends ReactFnComponent[GhostIfuConfigPanel](props =>
      for
        ctx       <- useContext(AppContext.ctx)
        editState <- useStateView(ConfigEditState.View)
      yield
        import ctx.given

        val mode                     = props.observingMode.get
        val disableEdit              =
          editState.get =!= ConfigEditState.SimpleEdit && !props.permissions.isFullEdit
        val showCustomization        = props.calibrationRole.isEmpty
        val allowRevertCustomization = props.permissions.isFullEdit

        val resolutionModeView: View[GhostResolutionMode] =
          props.observingMode
            .zoom(GhostIfu.resolutionMode, GhostIfuInput.resolutionMode.modify)
            .view(_.assign)

        val ifu1AgitatorView: View[Option[GhostIfu1FiberAgitator]] =
          props.observingMode
            .zoom(GhostIfu.explicitIfu1Agitator, GhostIfuInput.explicitIfu1Agitator.modify)
            .view(_.orUnassign)

        val ifu2AgitatorView: View[Option[GhostIfu2FiberAgitator]] =
          props.observingMode
            .zoom(GhostIfu.explicitIfu2Agitator, GhostIfuInput.explicitIfu2Agitator.modify)
            .view(_.orUnassign)

        val ifu1EnabledView: View[Boolean] =
          ifu1AgitatorView.zoom[Boolean](
            _.getOrElse(mode.defaultIfu1Agitator) === GhostIfu1FiberAgitator.Enabled
          )(modBool =>
            opt =>
              val current =
                opt.getOrElse(mode.defaultIfu1Agitator) === GhostIfu1FiberAgitator.Enabled
              val next    =
                if modBool(current) then GhostIfu1FiberAgitator.Enabled
                else GhostIfu1FiberAgitator.Disabled
              Option.when(next =!= mode.defaultIfu1Agitator)(next)
          )

        val ifu2EnabledView: View[Boolean] =
          ifu2AgitatorView.zoom[Boolean](
            _.getOrElse(mode.defaultIfu2Agitator) === GhostIfu2FiberAgitator.Enabled
          )(modBool =>
            opt =>
              val current =
                opt.getOrElse(mode.defaultIfu2Agitator) === GhostIfu2FiberAgitator.Enabled
              val next    =
                if modBool(current) then GhostIfu2FiberAgitator.Enabled
                else GhostIfu2FiberAgitator.Disabled
              Option.when(next =!= mode.defaultIfu2Agitator)(next)
          )

        def detectorAligner(
          detectorLens:      Lens[GhostIfu, GhostIfu.GhostDetector],
          detectorInputLens: Lens[GhostIfuInput, Input[GhostDetectorConfigInput]]
        ): Aligner[GhostIfu.GhostDetector, GhostDetectorConfigInput] =
          props.observingMode.zoom[GhostIfu.GhostDetector, GhostDetectorConfigInput](
            modelGet = detectorLens.get,
            modelMod = detectorLens.modify,
            remoteMod = (f: GhostDetectorConfigInput => GhostDetectorConfigInput) =>
              detectorInputLens.modify(_.map(f))
          )

        def detectorPanel(
          label:      String,
          detector:   GhostIfu.GhostDetector,
          colorClazz: Css,
          idPrefix:   NonEmptyString,
          aligner:    Aligner[GhostIfu.GhostDetector, GhostDetectorConfigInput]
        ): VdomNode =
          val timeAndCountView: View[ExposureTimeMode.TimeAndCountMode] =
            aligner
              .zoom(
                GhostIfu.GhostDetector.timeAndCount,
                GhostDetectorConfigInput.exposureTimeMode.modify
              )
              .view(tc => (tc: ExposureTimeMode).toInput.assign)

          val binningView: View[Option[GhostBinning]] =
            aligner
              .zoom(
                GhostIfu.GhostDetector.explicitBinning,
                GhostDetectorConfigInput.explicitBinning.modify
              )
              .view(_.orUnassign)

          val readModeView: View[Option[GhostReadMode]] =
            aligner
              .zoom(
                GhostIfu.GhostDetector.explicitReadMode,
                GhostDetectorConfigInput.explicitReadMode.modify
              )
              .view(_.orUnassign)

          <.div(
            ExploreStyles.GhostDetectorPanel |+| colorClazz,
            <.div(ExploreStyles.GhostDetectorHeader, label),
            <.div(
              LucumaPrimeStyles.FormColumnCompact,
              TimeAndCountModeEditor(
                instrument = mode.instrument.some,
                value = timeAndCountView,
                readonly = !props.permissions.isFullEdit,
                calibrationRole = props.calibrationRole,
                idPrefix = idPrefix
              ),
              CustomizableEnumSelectOptional(
                id = NonEmptyString.unsafeFrom(s"${idPrefix.value}-binning"),
                view = binningView.withDefault(detector.defaultBinning),
                defaultValue = detector.defaultBinning.some,
                label = "Binning".some,
                disabled = disableEdit,
                resetToOriginal = true,
                showCustomization = showCustomization,
                allowRevertCustomization = allowRevertCustomization
              ),
              CustomizableEnumSelectOptional(
                id = NonEmptyString.unsafeFrom(s"${idPrefix.value}-readout"),
                view = readModeView.withDefault(detector.defaultReadMode),
                defaultValue = detector.defaultReadMode.some,
                label = "Readout".some,
                disabled = disableEdit,
                resetToOriginal = true,
                showCustomization = showCustomization,
                allowRevertCustomization = allowRevertCustomization
              )
            )
          )

        React.Fragment(
          <.div(
            ExploreStyles.GhostUpperGrid,
            <.div(
              LucumaPrimeStyles.FormColumnCompact,
              CustomizableEnumSelect(
                id = "ghost-resolution-mode".refined,
                view = resolutionModeView,
                defaultValue = mode.resolutionMode,
                label = "Resolution Mode".some,
                disabled = disableEdit,
                showCustomization = false,
                allowRevertCustomization = false
              ),
              FormLabel(htmlFor = "ghost-sn-wavelength".refined)("λ for S/N"),
              <.span(LucumaPrimeStyles.FormField)(
                <.span(
                  ^.id := "ghost-sn-wavelength",
                  s"${explore.model.display.wavelengthDisplay(props.units).shortName(mode.signalToNoiseAt)} ${props.units.symbol}"
                )
              ),
              FormLabel(htmlFor = "ghost-agitator-ifu1".refined)("Agitators"),
              <.div(
                LucumaPrimeStyles.FormField |+| ExploreStyles.GhostAgitators,
                CheckboxView(
                  id = "ghost-agitator-ifu1".refined,
                  value = ifu1EnabledView,
                  label = "IFU 1",
                  disabled = disableEdit
                ),
                CheckboxView(
                  id = "ghost-agitator-ifu2".refined,
                  value = ifu2EnabledView,
                  label = "IFU 2",
                  disabled = disableEdit
                )
              )
            ),
            detectorPanel(
              label = "Blue Camera",
              detector = mode.blue,
              colorClazz = ExploreStyles.GhostDetectorPanelBlue,
              idPrefix = "ghostBlue".refined,
              aligner = detectorAligner(GhostIfu.blue, GhostIfuInput.blue)
            ),
            detectorPanel(
              label = "Red Camera",
              detector = mode.red,
              colorClazz = ExploreStyles.GhostDetectorPanelRed,
              idPrefix = "ghostRed".refined,
              aligner = detectorAligner(GhostIfu.red, GhostIfuInput.red)
            )
          ),
          <.div(
            ExploreStyles.GhostLowerGrid,
            AdvancedConfigButtons(
              editState = editState,
              isCustomized = mode.isCustomized,
              revertConfig = props.revertConfig,
              revertCustomizations =
                props.observingMode.view(_.toInput).mod(_.revertCustomizations),
              sequenceChanged = props.sequenceChanged,
              readonly = !props.permissions.isFullEdit,
              showAdvancedButton = false
            )
          )
        )
    )

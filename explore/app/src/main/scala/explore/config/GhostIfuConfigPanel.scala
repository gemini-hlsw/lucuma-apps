// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.Eq
import cats.syntax.all.*
import clue.data.Input
import clue.data.syntax.*
import crystal.react.View
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.common.Aligner
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.config.ConfigurationFormats.*
import explore.model.AppContext
import explore.model.Observation
import explore.model.enums.WavelengthUnits
import explore.syntax.ui.*
import explore.utils.forceAssign
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostIfu1FiberAgitator
import lucuma.core.enums.GhostIfu2FiberAgitator
import lucuma.core.enums.GhostReadMode
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.math.Wavelength
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
import cats.Endo

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

        // The same wavelength needs to be set for red and blue.
        val snAtView: View[Wavelength] =
          props.observingMode
            .zoom[Wavelength, GhostIfuInput](GhostIfu.signalToNoiseAt, identity)
            .viewMod: (w: Wavelength) =>
              def detectorInput(base: GhostIfu.GhostDetector): GhostDetectorConfigInput =
                GhostDetectorConfigInput(
                  exposureTimeMode = ExposureTimeModeInput
                    .TimeAndCount(
                      TimeAndCountExposureTimeModeInput(
                        count = base.timeAndCount.count,
                        time = base.timeAndCount.time.toInput,
                        at = w.toInput
                      )
                    )
                    .assign
                )

              val setRed: Endo[GhostIfuInput] =
                forceAssign(GhostIfuInput.red.modify)(mode.red.toInput)(_ =>
                  detectorInput(mode.red)
                )

              val setBlue: Endo[GhostIfuInput] =
                forceAssign(GhostIfuInput.blue.modify)(mode.blue.toInput)(_ =>
                  detectorInput(mode.blue)
                )

              setBlue >>> setRed

        def agitatorView[A: Eq](
          view:     View[Option[A]],
          default:  A,
          enabled:  A,
          disabled: A
        ): View[Boolean] =
          view.zoom(_.getOrElse(default) === enabled): set =>
            opt =>
              val next =
                if set(opt.getOrElse(default) === enabled) then enabled else disabled
              Option.when(next =!= default)(next)

        val ifu1EnabledView: View[Boolean] =
          agitatorView(
            ifu1AgitatorView,
            mode.defaultIfu1Agitator,
            GhostIfu1FiberAgitator.Enabled,
            GhostIfu1FiberAgitator.Disabled
          )

        val ifu2EnabledView: View[Boolean] =
          agitatorView(
            ifu2AgitatorView,
            mode.defaultIfu2Agitator,
            GhostIfu2FiberAgitator.Enabled,
            GhostIfu2FiberAgitator.Disabled
          )

        def detectorAligner(
          lens:      Lens[GhostIfu, GhostIfu.GhostDetector],
          inputLens: Lens[GhostIfuInput, Input[GhostDetectorConfigInput]]
        ): Aligner[GhostIfu.GhostDetector, GhostDetectorConfigInput] =
          props.observingMode.zoom(
            lens = lens,
            remoteMod = f => inputLens.modify(_.map(f))
          )

        // One panel per detector with a subset of the ETM
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
                helpId = Some("configuration/ghost/binning.md".refined),
                disabled = disableEdit,
                resetToOriginal = true,
                showCustomization = false,
                allowRevertCustomization = allowRevertCustomization
              ),
              CustomizableEnumSelectOptional(
                id = NonEmptyString.unsafeFrom(s"${idPrefix.value}-readout"),
                view = readModeView.withDefault(detector.defaultReadMode),
                defaultValue = detector.defaultReadMode.some,
                label = "Readout".some,
                helpId = Some("configuration/ghost/readout.md".refined),
                disabled = disableEdit,
                resetToOriginal = true,
                showCustomization = false,
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
                helpId = Some("configuration/ghost/resolution-mode.md".refined),
                disabled = disableEdit,
                showCustomization = false,
                allowRevertCustomization = allowRevertCustomization
              ),
              FormInputTextView(
                id = "ghost-sn-wavelength".refined,
                value = snAtView,
                label = "λ for S/N",
                validFormat = props.units.toInputFormat,
                changeAuditor = props.units.toSNAuditor,
                units = props.units.symbol,
                disabled = disableEdit
              )(^.autoComplete.off),
              FormLabel(htmlFor = "ghost-agitator-ifu1".refined)(
                "Agitators",
                HelpIcon("configuration/ghost/agitators.md".refined)
              ),
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
            // GHOST doesn't expose advanced customization
            AdvancedConfigButtons(
              editState = editState,
              isCustomized = mode.isCustomized,
              revertConfig = props.revertConfig,
              revertCustomizations =
                props.observingMode.view(_.toInput).mod(_.revertCustomizations),
              sequenceChanged = props.sequenceChanged,
              readonly = !props.permissions.isFullEdit,
              showAdvancedButton = false,
              showCustomizeButton = false
            )
          )
        )
    )

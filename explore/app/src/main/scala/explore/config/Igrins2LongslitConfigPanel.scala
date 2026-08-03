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
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.config.offsets.SlitTelescopeConfigsEditor
import explore.config.offsets.TelescopeConfigsEditor
import explore.model.AppContext
import explore.model.Observation
import explore.model.enums.WavelengthUnits
import explore.model.syntax.all.*
import explore.modes.SpectroscopyModesMatrix
import explore.syntax.ui.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Program
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.igrins2
import lucuma.core.model.sequence.igrins2.CentralWavelength as Igrins2CentralWavelength
import lucuma.core.model.sequence.igrins2.SvcMinExposureTime
import lucuma.core.util.TimeSpan
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Checkbox
import lucuma.react.primereact.InputNumber
import lucuma.react.primereact.Panel
import lucuma.react.primereact.valueOption
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given

final case class Igrins2LongslitConfigPanel(
  programId:       Program.Id,
  obsId:           Observation.Id,
  calibrationRole: Option[CalibrationRole],
  observingMode:   Aligner[ObservingMode.Igrins2LongSlit, Igrins2LongSlitInput],
  revertConfig:    IO[Unit],
  confMatrix:      SpectroscopyModesMatrix,
  sequenceChanged: Callback,
  permissions:     ConfigEditPermissions,
  units:           WavelengthUnits
) extends ReactFnProps(Igrins2LongslitConfigPanel)

object Igrins2LongslitConfigPanel
    extends ReactFnComponent[Igrins2LongslitConfigPanel](props =>
      for {
        ctx       <- useContext(AppContext.ctx)
        modeData  <- useModeData(props.confMatrix, props.observingMode.get)
        editState <- useStateView(ConfigEditState.View)
      } yield
        import ctx.given

        val disableEdit =
          editState.get =!= ConfigEditState.SimpleEdit && !props.permissions.isFullEdit

        val showAcquisitionConfig = props.calibrationRole.needsAcquisitionConfig

        val explicitTelescopeConfigsView: View[Option[SlitTelescopeConfigs]] = props.observingMode
          .zoom(
            ObservingMode.Igrins2LongSlit.explicitTelescopeConfigs,
            Igrins2LongSlitInput.explicitTelescopeConfigs.modify
          )
          .view(_.map(_.toInput).orUnassign)

        val exposureTimeMode: View[ExposureTimeMode] = props.observingMode
          .zoom(
            ObservingMode.Igrins2LongSlit.exposureTimeMode,
            Igrins2LongSlitInput.exposureTimeMode.modify
          )
          .view(_.toInput.assign)

        val svcEnabled: View[Boolean] = props.observingMode
          .zoom(
            ObservingMode.Igrins2LongSlit.svc,
            Igrins2LongSlitInput.svc.modify
          )
          .view(_.map(_.toInput).orUnassign)
          .zoom(_.isDefined)(f =>
            opt =>
              if f(opt.isDefined) then opt.orElse(ObservingMode.Igrins2LongSlit.Svc.Default.some)
              else none
          )

        // When enabled, drill into the SVC sub-config to override the exposure
        // time and the telescope offsets
        val svcConfigFields: Option[(View[TimeSpan], View[NonEmptyList[TelescopeConfig]])] =
          props.observingMode
            .zoom(
              ObservingMode.Igrins2LongSlit.svc,
              forceAssign(Igrins2LongSlitInput.svc.modify)(Igrins2SvcInput())
            )
            .toOption
            .map: svcAligner =>
              val exposure: View[TimeSpan] =
                svcAligner
                  .zoom(
                    ObservingMode.Igrins2LongSlit.Svc.explicitExposure,
                    Igrins2SvcInput.explicitExposure.modify
                  )
                  .view(_.map(_.toInput).orUnassign)
                  .removeOptionality(svcAligner.get.defaultExposure)

              val telescopeConfigs: View[NonEmptyList[TelescopeConfig]] =
                svcAligner
                  .zoom(
                    ObservingMode.Igrins2LongSlit.Svc.explicitTelescopeConfigs,
                    Igrins2SvcInput.explicitTelescopeConfigs.modify
                  )
                  .view(_.map(_.toList.map(_.toInput)).orUnassign)
                  .removeOptionality(svcAligner.get.defaultTelescopeConfigs)

              (exposure, telescopeConfigs)

        React.Fragment(
          <.div(
            ExploreStyles.Igrins2UpperGrid
          )(
            <.div(LucumaPrimeStyles.FormColumnCompact)(
              ExposureTimeModeEditor(
                instrument = props.observingMode.get.instrument,
                wavelength = none,
                exposureTimeMode = exposureTimeMode,
                coadds = none,
                scienceMode = ScienceMode.Spectroscopy,
                readonly = !props.permissions.isFullEdit,
                units = props.units,
                calibrationRole = props.calibrationRole,
                idPrefix = "ig2LongSlit".refined
              )
            ),
            <.div(LucumaPrimeStyles.FormColumnCompact)(
              LambdaAndIntervalFormValues(
                modeData = modeData,
                centralWavelength = Igrins2CentralWavelength,
                units = props.units
              )
            ),
            <.div(LucumaPrimeStyles.FormColumnCompact, ExploreStyles.SlitTelescopeConfigEditor)(
              SlitTelescopeConfigsEditor(
                explicitValue = explicitTelescopeConfigsView,
                defaultValue = props.observingMode.get.defaultTelescopeConfigs,
                defaultForPreset = igrins2.defaultSlitTelescopeConfigs,
                helpId = "configuration/igrins2/spatial-offsets.md".refined,
                presetsReadonly = !props.permissions.isFullEdit,
                editingReadonly = disableEdit
              )
            )
          ),
          <.div(
            ExploreStyles.Igrins2LowerGrid,
            Panel(
              header = <.span(
                "Acquisition",
                HelpIcon("configuration/igrins2/acquisition-customization.md".refined)
              ),
              toggleable = true,
              collapsed = true
            )(
              <.div(
                ExploreStyles.SvcAcquisitionLayout,
                <.div(
                  LucumaPrimeStyles.FormColumnCompact,
                  <.div(
                    LucumaPrimeStyles.FormFieldLabel,
                    <.label(^.htmlFor := "ig2-svc-enabled", "Enable SVC")
                  ),
                  <.div(LucumaPrimeStyles.FormField)(
                    Checkbox(
                      id = "ig2-svc-enabled",
                      checked = svcEnabled.get,
                      disabled = !props.permissions.isFullEdit,
                      onChange = r => svcEnabled.set(r)
                    )
                  ),
                  svcConfigFields.whenDefined(using
                    (svcExposure, _) =>
                      ReactFragment(
                        <.div(
                          LucumaPrimeStyles.FormFieldLabel,
                          <.label(^.htmlFor := "ig2-svc-exposure", "Exp. Time")
                        ),
                        <.div(LucumaPrimeStyles.FormField)(
                          InputNumber(
                            id = "ig2-svc-exposure",
                            value = svcExposure.get.toSeconds.toDouble,
                            min = SvcMinExposureTime.toSeconds.toDouble,
                            maxFractionDigits = 3,
                            disabled = !props.permissions.isFullEdit,
                            onValueChange = e =>
                              e.valueOption
                                .flatMap(d => TimeSpan.fromSeconds(BigDecimal(d)))
                                .fold(Callback.empty)(svcExposure.set)
                          )
                        )
                      )
                  )
                ),
                svcConfigFields.whenDefined(using
                  (_, svcTelescopeConfigs) =>
                    <.div(LucumaPrimeStyles.FormColumnCompact,
                          ExploreStyles.SvcTelescopeConfigsEditor
                    )(
                      TelescopeConfigsEditor(
                        telescopeConfigs = svcTelescopeConfigs,
                        readonly = !props.permissions.isFullEdit
                      )
                    )
                )
              )
            ).when(showAcquisitionConfig),
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

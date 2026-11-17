// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.Endo
import cats.effect.IO
import cats.syntax.all.*
import clue.data.Input
import clue.data.syntax.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.Aligner
import explore.components.ui.ExploreStyles
import explore.config.offsets.SlitTelescopeConfigsEditor
import explore.model.AppContext
import explore.model.ExposureTimeRules
import explore.model.enums.ExposureTimeModeType
import explore.model.enums.WavelengthUnits
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ScienceMode
import lucuma.core.enums.ScorpioFpu
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.react.common.Css
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given

import scala.util.chaining.*

final case class ScorpioLongSlitConfigPanel(
  calibrationRole: Option[CalibrationRole],
  observingMode:   Aligner[ObservingMode.ScorpioLongSlit, ScorpioLongSlitInput],
  revertConfig:    Option[IO[Unit]],
  sequenceChanged: Callback,
  permissions:     ConfigEditPermissions,
  units:           WavelengthUnits
) extends ReactFnProps(ScorpioLongSlitConfigPanel)

object ScorpioLongSlitConfigPanel
    extends ReactFnComponent[ScorpioLongSlitConfigPanel](props =>
      for
        ctx       <- useContext(AppContext.ctx)
        editState <- useStateView(ConfigEditState.View)
      yield
        import ctx.given

        val readonly: Boolean = !props.permissions.isFullEdit

        val fpuView: View[ScorpioFpu] = props.observingMode
          .zoom(ObservingMode.ScorpioLongSlit.fpu, ScorpioLongSlitInput.fpu.modify)
          .view(_.assign)

        val explicitTelescopeConfigsView: View[Option[SlitTelescopeConfigs]] = props.observingMode
          .zoom(
            ObservingMode.ScorpioLongSlit.explicitTelescopeConfigs,
            ScorpioLongSlitInput.explicitTelescopeConfigs.modify
          )
          .view(_.map(_.toInput).orUnassign)

        // g/r and i/z share a detector controller: each pair is edited as one value, and only that
        // pair is sent to the ODB.
        val grView: View[ExposureTimeMode] = props.observingMode
          .zoom(
            ObservingMode.ScorpioLongSlit.gr,
            (f: Endo[Input[ExposureTimeModeInput]]) =>
              (in: ScorpioLongSlitInput) => f(in.g).pipe(etm => in.copy(g = etm, r = etm))
          )
          .view(_.toInput.assign)

        val izView: View[ExposureTimeMode] = props.observingMode
          .zoom(
            ObservingMode.ScorpioLongSlit.iz,
            (f: Endo[Input[ExposureTimeModeInput]]) =>
              (in: ScorpioLongSlitInput) => f(in.i).pipe(etm => in.copy(i = etm, z = etm))
          )
          .view(_.toInput.assign)

        def channelRow(name: String, etm: View[ExposureTimeMode]): VdomNode =
          React.Fragment.withKey(name)(
            <.label(ExploreStyles.ScorpioChannelLabel, name),
            ExposureTimeModeEditor(
              instrument = props.observingMode.get.instrument,
              exposureRules = ExposureTimeRules(props.observingMode.get.instrument),
              wavelength = none,
              exposureTimeMode = etm,
              scienceMode = ScienceMode.Imaging,
              readonly = !props.permissions.canEditScienceEtm,
              units = props.units,
              calibrationRole = props.calibrationRole,
              idPrefix = NonEmptyString.unsafeFrom(s"scorpio-$name"),
              forceModeType = ExposureTimeModeType.TimeAndCount.some,
              forGridRow = true
            )
          )

        def pairLink(a: String, b: String, rows: Css): VdomNode =
          <.span(
            ExploreStyles.ScorpioChannelLink,
            rows,
            ^.title := s"$a and $b share a detector controller, so their exposures are always equal",
            Icons.LockKeyhole
          )

        React.Fragment(
          <.div(ExploreStyles.Igrins2UpperGrid)(
            <.div(LucumaPrimeStyles.FormColumnCompact)(
              FormEnumDropdownView(
                id = "scorpio-fpu".refined,
                value = fpuView,
                label = "FPU",
                disabled = readonly
              )
            ),
            <.div(ExploreStyles.ScorpioChannelGrid)(
              <.label(
                LucumaPrimeStyles.FormFieldLabel,
                ExploreStyles.ScorpioChannelTitle,
                "Exposure Times"
              ),
              channelRow("g", grView),
              channelRow("r", grView),
              channelRow("i", izView),
              channelRow("z", izView),
              pairLink("g", "r", ExploreStyles.ScorpioChannelLinkGR),
              pairLink("i", "z", ExploreStyles.ScorpioChannelLinkIZ)
            ),
            <.div(LucumaPrimeStyles.FormColumnCompact, ExploreStyles.SlitTelescopeConfigEditor)(
              SlitTelescopeConfigsEditor(
                explicitValue = explicitTelescopeConfigsView,
                defaultValue = props.observingMode.get.defaultTelescopeConfigs,
                presets = props.observingMode.get.telescopeConfigPresets,
                helpId = "configuration/slit-spatial-offsets.md".refined,
                presetsReadonly = readonly,
                editingReadonly = readonly
              )
            )
          ),
          <.div(ExploreStyles.Igrins2LowerGrid)(
            AdvancedConfigButtons(
              editState = editState,
              isCustomized = props.observingMode.get.isCustomized,
              revertConfig = props.revertConfig,
              revertCustomizations =
                props.observingMode.view(_.toInput).mod(_.revertCustomizations),
              sequenceChanged = props.sequenceChanged,
              readonly = readonly,
              showAdvancedButton = false,
              showCustomizeButton = false
            )
          )
        )
    )

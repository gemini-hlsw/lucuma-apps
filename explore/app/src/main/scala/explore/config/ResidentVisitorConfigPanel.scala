// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.effect.IO
import cats.syntax.all.*
import clue.data.syntax.*
import crystal.react.View
import crystal.react.ViewOpt
import crystal.react.hooks.*
import explore.common.Aligner
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Observation
import explore.model.ScienceRequirements
import explore.model.enums.WavelengthUnits
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.ExposureTimeMode.TimeAndCountMode
import lucuma.core.model.Program
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.CentralWavelength
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given

case class ResidentVisitorConfigPanel(
  programId:        Program.Id,
  obsId:            Observation.Id,
  observingMode:    Aligner[ObservingMode.Visitor, VisitorInput],
  requirementsView: View[ScienceRequirements],
  revertConfig:     IO[Unit],
  sequenceChanged:  Callback,
  permissions:      ConfigEditPermissions,
  units:            WavelengthUnits
) extends ReactFnProps(ResidentVisitorConfigPanel)

object ResidentVisitorConfigPanel
    extends ReactFnComponent[ResidentVisitorConfigPanel](props =>
      for
        ctx       <- useContext(AppContext.ctx)
        editState <- useStateView(ConfigEditState.View)
      yield
        import ctx.given

        val mode        = props.observingMode.get
        val disableEdit =
          editState.get =!= ConfigEditState.SimpleEdit && !props.permissions.isFullEdit

        val centralWavelengthView: View[Wavelength] =
          props.observingMode
            .zoom(
              ObservingMode.Visitor.centralWavelength.andThen(CentralWavelength.Value),
              VisitorInput.centralWavelength.modify
            )
            .view(_.toInput.assign)

        val scienceFovView: View[Angle] =
          props.observingMode
            .zoom(ObservingMode.Visitor.scienceFov, VisitorInput.scienceFov.modify)
            .view(_.toInput.assign)

        val exposureTimeMode: ViewOpt[TimeAndCountMode] =
          props.requirementsView
            .zoom(ScienceRequirements.exposureTimeMode.some)
            .zoom(ExposureTimeMode.timeAndCount)

        val header: VdomNode =
          React.Fragment(
            FormLabel(htmlFor = "visitor-instrument-name".refined)("Instrument Name"),
            <.div(^.id := "visitor-instrument-name", mode.instrument.longName)
          )

        React.Fragment(
          VisitorConfigFields(
            header = header,
            centralWavelength = centralWavelengthView,
            scienceFov = scienceFovView,
            timeAndCount = exposureTimeMode.asView,
            instrument = mode.instrument,
            units = props.units,
            disabled = disableEdit,
            timeAndCountReadonly = (!props.permissions.isFullEdit).some
          ),
          <.div(
            ExploreStyles.VisitorLowerGrid,
            AdvancedConfigButtons(
              editState = editState,
              isCustomized = mode.isCustomized,
              revertConfig = props.revertConfig,
              revertCustomizations = Callback.empty,
              sequenceChanged = props.sequenceChanged,
              readonly = !props.permissions.isFullEdit,
              showAdvancedButton = false,
              showCustomizeButton = false
            )
          )
        )
    )

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
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.config.ConfigurationFormats.*
import explore.model.AppContext
import explore.model.ExploreModelValidators
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
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given

case class ResidentVisitorConfigPanel(
  programId:        Program.Id,
  obsId:            Observation.Id,
  observingMode:    Aligner[ObservingMode.Visitor, VisitorInput],
  requirementsView: View[ScienceRequirements],
  revertConfig:     IO[Unit],
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

        val mode = props.observingMode.get

        val centralWavelengthView: View[Wavelength] =
          props.observingMode
            .zoom(
              ObservingMode.Visitor.centralWavelength.andThen(CentralWavelength.Value),
              VisitorInput.centralWavelength.modify
            )
            .view(_.toInput.assign)

        val agsDiameterView: View[Angle] =
          props.observingMode
            .zoom(ObservingMode.Visitor.agsDiameter, VisitorInput.agsDiameter.modify)
            .view(_.toInput.assign)

        val exposureTimeMode: ViewOpt[TimeAndCountMode] =
          props.requirementsView
            .zoom(ScienceRequirements.exposureTimeMode.some)
            .zoom(ExposureTimeMode.timeAndCount)

        React.Fragment(
          <.div(
            ExploreStyles.VisitorUpperGrid,
            LucumaPrimeStyles.FormColumnCompact,
            FormLabel(htmlFor = "visitor-instrument-name".refined)("Instrument Name"),
            <.div(^.id := "visitor-instrument-name", mode.instrument.longName),
            FormInputTextView(
              id = "visitor-central-wavelength".refined,
              value = centralWavelengthView,
              label = "Central Wavelength",
              validFormat = props.units.toInputFormat,
              changeAuditor = props.units.toAuditor,
              units = props.units.symbol,
              disabled = true // the mode defines the central wavelength, so it can't be edited
            )(^.autoComplete.off),
            FormInputTextView(
              id = "visitor-science-fov".refined,
              value = agsDiameterView,
              label = React.Fragment("AGS Diameter",
                                     HelpIcon("configuration/visitor/ags-diameter.md".refined)
              ),
              validFormat = ExploreModelValidators.decimalArcsecondsValidWedge,
              changeAuditor = angleArcsecondsChangeAuditor,
              units = "arcsec",
              disabled = true // the mode defines the science FOV, so it can't be edited
            )(^.autoComplete.off),
            exposureTimeMode.asView.map: tcView =>
              TimeAndCountModeEditor(
                instrument = mode.instrument.some,
                value = tcView,
                readonly = !props.permissions.isFullEdit,
                calibrationRole = none,
                idPrefix = "visitor".refined,
                showCount = true
              )
          ),
          <.div(
            ExploreStyles.VisitorLowerGrid,
            AdvancedConfigButtons(
              editState = editState,
              isCustomized = mode.isCustomized,
              revertConfig = props.revertConfig,
              revertCustomizations = Callback.empty,
              sequenceChanged = Callback.empty,
              readonly = !props.permissions.isFullEdit,
              showAdvancedButton = false,
              showCustomizeButton = false
            )
          )
        )
    )

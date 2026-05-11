// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import clue.data.syntax.*
import crystal.react.View
import crystal.react.hooks.*
import eu.timepit.refined.types.numeric.PosInt
import explore.common.Aligner
import explore.components.ui.ExploreStyles
import explore.config.ConfigurationFormats.*
import explore.model.AppContext
import explore.model.ExploreModelValidators
import explore.model.Observation
import explore.model.ScienceRequirements
import explore.model.enums.WavelengthUnits
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Program
import lucuma.core.util.TimeSpan
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

final case class VisitorConfigPanel(
  programId:        Program.Id,
  obsId:            Observation.Id,
  calibrationRole:  Option[CalibrationRole],
  observingMode:    Aligner[ObservingMode.Visitor, VisitorInput],
  requirementsView: View[ScienceRequirements],
  revertConfig:     Callback,
  sequenceChanged:  Callback,
  permissions:      ConfigEditPermissions,
  units:            WavelengthUnits
) extends ReactFnProps(VisitorConfigPanel)

object VisitorConfigPanel
    extends ReactFnComponent[VisitorConfigPanel](props =>
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

        val exposureTimeMode: View[Option[ExposureTimeMode]] =
          props.requirementsView.zoom(ScienceRequirements.exposureTimeMode)

        // Visitors only support Time & Count mode; project the optional ExposureTimeMode
        // onto a concrete TimeAndCountMode using current central wavelength for `at`.
        val defaultTimeAndCount: ExposureTimeMode.TimeAndCountMode =
          ExposureTimeMode.TimeAndCountMode(
            TimeSpan.unsafeFromMicroseconds(0L),
            PosInt.unsafeFrom(1),
            centralWavelengthView.get
          )

        val timeAndCountView: View[ExposureTimeMode.TimeAndCountMode] =
          exposureTimeMode.zoom {
            case Some(t: ExposureTimeMode.TimeAndCountMode) => t
            case _                                          => defaultTimeAndCount
          } { f => opt =>
            val current = opt match
              case Some(t: ExposureTimeMode.TimeAndCountMode) => t
              case _                                          => defaultTimeAndCount
            f(current).some
          }

        val instrumentLabel: String = mode.instrument.longName

        React.Fragment(
          <.div(
            ExploreStyles.VisitorUpperGrid,
            <.div(
              ExploreStyles.VisitorHeader,
              LucumaPrimeStyles.FormColumnCompact,
              FormLabel(htmlFor = "visitor-instrument-name".refined)("Instrument Name"),
              <.div(^.id := "visitor-instrument-name", instrumentLabel)
            ),
            <.div(
              LucumaPrimeStyles.FormColumnCompact,
              FormInputTextView(
                id = "visitor-central-wavelength".refined,
                value = centralWavelengthView,
                label = "Central Wavelength",
                validFormat = props.units.toInputFormat,
                changeAuditor = props.units.toAuditor,
                units = props.units.symbol,
                disabled = disableEdit
              )(^.autoComplete.off),
              FormInputTextView(
                id = "visitor-science-fov".refined,
                value = scienceFovView,
                label = "Sci FoV Diameter",
                validFormat = ExploreModelValidators.decimalArcsecondsValidWedge,
                changeAuditor = angleArcsecondsChangeAuditor,
                units = "arcsec",
                disabled = disableEdit
              )(^.autoComplete.off)
            ),
            <.div(
              LucumaPrimeStyles.FormColumnCompact,
              TimeAndCountModeEditor(
                instrument = mode.instrument.some,
                value = timeAndCountView,
                readonly = !props.permissions.isFullEdit,
                calibrationRole = props.calibrationRole,
                idPrefix = "visitor".refined,
                showCount = true
              )
            )
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

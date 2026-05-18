// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import crystal.react.View
import explore.components.ui.ExploreStyles
import explore.config.ConfigurationFormats.*
import explore.model.ExploreModelValidators
import explore.model.enums.WavelengthUnits
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Instrument
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given

// Fields shared by the resident-visitor configuration editor.
case class VisitorConfigFields(
  header:               VdomNode,
  centralWavelength:    View[Wavelength],
  scienceFov:           View[Angle],
  timeAndCount:         Option[View[ExposureTimeMode.TimeAndCountMode]],
  instrument:           Instrument,
  units:                WavelengthUnits,
  disabled:             Boolean,
  timeAndCountReadonly: Option[Boolean] = None
) extends ReactFnProps(VisitorConfigFields)

object VisitorConfigFields
    extends ReactFnComponent[VisitorConfigFields](props =>
      <.div(
        ExploreStyles.VisitorUpperGrid,
        <.div(
          ExploreStyles.VisitorHeader,
          LucumaPrimeStyles.FormColumnCompact,
          props.header
        ),
        <.div(
          LucumaPrimeStyles.FormColumnCompact,
          FormInputTextView(
            id = "visitor-central-wavelength".refined,
            value = props.centralWavelength,
            label = "Central Wavelength",
            validFormat = props.units.toInputFormat,
            changeAuditor = props.units.toAuditor,
            units = props.units.symbol,
            disabled = props.disabled
          )(^.autoComplete.off),
          FormInputTextView(
            id = "visitor-science-fov".refined,
            value = props.scienceFov,
            label = "Instrument Diameter",
            validFormat = ExploreModelValidators.decimalArcsecondsValidWedge,
            changeAuditor = angleArcsecondsChangeAuditor,
            units = "arcsec",
            disabled = props.disabled
          )(^.autoComplete.off)
        ),
        props.timeAndCount.map: tcView =>
          <.div(
            LucumaPrimeStyles.FormColumnCompact,
            TimeAndCountModeEditor(
              instrument = props.instrument.some,
              value = tcView,
              readonly = props.timeAndCountReadonly.getOrElse(props.disabled),
              calibrationRole = none,
              idPrefix = "visitor".refined,
              showCount = true
            )
          )
      )
    )

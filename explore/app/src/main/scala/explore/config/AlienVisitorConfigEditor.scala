// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import crystal.react.View
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.ui.ExploreStyles
import explore.config.ConfigurationFormats.*
import explore.model.TimeAndCountModeInfo
import explore.model.display.given
import explore.model.enums.WavelengthUnits
import explore.model.formats.durationHM
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Site
import lucuma.core.enums.VisitorObservingModeType
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.util.TimeSpan
import lucuma.core.validation.InputValidSplitEpi
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.common.style.Css
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.clearable
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given

// The alien-visitor editor is a *preview* of the configuration to be created.
// All inputs are optional (empty on init) and only persisted on Accept.
final case class AlienVisitorConfigEditor(
  site:              View[Option[Site]],
  name:              View[Option[NonEmptyString]],
  centralWavelength: View[Option[Wavelength]],
  scienceFov:        View[Option[Angle]],
  totalRequestTime:  View[Option[TimeSpan]],
  timeAndCount:      View[TimeAndCountModeInfo],
  calibrationRole:   Option[CalibrationRole],
  readonly:          Boolean,
  units:             WavelengthUnits
) extends ReactFnProps(AlienVisitorConfigEditor)

object AlienVisitorConfigEditor
    extends ReactFnComponent[AlienVisitorConfigEditor](props =>
      val mode: Option[VisitorObservingModeType] = props.site.get.map:
        case Site.GN => VisitorObservingModeType.VisitorNorth
        case Site.GS => VisitorObservingModeType.VisitorSouth

      <.div(
        ExploreStyles.VisitorUpperGrid,
        <.div(
          ExploreStyles.VisitorHeader,
          LucumaPrimeStyles.FormColumnCompact,
          FormLabel(htmlFor = "visitor-basic-site".refined)("Site"),
          EnumDropdownOptionalView(
            id = "visitor-basic-site".refined,
            value = props.site,
            showClear = true,
            disabled = props.readonly
          ),
          FormInputTextView(
            id = "visitor-basic-name".refined,
            value = props.name,
            label = "Name",
            groupClass = ExploreStyles.WarningInput.when_(props.name.get.isEmpty),
            validFormat = InputValidSplitEpi.nonEmptyString.optional,
            disabled = props.readonly
          ).clearable(^.autoComplete.off)
        ),
        <.div(
          LucumaPrimeStyles.FormColumnCompact,
          FormInputTextView(
            id = "visitor-basic-central-wavelength".refined,
            value = props.centralWavelength,
            label = "Central Wavelength",
            groupClass = ExploreStyles.WarningInput.when_(props.centralWavelength.get.isEmpty),
            validFormat = props.units.toInputWedge,
            changeAuditor = props.units.toAuditor.optional,
            units = props.units.symbol,
            disabled = props.readonly
          ).clearable(^.autoComplete.off),
          FormInputTextView(
            id = "visitor-basic-science-fov".refined,
            value = props.scienceFov,
            label = "Sci FoV Diameter",
            groupClass = ExploreStyles.WarningInput.when_(props.scienceFov.get.isEmpty),
            validFormat = angleArcsecsFormat,
            changeAuditor = angleArcsecondsChangeAuditor,
            units = "arcsec",
            disabled = props.readonly
          ).clearable(^.autoComplete.off),
          FormInputTextView(
            id = "visitor-basic-total-time".refined,
            value = props.totalRequestTime,
            label = "Total Req. Time",
            groupClass = ExploreStyles.WarningInput.when_(props.totalRequestTime.get.isEmpty),
            validFormat = durationHM.optional,
            units = "h:mm",
            disabled = props.readonly
          ).clearable(^.autoComplete.off)
        ),
        <.div(
          LucumaPrimeStyles.FormColumnCompact,
          TimeAndCountFieldsEditor(
            instrument = mode.map(_.instrument),
            options = props.timeAndCount,
            readonly = props.readonly,
            calibrationRole = props.calibrationRole,
            showCount = true,
            makeId = base => NonEmptyString.unsafeFrom(s"visitor-basic$base"),
            labelClass = Css.Empty,
            controlsWrapper = (node, _) => node
          )
        )
      )
    )

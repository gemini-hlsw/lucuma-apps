// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.config.ConfigurationFormats.*
import explore.model.display.given
import explore.model.enums.WavelengthUnits
import explore.model.formats.durationHM
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Site
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.util.TimeSpan
import lucuma.core.validation.InputValidSplitEpi
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.*
import lucuma.ui.primereact.clearable
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given
import monocle.Focus
import monocle.Lens

case class AlienVisitorState(
  site:              Option[Site],
  name:              Option[NonEmptyString],
  centralWavelength: Option[Wavelength],
  agsDiameter:       Option[Angle],
  totalRequestTime:  Option[TimeSpan]
)

object AlienVisitorState:
  val Empty: AlienVisitorState = AlienVisitorState(none, none, none, none, none)

  val site: Lens[AlienVisitorState, Option[Site]] =
    Focus[AlienVisitorState](_.site)

  val name: Lens[AlienVisitorState, Option[NonEmptyString]] =
    Focus[AlienVisitorState](_.name)

  val centralWavelength: Lens[AlienVisitorState, Option[Wavelength]] =
    Focus[AlienVisitorState](_.centralWavelength)

  val agsDiameter: Lens[AlienVisitorState, Option[Angle]] =
    Focus[AlienVisitorState](_.agsDiameter)

  val totalRequestTime: Lens[AlienVisitorState, Option[TimeSpan]] =
    Focus[AlienVisitorState](_.totalRequestTime)

// All inputs are optional and only persisted on Accept.
case class AlienVisitorConfigEditor(
  state:    View[AlienVisitorState],
  readonly: Boolean,
  units:    WavelengthUnits
) extends ReactFnProps(AlienVisitorConfigEditor)

object AlienVisitorConfigEditor
    extends ReactFnComponent[AlienVisitorConfigEditor](props =>
      val site              = props.state.zoom(AlienVisitorState.site)
      val name              = props.state.zoom(AlienVisitorState.name)
      val centralWavelength = props.state.zoom(AlienVisitorState.centralWavelength)
      val agsDiameter       = props.state.zoom(AlienVisitorState.agsDiameter)
      val totalRequestTime  = props.state.zoom(AlienVisitorState.totalRequestTime)

      <.div(
        ExploreStyles.VisitorUpperGrid,
        LucumaPrimeStyles.FormColumnCompact,
        FormLabel(htmlFor = "visitor-basic-site".refined)("Site"),
        EnumDropdownOptionalView(
          id = "visitor-basic-site".refined,
          value = site,
          showClear = true,
          clazz = ExploreStyles.WarningInput.when_(site.get.isEmpty),
          disabled = props.readonly
        ),
        FormInputTextView(
          id = "visitor-basic-name".refined,
          value = name,
          label = "Name",
          groupClass = ExploreStyles.WarningInput.when_(name.get.isEmpty),
          validFormat = InputValidSplitEpi.nonEmptyString.optional,
          disabled = props.readonly
        ).clearable(^.autoComplete.off),
        FormInputTextView(
          id = "visitor-basic-central-wavelength".refined,
          value = centralWavelength,
          label = "Central Wavelength",
          groupClass = ExploreStyles.WarningInput.when_(centralWavelength.get.isEmpty),
          validFormat = props.units.toInputWedge,
          changeAuditor = props.units.toAuditor.optional,
          units = props.units.symbol,
          disabled = props.readonly
        ).clearable(^.autoComplete.off),
        FormInputTextView(
          id = "visitor-basic-science-fov".refined,
          value = agsDiameter,
          label = React.Fragment("AGS Diameter",
                                 HelpIcon("configuration/visitor/ags-diameter.md".refined)
          ),
          groupClass = ExploreStyles.WarningInput.when_(agsDiameter.get.isEmpty),
          validFormat = angleArcsecsFormat,
          changeAuditor = ChangeAuditor.posBigDecimal(2.refined).optional,
          units = "arcsec",
          disabled = props.readonly
        ).clearable(^.autoComplete.off),
        FormInputTextView(
          id = "visitor-basic-total-time".refined,
          value = totalRequestTime,
          label = React.Fragment(
            "Total Requested Time",
            HelpIcon("configuration/visitor/total-request-time.md".refined)
          ),
          groupClass = ExploreStyles.WarningInput.when_(totalRequestTime.get.isEmpty),
          validFormat = durationHM.optional,
          units = "h:mm",
          disabled = props.readonly
        ).clearable(^.autoComplete.off)
      )
    )

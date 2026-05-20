// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.effect.IO
import cats.syntax.all.*
import clue.data.syntax.*
import crystal.react.View
import crystal.react.hooks.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.common.Aligner
import explore.components.ui.ExploreStyles
import explore.config.ConfigurationFormats.*
import explore.model.AppContext
import explore.model.ExploreModelValidators
import explore.model.Observation
import explore.model.display.given
import explore.model.enums.WavelengthUnits
import explore.model.formats.durationHM
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Site
import lucuma.core.enums.VisitorObservingModeType
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.model.Program
import lucuma.core.util.TimeSpan
import lucuma.core.validation.InputValidSplitEpi
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.CentralWavelength
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.*
import lucuma.ui.primereact.clearable
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given
import monocle.Lens

case class AlienVisitorConfigPanel(
  programId:     Program.Id,
  obsId:         Observation.Id,
  observingMode: Aligner[ObservingMode.Visitor, VisitorInput],
  revertConfig:  IO[Unit],
  permissions:   ConfigEditPermissions,
  units:         WavelengthUnits
) extends ReactFnProps(AlienVisitorConfigPanel)

object AlienVisitorConfigPanel
    extends ReactFnComponent[AlienVisitorConfigPanel](props =>
      for
        ctx       <- useContext(AppContext.ctx)
        editState <- useStateView(ConfigEditState.View)
      yield
        import ctx.given

        val mode        = props.observingMode.get
        val disableEdit =
          editState.get =!= ConfigEditState.SimpleEdit && !props.permissions.isFullEdit

        // Send all fields on every update (mode/site, centralWavelength,
        val visitorView: View[ObservingMode.Visitor] =
          props.observingMode.view: m =>
            VisitorInput(
              mode = m.mode.assign,
              centralWavelength = m.centralWavelength.value.toInput.assign,
              scienceFov = m.scienceFov.toInput.assign,
              name = m.name.orUnassign,
              totalRequestTime = m.totalRequestTime.map(_.toInput).orUnassign
            )

        val centralWavelengthView: View[Wavelength] =
          visitorView.zoom(
            ObservingMode.Visitor.centralWavelength.andThen(CentralWavelength.Value)
          )

        val scienceFovView: View[Angle] =
          visitorView.zoom(ObservingMode.Visitor.scienceFov)

        // Site is encoded in the mode type for aliens
        val unsafeModelToSite: Lens[VisitorObservingModeType, Site] =
          Lens[VisitorObservingModeType, Site] {
            case VisitorObservingModeType.VisitorNorth => Site.GN
            case VisitorObservingModeType.VisitorSouth => Site.GS
            case _                                     => Site.GN
          }(site =>
            _ =>
              site match
                case Site.GN => VisitorObservingModeType.VisitorNorth
                case Site.GS => VisitorObservingModeType.VisitorSouth
          )

        val siteView: View[Site] =
          visitorView.zoom(ObservingMode.Visitor.mode).zoom(unsafeModelToSite)

        val nameView: View[Option[NonEmptyString]] =
          visitorView.zoom(ObservingMode.Visitor.name)

        val totalRequestTimeView: View[Option[TimeSpan]] =
          visitorView.zoom(ObservingMode.Visitor.totalRequestTime)

        React.Fragment(
          <.div(
            ExploreStyles.VisitorUpperGrid,
            LucumaPrimeStyles.FormColumnCompact,
            FormLabel(htmlFor = "visitor-site".refined)("Site"),
            EnumDropdownView(
              id = "visitor-site".refined,
              value = siteView,
              disabled = disableEdit
            ),
            FormInputTextView(
              id = "visitor-name".refined,
              value = nameView,
              label = "Name",
              validFormat = InputValidSplitEpi.nonEmptyString.optional,
              disabled = disableEdit
            ).clearable(^.autoComplete.off),
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
              label = "Instrument Diameter",
              validFormat = ExploreModelValidators.decimalArcsecondsValidWedge,
              changeAuditor = ChangeAuditor.posBigDecimal(2.refined),
              units = "arcsec",
              disabled = disableEdit
            )(^.autoComplete.off),
            FormInputTextView(
              id = "visitor-total-time".refined,
              value = totalRequestTimeView,
              label = "Total Req. Time",
              validFormat = durationHM.optional,
              units = "h:mm",
              disabled = disableEdit
            ).clearable(^.autoComplete.off)
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

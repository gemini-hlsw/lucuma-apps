// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.data.EitherNec
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import clue.data.syntax.*
import crystal.react.*
import crystal.react.hooks.*
import explore.Icons
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Observation
import explore.model.ScienceRequirements
import explore.model.ScienceRequirements.Imaging
import explore.model.ScienceRequirements.Spectroscopy
import explore.model.display.given
import explore.model.enums.ConfigurationKind
import explore.model.enums.ExposureTimeModeType
import explore.model.enums.PosAngleOptions
import explore.model.enums.WavelengthUnits
import explore.model.itc.ItcTarget
import explore.model.itc.ItcTargetProblem
import explore.model.syntax.all.*
import explore.modes.ConfigSelection
import explore.modes.ScienceModes
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ImagingCapability
import lucuma.core.enums.ScienceMode
import lucuma.core.enums.Site
import lucuma.core.enums.VisitorObservingModeType
import lucuma.core.math.Coordinates
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.User
import lucuma.core.util.NewBoolean
import lucuma.core.util.Timestamp
import lucuma.react.common.ReactFnProps
import lucuma.react.fa.FontAwesomeIcon
import lucuma.react.primereact.Button
import lucuma.react.primereact.Tag
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.CentralWavelength
import lucuma.schemas.odb.input.*
import lucuma.ui.LucumaIcons
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.syntax.effect.*

case class BasicConfigurationPanel(
  userId:              Option[User.Id],
  obsId:               Observation.Id,
  requirementsView:    View[ScienceRequirements],
  selectedConfig:      View[ConfigSelection],
  constraints:         ConstraintSet,
  itcTargets:          EitherNec[ItcTargetProblem, NonEmptyList[ItcTarget]],
  baseCoordinates:     Option[Coordinates],
  calibrationRole:     Option[CalibrationRole],
  createConfig:        (ObservingModeInput, PosAngleOptions) => IO[Unit],
  confMatrix:          ScienceModes,
  customSedTimestamps: List[Timestamp],
  readonly:            Boolean,
  units:               WavelengthUnits,
  targetView:          View[Option[ItcTarget]]
) extends ReactFnProps(BasicConfigurationPanel.component)

private object BasicConfigurationPanel:
  private type Props = BasicConfigurationPanel

  private object Creating extends NewBoolean

  private val component =
    ScalaFnComponent[Props]: props =>
      for
        ctx                  <- useContext(AppContext.ctx)
        scienceModeType      <- useStateView[ConfigurationKind](ConfigurationKind.Spectroscopy)
        _                    <- useEffectWithDeps(props.requirementsView.get.scienceModeType): modeType =>
                                  scienceModeType.mod: current =>
                                    // There is no visitor science mode.
                                    if current === ConfigurationKind.Visitor then current
                                    else ConfigurationKind.fromScienceMode(modeType)
        creating             <- useStateView(Creating(false))
        imagingCap           <- useStateView(none[ImagingCapability])
        exposureTimeModeType <- useStateView(
                                  props.requirementsView.get.exposureTimeMode
                                    .map(ExposureTimeModeType.fromExposureTimeMode)
                                    .getOrElse(ExposureTimeModeType.SignalToNoise)
                                )
        // Local preview state for alien visitor mode; only persisted on Accept.
        alienVisitor         <- useStateView(AlienVisitorState.Empty)
      yield
        import ctx.given

        val etm: Option[ExposureTimeMode] = props.requirementsView.get.exposureTimeMode
        val isVisitor                     = props.selectedConfig.get.isVisitor
        val isAlienVisitor                = scienceModeType.get === ConfigurationKind.Visitor
        val visitorEtmOk                  =
          !isVisitor || exposureTimeModeType.get === ExposureTimeModeType.TimeAndCount
        val alienVisitorState             = alienVisitor.get

        val visitorMode: Option[VisitorObservingModeType] = alienVisitorState.site.map:
          case Site.GN => VisitorObservingModeType.VisitorNorth
          case Site.GS => VisitorObservingModeType.VisitorSouth

        // for alien visitors mode we get the config from a temporary state until saved.
        val alienVisitorConfig: Option[BasicConfiguration.Visitor] =
          (visitorMode, alienVisitorState.centralWavelength, alienVisitorState.scienceFov).mapN:
            (mode, cw, fov) => BasicConfiguration.Visitor(mode, CentralWavelength(cw), fov)

        val alienInput: Option[ObservingModeInput] =
          (alienVisitorConfig, alienVisitorState.name, alienVisitorState.totalRequestTime).mapN:
            (visitor, name, totalTime) =>
              ObservingModeInput.Visitor:
                VisitorInput(
                  mode = visitor.mode.assign,
                  centralWavelength = visitor.centralWavelength.value.toInput.assign,
                  scienceFov = visitor.scienceFov.toInput.assign,
                  name = name.assign,
                  totalRequestTime = totalTime.toInput.assign
                )

        // For spec/imaging, the configuration comes from the table selection.
        val selectedBasicConfig: Option[BasicConfiguration] =
          props.selectedConfig.get.toBasicConfiguration()

        val canAccept: Boolean =
          if isAlienVisitor then alienInput.isDefined
          else props.selectedConfig.get.canAccept(etm) && visitorEtmOk

        val acceptAction: IO[Unit] =
          if isAlienVisitor then
            (alienInput, alienVisitorConfig)
              .mapN: (input, bc) =>
                props.createConfig(input, bc.obsModeType.defaultPosAngleOptions)
              .getOrElse(IO.unit)
          else
            selectedBasicConfig
              .map(bc => props.createConfig(bc.toInput, bc.obsModeType.defaultPosAngleOptions))
              .getOrElse(IO.unit)

        val spectroscopyView: ViewOpt[Spectroscopy] = props.requirementsView
          .zoom(ScienceRequirements.spectroscopy)

        val imagingView: ViewOpt[Imaging] = props.requirementsView
          .zoom(ScienceRequirements.imaging)

        val exposureTimeView = props.requirementsView
          .zoom(ScienceRequirements.exposureTimeMode)

        val isNotTimeAndCount =
          exposureTimeModeType.get =!= ExposureTimeModeType.TimeAndCount

        // wavelength has to be handled special for spectroscopy because you can't select a row without a wavelength.
        val message: Option[String] =
          if (isAlienVisitor) none
          else if (spectroscopyView.get.exists(_.wavelength.isEmpty))
            "Wavelength is required for creating a configuration.".some
          else if (
            props.selectedConfig.get.hasItcErrors || props.selectedConfig.get.isMissingSomeItc
          )
            "ITC issues must be fixed.".some
          else if (props.selectedConfig.get.hasPendingItc)
            "Waiting for ITC result...".some
          else if (props.selectedConfig.get.isEmpty)
            "To create a configuration, select a table row.".some
          else if (props.selectedConfig.get.isVisitor && isNotTimeAndCount)
            "Use Time and Count mode for Visitor instruments.".some
          else none

        def switchMode(modeType: ConfigurationKind): Callback =
          modeType match
            case ConfigurationKind.Spectroscopy =>
              props.requirementsView
                .zoom(ScienceRequirements.scienceMode)
                .set(ScienceRequirements.Spectroscopy.Default.asLeft)
            case ConfigurationKind.Imaging      =>
              props.requirementsView
                .zoom(ScienceRequirements.scienceMode)
                .set(ScienceRequirements.Imaging.Default.asRight)
            case ConfigurationKind.Visitor      =>
              Callback.empty

        val buttonIcon: FontAwesomeIcon =
          if (creating.get.value) Icons.Spinner.withSpin(true)
          else LucumaIcons.Gears

        val modeDropdown: VdomNode =
          FormEnumDropdownView(
            id = "configuration-mode".refined,
            label = React.Fragment("Mode", HelpIcon("configuration/mode.md".refined)),
            value = scienceModeType.withOnMod(switchMode),
            disabled = props.readonly
          )

        <.div(
          ExploreStyles.BasicConfigurationGrid,
          ExploreStyles.BasicConfigurationGridVisitor.when(isAlienVisitor)
        )(
          if isAlienVisitor then
            <.div(ExploreStyles.VisitorBasicArea)(
              <.div(LucumaPrimeStyles.FormColumnCompact)(modeDropdown),
              AlienVisitorConfigEditor(
                state = alienVisitor,
                readonly = props.readonly,
                units = props.units
              )
            )
          else
            React.Fragment(
              <.div(
                ExploreStyles.BasicConfigurationForm,
                modeDropdown,
                spectroscopyView.mapValue: s =>
                  SpectroscopyConfigurationPanel(
                    props.selectedConfig.get.headOption.map(_.instrument),
                    exposureTimeView,
                    exposureTimeModeType,
                    s,
                    props.readonly,
                    props.units,
                    props.calibrationRole
                  ),
                imagingView.mapValue: s =>
                  ImagingConfigurationPanel(
                    props.selectedConfig.get.headOption.map(_.instrument),
                    exposureTimeView,
                    exposureTimeModeType,
                    s,
                    imagingCap,
                    props.readonly,
                    props.units,
                    props.calibrationRole
                  )
              ),
              spectroscopyView
                .mapValue(s =>
                  SpectroscopyModesTable(
                    props.userId,
                    props.selectedConfig,
                    exposureTimeView.get,
                    s.get,
                    props.constraints,
                    props.itcTargets,
                    props.baseCoordinates,
                    props.confMatrix.spectroscopy,
                    props.customSedTimestamps,
                    props.units
                  )
                ),
              imagingView.mapValue(s =>
                ImagingModesTable(
                  props.userId,
                  props.selectedConfig,
                  exposureTimeView.get,
                  s.get,
                  props.confMatrix.imaging,
                  props.constraints,
                  props.itcTargets,
                  props.baseCoordinates,
                  props.customSedTimestamps,
                  props.units,
                  props.targetView,
                  imagingCap.get
                )
              )
            )
          ,
          <.div(ExploreStyles.BasicConfigurationButtons)(
            message.map(Tag(_, severity = Tag.Severity.Success)),
            Button(
              "Accept Configuration",
              icon = buttonIcon,
              disabled = creating.get.value || !canAccept,
              severity = Button.Severity.Primary,
              onClick = acceptAction.switching(creating.async, Creating(_)).runAsync
            ).compact.small.when(isAlienVisitor || canAccept)
          ).unless(props.readonly)
        )

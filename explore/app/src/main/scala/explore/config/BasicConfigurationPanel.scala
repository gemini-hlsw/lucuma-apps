// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.data.EitherNec
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import explore.Icons
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Observation
import explore.model.ScienceRequirements
import explore.model.TimeAndCountModeInfo
import explore.model.ScienceRequirements.Imaging
import explore.model.ScienceRequirements.Spectroscopy
import explore.model.display.given
import explore.model.enums.ExposureTimeModeType
import explore.model.enums.ConfigurationMode
import explore.model.enums.WavelengthUnits
import explore.model.itc.ItcTarget
import explore.model.itc.ItcTargetProblem
import explore.modes.ConfigSelection
import explore.modes.ScienceModes
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ImagingCapability
import lucuma.core.enums.ScienceMode
import lucuma.core.enums.Site
import lucuma.core.enums.VisitorObservingModeType
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.User
import lucuma.core.util.NewBoolean
import lucuma.core.util.Timestamp
import lucuma.schemas.model.BasicConfiguration
import lucuma.schemas.model.CentralWavelength
import lucuma.react.common.ReactFnProps
import lucuma.react.fa.FontAwesomeIcon
import lucuma.react.primereact.Button
import lucuma.react.primereact.Tag
import lucuma.refined.*
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
  createConfig:        IO[Unit],
  createVisitorConfig: (BasicConfiguration.Visitor, ExposureTimeMode.TimeAndCountMode) => IO[Unit],
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
        scienceModeType      <-
          useStateView[ConfigurationMode](ConfigurationMode.Spectroscopy)
        _                    <- useEffectWithDeps(props.requirementsView.get.scienceModeType): modeType =>
                                  scienceModeType.mod: current =>
                                    if current === ConfigurationMode.Visitor then current
                                    else ConfigurationMode.fromScienceMode(modeType)
        creating             <- useStateView(Creating(false))
        imagingCap           <- useStateView(none[ImagingCapability])
        exposureTimeModeType <- useStateView(
                                  props.requirementsView.get.exposureTimeMode
                                    .map(ExposureTimeModeType.fromExposureTimeMode)
                                    .getOrElse(ExposureTimeModeType.SignalToNoise)
                                )
        // Local preview state for alien visitor mode; only persisted on Accept.
        visitorSite          <- useStateView(none[Site])
        visitorCw            <- useStateView(none[Wavelength])
        visitorFov           <- useStateView(none[Angle])
        visitorTimeAndCount  <- useStateView(TimeAndCountModeInfo(none, none, none))
      yield
        import ctx.given

        val etm: Option[ExposureTimeMode] = props.requirementsView.get.exposureTimeMode
        val isVisitor                     = props.selectedConfig.get.isVisitor
        val isAlienVisitorMode            = scienceModeType.get === ConfigurationMode.Visitor
        val visitorEtmOk                  =
          !isVisitor || exposureTimeModeType.get === ExposureTimeModeType.TimeAndCount
        val visitorMode: Option[VisitorObservingModeType] = visitorSite.get.map:
          case Site.GN => VisitorObservingModeType.VisitorNorth
          case Site.GS => VisitorObservingModeType.VisitorSouth

        // Alien-visitor Accept requires all five user-entered fields to be present.
        val visitorAcceptPayload: Option[(BasicConfiguration.Visitor, ExposureTimeMode.TimeAndCountMode)] =
          (visitorMode,
           visitorCw.get,
           visitorFov.get,
           visitorTimeAndCount.get.time,
           visitorTimeAndCount.get.count
          ).mapN: (mode, cw, fov, time, count) =>
            val visitor = BasicConfiguration.Visitor(mode, CentralWavelength(cw), fov)
            val tcMode  = ExposureTimeMode.TimeAndCountMode(time, count, cw)
            (visitor, tcMode)

        val canAccept: Boolean =
          if isAlienVisitorMode then visitorAcceptPayload.isDefined
          else props.selectedConfig.get.canAccept(etm) && visitorEtmOk

        val acceptAction: IO[Unit] =
          if isAlienVisitorMode then
            visitorAcceptPayload.fold(IO.unit)(props.createVisitorConfig.tupled)
          else props.createConfig

        val spectroscopyView: ViewOpt[Spectroscopy] = props.requirementsView
          .zoom(ScienceRequirements.spectroscopy)

        val imagingView: ViewOpt[Imaging] = props.requirementsView
          .zoom(ScienceRequirements.imaging)

        val exposureTimeView = props.requirementsView
          .zoom(ScienceRequirements.exposureTimeMode)

        val isNotTimeAndCount =
          exposureTimeModeType.get =!= ExposureTimeModeType.TimeAndCount

        // wavelength has to be handled special for spectroscopy because you can't select a row without a wavelength.
        // The spectroscopy/ITC/table checks don't apply in alien-visitor mode — that flow has its own local fields.
        val message: Option[String] =
          if (isAlienVisitorMode) none
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

        def switchMode(modeType: ConfigurationMode): Callback =
          modeType match
            case ConfigurationMode.Spectroscopy =>
              props.requirementsView
                .zoom(ScienceRequirements.scienceMode)
                .set(ScienceRequirements.Spectroscopy.Default.asLeft)
            case ConfigurationMode.Imaging      =>
              props.requirementsView
                .zoom(ScienceRequirements.scienceMode)
                .set(ScienceRequirements.Imaging.Default.asRight)
            case ConfigurationMode.Visitor      =>
              // Visitor requires Time & Count ETM, but we can't install a placeholder:
              // the server rejects `time = 0`. Leave the current ETM alone; the user
              // must enter a positive time in the Visitor editor before accepting.
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
          ExploreStyles.BasicConfigurationGridVisitor.when(isAlienVisitorMode)
        )(
          if isAlienVisitorMode then
            <.div(ExploreStyles.VisitorBasicArea)(
              <.div(LucumaPrimeStyles.FormColumnCompact)(modeDropdown),
              AlienVisitorConfigEditor(
                site = visitorSite,
                centralWavelength = visitorCw,
                scienceFov = visitorFov,
                timeAndCount = visitorTimeAndCount,
                calibrationRole = props.calibrationRole,
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
            ),
          <.div(ExploreStyles.BasicConfigurationButtons)(
            message.map(Tag(_, severity = Tag.Severity.Success)),
            Button(
              "Accept Configuration",
              icon = buttonIcon,
              disabled = creating.get.value || !canAccept,
              severity = Button.Severity.Primary,
              onClick = acceptAction.switching(creating.async, Creating(_)).runAsync
            ).compact.small.when(canAccept || isAlienVisitorMode)
          ).unless(props.readonly)
        )

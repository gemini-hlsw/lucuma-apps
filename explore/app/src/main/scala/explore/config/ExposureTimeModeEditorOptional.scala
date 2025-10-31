// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import crystal.react.View
import crystal.react.hooks.*
import eu.timepit.refined.types.numeric.PosInt
import explore.components.HelpIcon
import explore.model.SignalToNoiseModeInfo
import explore.model.TimeAndCountModeInfo
import explore.model.enums.ExposureTimeModeType
import explore.model.enums.ExposureTimeModeType.*
import explore.model.enums.WavelengthUnits
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Instrument
import lucuma.core.enums.ScienceMode
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given

case class ExposureTimeModeEditorOptional(
  instrument:       Option[Instrument],
  wavelength:       Option[Wavelength],
  exposureTimeMode: View[Option[ExposureTimeMode]],
  scienceMode:      ScienceMode,
  readonly:         Boolean,
  units:            WavelengthUnits,
  calibrationRole:  Option[CalibrationRole],
  forceCount:       Option[PosInt] = None
) extends ReactFnProps[ExposureTimeModeEditorOptional](ExposureTimeModeEditorOptional.component)

object ExposureTimeModeEditorOptional:
  private type Props = ExposureTimeModeEditorOptional

  protected val component =
    ScalaFnComponent[Props]: props =>
      for
        exposureTimeModeView <- useStateView(
                                  props.exposureTimeMode.get
                                    .map(_.modeType)
                                    .getOrElse(ExposureTimeModeType.SignalToNoise)
                                )
        signalToNoiseView    <- useStateView(
                                  props.exposureTimeMode.get
                                    .flatMap(SignalToNoiseModeInfo.fromModel)
                                    .getOrElse(SignalToNoiseModeInfo.default(props.scienceMode))
                                )
        timeAndCountView     <-
          useStateView(
            props.exposureTimeMode.get
              .flatMap(TimeAndCountModeInfo.fromModel)
              .getOrElse(TimeAndCountModeInfo.default(props.scienceMode, props.forceCount))
          )
        _                    <- useEffectWithDeps((props.exposureTimeMode.get, props.forceCount)): (exp, force) =>
                                  // Exposure time mode updated upstream
                                  exp
                                    .map: etm =>
                                      SignalToNoiseModeInfo.fromModel(etm).traverse(signalToNoiseView.set) *>
                                        TimeAndCountModeInfo.fromModel(etm).traverse(timeAndCountView.set) *>
                                        exposureTimeModeView.set(etm.modeType)
                                    .getOrElse:
                                      signalToNoiseView.set(SignalToNoiseModeInfo.default(props.scienceMode)) *>
                                        timeAndCountView.set(TimeAndCountModeInfo.default(props.scienceMode, force)) *>
                                        exposureTimeModeView.set(ExposureTimeModeType.SignalToNoise)
        _                    <- useEffectWithDeps(props.wavelength):
                                  // Wavelength updated upstream, set `at` if empty
                                  _.map: wv =>
                                    exposureTimeModeView.get match
                                      case ExposureTimeModeType.SignalToNoise =>
                                        signalToNoiseView.set(signalToNoiseView.get.withRequirementsWavelength(wv))
                                      case ExposureTimeModeType.TimeAndCount  =>
                                        timeAndCountView.set(timeAndCountView.get.withRequirementsWavelength(wv))
                                  .getOrEmpty
      yield

        val snModeView: View[SignalToNoiseModeInfo] = signalToNoiseView.withOnMod:
          case SignalToNoiseModeInfo(Some(value), Some(at)) =>
            props.exposureTimeMode.set(ExposureTimeMode.SignalToNoiseMode(value, at).some)
          case _                                            =>
            Callback.empty

        val tcModeView: View[TimeAndCountModeInfo] = timeAndCountView.withOnMod:
          case TimeAndCountModeInfo(Some(time), Some(count), Some(at)) =>
            props.exposureTimeMode.set(ExposureTimeMode.TimeAndCountMode(time, count, at).some)
          case _                                                       =>
            Callback.empty

        React.Fragment(
          FormEnumDropdownView(
            id = "exposureMode".refined,
            value = exposureTimeModeView,
            label = ReactFragment(
              "Exposure Mode",
              HelpIcon("configuration/exposure-mode.md".refined)
            ),
            onChangeE = (v, _) =>
              v match
                case Some(ExposureTimeModeType.SignalToNoise) =>
                  timeAndCountView.get match
                    case TimeAndCountModeInfo(_, _, Some(at)) =>
                      snModeView.mod:
                        case s @ SignalToNoiseModeInfo(_, None) => s.copy(at = Some(at))
                        case s                                  => s
                    case _                                    => Callback.empty
                case Some(ExposureTimeModeType.TimeAndCount)  =>
                  signalToNoiseView.get match
                    case SignalToNoiseModeInfo(_, Some(at)) =>
                      tcModeView.mod:
                        case s @ TimeAndCountModeInfo(_, _, None) => s.copy(at = Some(at))
                        case s                                    => s
                    case _                                  => Callback.empty
                case None                                     => Callback.empty
            ,
            disabled = props.readonly
          ),
          if (exposureTimeModeView.get === ExposureTimeModeType.SignalToNoise)
            SignalToNoiseAtEditor(snModeView,
                                  props.scienceMode,
                                  props.readonly,
                                  props.units,
                                  props.calibrationRole
            )
          else
            TimeAndCountEditor(props.instrument,
                               tcModeView,
                               props.scienceMode,
                               props.readonly,
                               props.units,
                               props.calibrationRole,
                               showCount = props.forceCount.isEmpty
            )
        )

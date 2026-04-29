// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.ui.ExploreStyles
import explore.itc.renderRequiredForITCIcon
import explore.model.Constants
import explore.model.TimeAndCountModeInfo
import explore.model.enums.WavelengthUnits
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Instrument
import lucuma.core.enums.ScienceMode
import lucuma.react.common.ReactFnProps
import lucuma.react.common.style.Css
import lucuma.refined.*
import lucuma.ui.primereact.FormInputTextView
import lucuma.ui.primereact.clearable
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given

case class TimeAndCountEditor(
  instrument:      Option[Instrument],
  options:         View[TimeAndCountModeInfo],
  scienceMode:     ScienceMode,
  readonly:        Boolean,
  units:           WavelengthUnits,
  calibrationRole: Option[CalibrationRole],
  showCount:       Boolean,
  makeId:          NonEmptyString => NonEmptyString,
  labelClass:      Css,
  controlsWrapper: (VdomNode, Css) => VdomNode
) extends ReactFnProps[TimeAndCountEditor](TimeAndCountEditor.component)

object TimeAndCountEditor extends ConfigurationFormats:
  private type Props = TimeAndCountEditor

  protected val component =
    ScalaFnComponent[Props]: props =>
      val signalToNoiseAt = props.options.zoom(TimeAndCountModeInfo.at)

      React.Fragment(
        TimeAndCountFieldsEditor(
          props.instrument,
          props.options,
          props.readonly,
          props.calibrationRole,
          props.showCount,
          props.makeId,
          props.labelClass,
          props.controlsWrapper
        ),
        Option.when(props.scienceMode === ScienceMode.Spectroscopy):
          props.controlsWrapper(
            FormInputTextView(
              id = props.makeId("SignalToNoiseAt".refined),
              label = Constants.SignalToNoiseAtLabel,
              labelClass = props.labelClass,
              groupClass = ExploreStyles.WarningInput.when_(signalToNoiseAt.get.isEmpty),
              postAddons = signalToNoiseAt.get.foldMap(_ =>
                List(props.calibrationRole.renderRequiredForITCIcon)
              ),
              value = signalToNoiseAt,
              units = props.units.symbol,
              validFormat = props.units.toInputWedge,
              changeAuditor = props.units.toSNAuditor,
              disabled = props.readonly
            ).clearable(^.autoComplete.off),
            ExploreStyles.ExposureTimeModeAt
          )
      )

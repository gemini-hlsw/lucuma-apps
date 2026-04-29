// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import crystal.react.View
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.ui.ExploreStyles
import explore.model.TimeAndCountModeInfo
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.Instrument
import lucuma.core.model.ExposureTimeMode
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.common.style.Css

final case class TimeAndCountModeEditor(
  instrument:      Option[Instrument],
  value:           View[ExposureTimeMode.TimeAndCountMode],
  readonly:        Boolean,
  calibrationRole: Option[CalibrationRole],
  idPrefix:        NonEmptyString,
  showCount:       Boolean = true,
  forGridRow:      Boolean = false
) extends ReactFnProps(TimeAndCountModeEditor):
  def tci: TimeAndCountModeInfo =
    TimeAndCountModeInfo(value.get.time.some, value.get.count.some, value.get.at.some)

object TimeAndCountModeEditor
    extends ReactFnComponent[TimeAndCountModeEditor](props =>
      for
        local <- useStateView(props.tci)
        _     <- useEffectWithDeps(props.tci)(local.set)
      yield
        val info: View[TimeAndCountModeInfo] = local.withOnMod:
          case TimeAndCountModeInfo(Some(t), Some(c), Some(a)) =>
            props.value.set(ExposureTimeMode.TimeAndCountMode(t, c, a))
          case _                                               => Callback.empty

        val controlsWrapper: (VdomNode, Css) => VdomNode = (node, clazz) =>
          if props.forGridRow then <.span(node, clazz) else node

        TimeAndCountFieldsEditor(
          props.instrument,
          info,
          props.readonly,
          props.calibrationRole,
          props.showCount,
          base => NonEmptyString.unsafeFrom(s"${props.idPrefix}$base"),
          ExploreStyles.HiddenLabel.when_(props.forGridRow),
          controlsWrapper
        )
    )

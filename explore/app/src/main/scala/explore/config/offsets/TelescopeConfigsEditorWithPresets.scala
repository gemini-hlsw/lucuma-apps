// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.offsets

import cats.data.NonEmptyList
import cats.syntax.all.*
import crystal.react.*
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.react.common.*
import lucuma.react.primereact.SelectButtonOptional
import lucuma.react.primereact.SelectItem
import lucuma.refined.*
import lucuma.ui.primereact.*

final case class TelescopeConfigsEditorWithPresets(
  telescopeConfigs: View[NonEmptyList[TelescopeConfig]],
  pEnabled:         Boolean = true,
  maxOffsets:       Int = Int.MaxValue,
  presets:          List[(String, NonEmptyList[TelescopeConfig])],
  readonly:         Boolean = false
) extends ReactFnProps(TelescopeConfigsEditorWithPresets)

object TelescopeConfigsEditorWithPresets
    extends ReactFnComponent[TelescopeConfigsEditorWithPresets](props =>
      val selected: Option[String] =
        props.presets.find(_._2 === props.telescopeConfigs.get).map(_._1)

      React.Fragment(
        <.span(ExploreStyles.SlitTelescopeConfigEditorHeader)(
          FormLabel(htmlFor = "offset-presets".refined)(
            "Spatial Offsets",
            HelpIcon("configuration/ifu-spatial-offsets.md".refined)
          ),
          SelectButtonOptional(
            id = "offset-presets",
            value = selected,
            options = props.presets.map((name, _) => SelectItem(name, name)),
            onChange = (name: Option[String]) =>
              name
                .flatMap(n => props.presets.find(_._1 === n).map(_._2))
                .foldMap(props.telescopeConfigs.set),
            disabled = props.readonly
          ).veryCompact.mini
        ),
        TelescopeConfigsEditor(
          props.telescopeConfigs,
          props.pEnabled,
          props.maxOffsets,
          props.readonly
        )
      )
    )

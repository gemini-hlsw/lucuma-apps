// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.offsets

import cats.data.NonEmptyList
import cats.syntax.all.*
import crystal.react.View
import explore.components.CustomizedGroupAddon
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model.display.given
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.syntax.display.*
import lucuma.core.util.Enumerated
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.SelectItem
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given

final case class SlitTelescopeConfigsEditor(
  explicitValue:   View[Option[SlitTelescopeConfigs]],
  defaultValue:    SlitTelescopeConfigs,
  defaultForMode:  SlitOffsetMode => SlitTelescopeConfigs,
  presetsReadonly: Boolean, // selecting a preset
  editingReadonly: Boolean  // editing the individual offsets.
) extends ReactFnProps(SlitTelescopeConfigsEditor)

object SlitTelescopeConfigsEditor
    extends ReactFnComponent[SlitTelescopeConfigsEditor](props =>
      val value: View[SlitTelescopeConfigs] =
        props.explicitValue.removeOptionality(props.defaultValue)

      val alongSlitView: Option[View[NonEmptyList[TelescopeConfig]]] =
        value
          .zoom(SlitTelescopeConfigs.alongSlit.andThen(SlitTelescopeConfigs.AlongSlit.value))
          .zoom(_.map(_.toTelescopeConfig))(mod =>
            alongSlitTcs =>
              mod(alongSlitTcs.map(_.toTelescopeConfig)).map(_.toTelescopeConfigAlongSlit)
          )
          .toOptionView

      val toSkyView: Option[View[NonEmptyList[TelescopeConfig]]] =
        value
          .zoom(SlitTelescopeConfigs.toSky.andThen(SlitTelescopeConfigs.ToSky.value))
          .toOptionView

      React.Fragment(
        <.span(ExploreStyles.SlitTelescopeConfigEditorHeader)(
          FormDropdown(
            id = "offset-mode".refined,
            value = value.get.offsetsType,
            options = Enumerated[SlitOffsetMode].all.map(t => SelectItem(t, t.shortName)).toList,
            label = React.Fragment(
              "Spatial Offsets",
              HelpIcon("configuration/slit-spatial-offsets.md".refined)
            ),
            onChange = mode => value.set(props.defaultForMode(mode)),
            disabled = props.presetsReadonly
          ),
          CustomizedGroupAddon(
            "default offsets",
            props.explicitValue.set(none),
            allowRevert = true
          ).when(props.explicitValue.get.exists(_ =!= props.defaultValue))
        ),
        alongSlitView.map: alongSlitTelescopeConfigs =>
          TelescopeConfigsEditor(
            telescopeConfigs = alongSlitTelescopeConfigs,
            pEnabled = false,
            readonly = props.editingReadonly
          ),
        toSkyView.map: toSkyTelescopeConfigs =>
          TelescopeConfigsEditor(
            telescopeConfigs = toSkyTelescopeConfigs,
            readonly = props.editingReadonly
          )
      )
    )

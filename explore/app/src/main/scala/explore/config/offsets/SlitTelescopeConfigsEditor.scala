// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.offsets

import cats.data.NonEmptyList
import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.CustomizedGroupAddon
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.SlitOffsetPreset
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given

final case class SlitTelescopeConfigsEditor[P <: SlitOffsetPreset](
  explicitValue:    View[Option[SlitTelescopeConfigs]],
  defaultValue:     SlitTelescopeConfigs,
  defaultForPreset: P => SlitTelescopeConfigs,
  helpId:           NonEmptyString,
  presetsReadonly:  Boolean, // selecting a preset
  editingReadonly:  Boolean  // editing the individual offsets.
)(using val enumerated: Enumerated[P], val display: Display[P])
    extends ReactFnProps(SlitTelescopeConfigsEditor.component)

object SlitTelescopeConfigsEditor:
  private type Preset = SlitOffsetPreset

  private def buildComponent[P <: SlitOffsetPreset] =
    ScalaFnComponent[SlitTelescopeConfigsEditor[P]]: props =>
      import props.given

      val value: View[SlitTelescopeConfigs] =
        props.explicitValue.removeOptionality(props.defaultValue)

      // Exact-match: the active preset is the one whose template equals the
      // effective configs. None means a custom, hand-edited pattern.
      val activePreset: Option[P] =
        Enumerated[P].all.find(p => props.defaultForPreset(p) === value.get)

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
          FormLabel(htmlFor = "offset-mode".refined)(
            "Spatial Offsets",
            HelpIcon(props.helpId)
          ),
          EnumOptionalDropdown(
            id = "offset-mode".refined,
            value = activePreset,
            showClear = false,
            placeholder = "Custom",
            disabled = props.presetsReadonly,
            onChange =
              (p: Option[P]) => p.foldMap(preset => value.set(props.defaultForPreset(preset)))
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

  private val component = buildComponent[Preset]

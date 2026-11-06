// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.offsets

import cats.data.NonEmptyList
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.*
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.schemas.model.TelescopeConfigPresets

final case class PresettableTelescopeConfigsEditor(
  telescopeConfigs: View[NonEmptyList[TelescopeConfig]],
  presets:          TelescopeConfigPresets,
  defaultConfigs:   NonEmptyList[TelescopeConfig],
  helpId:           NonEmptyString,
  presetsReadonly:  Boolean, // selecting a preset
  editingReadonly:  Boolean  // editing the individual offsets.
) extends ReactFnProps(PresettableTelescopeConfigsEditor)

object PresettableTelescopeConfigsEditor
    extends ReactFnComponent[PresettableTelescopeConfigsEditor](props =>
      // Presets are keyed by plain String, since these modes have no slit to derive an offset
      // direction from.

      // Exact-match: the active preset is the one whose pattern equals the
      // current configs; None means a custom, hand-edited pattern.
      val activePreset: Option[String] = props.presets.keyOf(props.telescopeConfigs.get)

      val onSelect: Option[String] => Callback =
        _.flatMap(props.presets.value).fold(Callback.empty)(props.telescopeConfigs.set)

      React.Fragment(
        OffsetPresetsHeader(
          id = "offset-mode".refined,
          helpId = props.helpId,
          presets = props.presets.keys.toList,
          label = identity,
          activePreset = activePreset,
          onSelect = onSelect,
          disabled = props.presetsReadonly,
          showRevert = activePreset.isEmpty,
          onRevert = props.telescopeConfigs.set(props.defaultConfigs)
        ),
        TelescopeConfigsEditor(
          telescopeConfigs = props.telescopeConfigs,
          readonly = props.editingReadonly
        )
      )
    )

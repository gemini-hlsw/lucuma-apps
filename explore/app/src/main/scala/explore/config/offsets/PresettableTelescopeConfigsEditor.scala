// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.offsets

import cats.data.NonEmptyList
import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.*
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.refined.*

final case class PresettableTelescopeConfigsEditor(
  telescopeConfigs: View[NonEmptyList[TelescopeConfig]],
  presets:          NonEmptyList[(String, NonEmptyList[TelescopeConfig])],
  defaultConfigs:   NonEmptyList[TelescopeConfig],
  helpId:           NonEmptyString,
  presetsReadonly:  Boolean, // selecting a preset
  editingReadonly:  Boolean  // editing the individual offsets.
) extends ReactFnProps(PresettableTelescopeConfigsEditor)

object PresettableTelescopeConfigsEditor
    extends ReactFnComponent[PresettableTelescopeConfigsEditor](props =>
      // Presets arrive as (name, pattern) pairs from core, keyed by plain String,
      // since these modes have no slit to derive an offset direction from.

      // Exact-match: the active preset is the one whose pattern equals the
      // current configs; None means a custom, hand-edited pattern.
      val activePreset: Option[String] =
        props.presets.find(_._2 === props.telescopeConfigs.get).map(_._1)

      val onSelect: Option[String] => Callback =
        _.flatMap(name => props.presets.find(_._1 === name).map(_._2))
          .fold(Callback.empty)(props.telescopeConfigs.set)

      React.Fragment(
        OffsetPresetsHeader(
          id = "offset-mode".refined,
          helpId = props.helpId,
          presets = props.presets.map(_._1).toList,
          label = identity,
          activePreset = activePreset,
          onSelect = onSelect,
          disabled = props.presetsReadonly,
          showRevert = props.telescopeConfigs.get =!= props.defaultConfigs,
          onRevert = props.telescopeConfigs.set(props.defaultConfigs)
        ),
        TelescopeConfigsEditor(
          telescopeConfigs = props.telescopeConfigs,
          readonly = props.editingReadonly
        )
      )
    )

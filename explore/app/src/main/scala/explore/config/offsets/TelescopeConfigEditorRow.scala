// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.offsets

import cats.syntax.all.*
import crystal.react.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.StepGuideState
import lucuma.core.math.*
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.react.common.*
import lucuma.react.primereact.Button
import lucuma.react.primereact.PrimeStyles
import lucuma.react.primereact.ToggleButton
import lucuma.ui.primereact.*
import lucuma.ui.sequence.SequenceIcons
import lucuma.ui.sequence.SequenceStyles

final case class TelescopeConfigEditorRow(
  telescopeConfig: View[TelescopeConfig],
  idx:             Int,
  pEnabled:        Boolean,
  remove:          Callback,
  readonly:        Boolean
) extends ReactFnProps(TelescopeConfigEditorRow):
  val isRemoveHidden: Boolean = idx === 0

object TelescopeConfigEditorRow
    extends ReactFnComponent[TelescopeConfigEditorRow](props =>
      val offset: View[Offset]          = props.telescopeConfig.zoom(TelescopeConfig.offset)
      val guiding: View[StepGuideState] = props.telescopeConfig.zoom(TelescopeConfig.guiding)

      extension (sgs: StepGuideState.type)
        def fromBoolean: Boolean => StepGuideState =
          case false => StepGuideState.Disabled
          case true  => StepGuideState.Enabled

      // maybe rename style
      <.div(OffsetGeneratorEditorStyles.ExplicitRow)(
        OffsetInput(
          id = NonEmptyString.unsafeFrom(s"explicit-offsets-${props.idx}"),
          offset = offset,
          pEnabled = props.pEnabled,
          readonly = props.readonly,
          inputClass = LucumaPrimeStyles.FormField
        ),
        ToggleButton(
          onIcon = SequenceIcons.Crosshairs.addClass(SequenceStyles.StepGuided),
          offIcon = SequenceIcons.Crosshairs.addClass(
            OffsetGeneratorEditorStyles.ExplicitUnguided
          ),
          onLabel = "",
          offLabel = "",
          tooltip = "Toggle Guiding",
          disabled = props.readonly,
          text = true,
          clazz = LucumaPrimeStyles.FormField |+| PrimeStyles.ButtonIconOnly |+|
            OffsetGeneratorEditorStyles.ToggleButton,
          checked = guiding.get === StepGuideState.Enabled,
          onChange = b => guiding.set(StepGuideState.fromBoolean(b))
        ).mini.compact,
        Button(
          icon = Icons.Trash.addClass(
            OffsetGeneratorEditorStyles.RemoveOffsetHidden.when_(props.isRemoveHidden)
          ),
          tooltip = "Remove Offset",
          disabled = props.readonly || props.isRemoveHidden,
          text = true,
          clazz = LucumaPrimeStyles.FormField,
          onClick = props.remove
        ).mini.compact
      )
    )

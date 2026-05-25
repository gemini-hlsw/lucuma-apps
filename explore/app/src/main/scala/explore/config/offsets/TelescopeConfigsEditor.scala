// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.offsets

import cats.data.NonEmptyList
import cats.syntax.all.*
import crystal.react.*
import explore.Icons
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.react.common.*
import lucuma.react.primereact.Button
import lucuma.ui.primereact.*
import lucuma.ui.utils.*

final case class TelescopeConfigsEditor(
  telescopeConfigs: View[NonEmptyList[TelescopeConfig]],
  pEnabled:         Boolean = true,
  maxOffsets:       Int = Int.MaxValue,
  readonly:         Boolean = false
) extends ReactFnProps(TelescopeConfigsEditor)

object TelescopeConfigsEditor
    extends ReactFnComponent[TelescopeConfigsEditor](props =>
      React.Fragment(
        props.telescopeConfigs.toNelOfViews.zipWithIndex
          .map: (telescopeConfig, idx) =>
            React.Fragment.withKey(s"explicit-offsets-row-$idx")(
              <.label(^.htmlFor := s"explicit-offsets-$idx", s"Offset ${idx + 1} (arcsec):"),
              TelescopeConfigEditorRow(
                telescopeConfig = telescopeConfig,
                idx = idx,
                pEnabled = props.pEnabled,
                remove = props.telescopeConfigs.mod: configs =>
                  NonEmptyList
                    .fromList:
                      configs.take(idx) ++ configs.toList.drop(idx + 1)
                    .getOrElse(NonEmptyList.one(TelescopeConfig.Default)),
                readonly = props.readonly
              )
            )
          .toList
          .toVdomArray,
        Button(
          icon = Icons.ThinPlus,
          severity = Button.Severity.Success,
          disabled = props.readonly || props.telescopeConfigs.get.length >= props.maxOffsets,
          tooltip = "Add Offset",
          text = true,
          clazz = LucumaPrimeStyles.FormField |+| OffsetGeneratorEditorStyles.AddOffset,
          onClick = props.telescopeConfigs.mod(_.append(TelescopeConfig.Default))
        ).mini.compact
      )
    )

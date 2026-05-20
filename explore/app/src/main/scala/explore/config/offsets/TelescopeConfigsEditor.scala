// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.offsets

import cats.data.NonEmptyList
import crystal.react.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.react.common.*
import lucuma.ui.utils.*

final case class TelescopeConfigsEditor(
  telescopeConfigs: View[NonEmptyList[TelescopeConfig]],
  readonly:         Boolean
) extends ReactFnProps(TelescopeConfigsEditor)

object TelescopeConfigsEditor
    extends ReactFnComponent[TelescopeConfigsEditor](props =>
      props.telescopeConfigs.toNelOfViews.zipWithIndex
        .map: (telescopeConfig, idx) =>
          React.Fragment.withKey(s"explicit-offsets-row-$idx")(
            <.label(^.htmlFor := s"explicit-offsets-$idx", s"Offset ${idx + 1} (arcsec):"),
            TelescopeConfigEditorRow(
              telescopeConfig = telescopeConfig,
              idx = idx,
              remove = props.telescopeConfigs.mod: configs =>
                NonEmptyList
                  .fromList:
                    configs.take(idx) ++ configs.toList.drop(idx + 1)
                  .getOrElse(NonEmptyList.one(TelescopeConfig.Default)),
              readonly = props.readonly
            )
          )
        .toList
        .toVdomArray
    )

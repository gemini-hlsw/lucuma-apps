// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.offsets

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import clue.data.Input
import clue.data.syntax.*
import crystal.react.*
import crystal.react.View
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.common.Aligner
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.config.offsets.OffsetInput
import explore.model.AppContext
import explore.model.Observation
import explore.model.display.given
import explore.model.enums.WavelengthUnits
import explore.modes.SpectroscopyModesMatrix
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.*
import lucuma.core.math.Offset
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Program
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.TelescopeConfigAlongSlit
import lucuma.core.syntax.display.*
import lucuma.core.util.Enumerated
import lucuma.react.common.*
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.SelectItem
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.*
import lucuma.ui.utils.toNelOfViews

final case class SlitTelescopeConfigsEditor(
  explicitValue:  View[Option[SlitTelescopeConfigs]],
  defaultValue:   SlitTelescopeConfigs,
  defaultForMode: SlitOffsetMode => SlitTelescopeConfigs,
  readonly:       Boolean
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

      // TODO REVERT TO DEFAULT

      React.Fragment(
        FormDropdown(
          id = "offset-mode".refined,
          value = value.get.offsetsType,
          options = Enumerated[SlitOffsetMode].all.map(t => SelectItem(t, t.shortName)).toList,
          label = "Spatial Offsets".some,
          onChange = mode => value.set(props.defaultForMode(mode)),
          disabled = props.readonly
        ),
        alongSlitView.map: alongSlitTelescopeConfigs =>
          TelescopeConfigsEditor(
            telescopeConfigs = alongSlitTelescopeConfigs,
            pEnabled = false,
            readonly = props.readonly
          ),
        toSkyView.map: toSkyTelescopeConfigs =>
          TelescopeConfigsEditor(
            telescopeConfigs = toSkyTelescopeConfigs,
            readonly = props.readonly
          )
      )
    )

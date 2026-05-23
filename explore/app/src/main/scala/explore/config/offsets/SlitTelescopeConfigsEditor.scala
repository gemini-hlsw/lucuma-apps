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
import lucuma.core.model.sequence.igrins2.CentralWavelength as Igrins2CentralWavelength
import lucuma.core.model.sequence.igrins2.defaultOffsetsFor
import lucuma.react.common.*
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
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
  slitTelescopeConfigs: View[SlitTelescopeConfigs],
  readonly:             Boolean
) extends ReactFnProps(SlitTelescopeConfigsEditor)

object SlitTelescopeConfigsEditor
    extends ReactFnComponent[SlitTelescopeConfigsEditor](props =>
      React.Fragment(
        // CustomizableEnumSelectOptional(
        //   id = "offset-mode".refined,
        //   view = offsetModeView.withDefault(defaultOffsetMode),
        //   defaultValue = defaultOffsetMode.some,
        //   label = "Offset Mode".some,
        //   disabled = disableEdit,
        //   showCustomization = showCustomization,
        //   allowRevertCustomization = allowRevertCustomization,
        //   resetToOriginal = true,
        //   helpId = Some("configuration/igrins2/offset-mode.md".refined)
        // ),
        // TelescopeConfigsEditor(
        //   telescopeConfigs = props.slitTelescopeConfigs.zoom(SlitTelescopeConfigs.telescopeConfigs),
        //   readonly = props.readonly
        // )
      )
    )

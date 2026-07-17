// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.data.NonEmptyList
import cats.effect.IO
import crystal.react.*
import crystal.react.hooks.*
import explore.common.Aligner
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*
import lucuma.ui.display.given
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given

import java.util.concurrent.TimeUnit

// The exchange mode has no optional fields, so the editor works directly on the
// mode value and is reused for both creation and editing.
case class KeckExchangeConfigEditor(
  mode:     View[ObservingMode.KeckExchange],
  readonly: Boolean
) extends ReactFnProps(KeckExchangeConfigEditor)

object KeckExchangeConfigEditor
    extends ReactFnComponent[KeckExchangeConfigEditor](props =>
      <.div(
        ExploreStyles.VisitorUpperGrid,
        LucumaPrimeStyles.FormColumnCompact,
        FormEnumDropdownView(
          id = "keck-instrument".refined,
          value = props.mode.zoom(ObservingMode.KeckExchange.keckInstrument),
          label = "Instrument",
          disabled = props.readonly
        ),
        FormTimeSpanInput(
          id = "keck-total-time".refined,
          value = props.mode.zoom(ObservingMode.KeckExchange.totalRequestTime),
          units = NonEmptyList.of(TimeUnit.HOURS, TimeUnit.MINUTES),
          label = "Total Requested Time",
          disabled = props.readonly
        )
      )
    )

// Editing panel for a persisted Keck exchange mode.
case class KeckExchangeConfigPanel(
  observingMode: Aligner[ObservingMode.KeckExchange, ExchangeInput],
  revertConfig:  IO[Unit],
  permissions:   ConfigEditPermissions
) extends ReactFnProps(KeckExchangeConfigPanel)

object KeckExchangeConfigPanel
    extends ReactFnComponent[KeckExchangeConfigPanel](props =>
      for
        ctx       <- useContext(AppContext.ctx)
        editState <- useStateView(ConfigEditState.View)
      yield
        import ctx.given

        // Send both fields on every update.
        val modeView: View[ObservingMode.KeckExchange] =
          props.observingMode.view(_.toInput)

        React.Fragment(
          KeckExchangeConfigEditor(modeView, !props.permissions.isFullEdit),
          <.div(
            ExploreStyles.VisitorLowerGrid,
            AdvancedConfigButtons(
              editState = editState,
              isCustomized = props.observingMode.get.isCustomized,
              revertConfig = props.revertConfig,
              revertCustomizations = Callback.empty,
              sequenceChanged = Callback.empty,
              readonly = !props.permissions.isFullEdit,
              showAdvancedButton = false,
              showCustomizeButton = false
            )
          )
        )
    )

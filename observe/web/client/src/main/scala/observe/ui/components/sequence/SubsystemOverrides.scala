// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.components.sequence

import crystal.react.*
import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Instrument
import lucuma.core.model.Observation
import lucuma.react.common.*
import lucuma.react.primereact.Button
import lucuma.react.primereact.Tooltip
import lucuma.react.primereact.tooltip.*
import lucuma.refined.*
import observe.model.Server
import observe.model.SubsystemEnabled
import observe.model.SubsystemOrServer
import observe.model.SystemOverrides
import observe.model.enums.ControlStrategy
import observe.model.enums.Resource
import observe.ui.Icons
import observe.ui.ObserveStyles
import observe.ui.model.AppContext
import observe.ui.services.ConfigApi

import scalajs.js

case class SubsystemOverrides(
  obsId:             Observation.Id,
  instrument:        Instrument,
  overrides:         View[SystemOverrides],
  controlStrategies: Map[SubsystemOrServer, ControlStrategy]
) extends ReactFnProps(SubsystemOverrides)

object SubsystemOverrides
    extends ReactFnComponent[SubsystemOverrides](props =>
      for
        ctx       <- useContext(AppContext.ctx)
        configApi <- useContext(ConfigApi.ctx)
      yield
        import ctx.given

        // Each subsystem renders as a button labeled with the subsystem name, with a
        // checkbox icon indicating its on/off state (emulating a toggle). A checked,
        // controllable button is solid green; unchecked is outlined. Simulated and
        // read-only subsystems are disabled and shown gray (the checkbox icon still
        // reflects their state); simulated ones additionally show a "SIM" badge. Controls
        // with no `onChange` (e.g. GWS, always on) are shown checked and disabled.
        def renderToggle(
          label:           NonEmptyString,
          checked:         Boolean,
          onChange:        js.UndefOr[Boolean => Callback],
          controlStrategy: Option[ControlStrategy]
        ): VdomNode =
          val simulated = controlStrategy.contains(ControlStrategy.Simulated)
          val readOnly  = controlStrategy.contains(ControlStrategy.ReadOnly)
          val disabled  = onChange.isEmpty || simulated || readOnly
          <.span(ObserveStyles.SubsystemToggle)(
            Button(
              label = label.value,
              icon = if (checked) Icons.SquareCheck else Icons.Square,
              severity =
                if (checked && !simulated && !readOnly) Button.Severity.Success
                else Button.Severity.Secondary,
              outlined = !checked,
              disabled = disabled,
              badge = if (simulated) "SIM" else js.undefined,
              onClick = onChange.toOption.fold(Callback.empty)(f => f(!checked))
            )
          ).withTooltip(
            content = if (simulated) "Simulated" else if (readOnly) "Read-only" else "",
            position = Tooltip.Position.Top
          )

        def renderOverrideControl(
          label:           NonEmptyString,
          enabled:         View[SubsystemEnabled],
          controlStrategy: Option[ControlStrategy]
        ): VdomNode =
          val boolView = enabled.as(SubsystemEnabled.Value)
          renderToggle(label, boolView.get, boolView.set, controlStrategy)

        <.span(ObserveStyles.ObsSummarySubsystems)(
          renderOverrideControl(
            "TCS".refined,
            props.overrides
              .zoom(SystemOverrides.isTcsEnabled)
              .withOnMod(configApi.setTcsEnabled(props.obsId, _).runAsync),
            props.controlStrategies.get(Resource.TCS)
          ),
          renderOverrideControl(
            "GCAL".refined,
            props.overrides
              .zoom(SystemOverrides.isGcalEnabled)
              .withOnMod(configApi.setGcalEnabled(props.obsId, _).runAsync),
            props.controlStrategies.get(Resource.Gcal)
          ),
          renderOverrideControl(
            "DHS".refined,
            props.overrides
              .zoom(SystemOverrides.isDhsEnabled)
              .withOnMod(configApi.setDhsEnabled(props.obsId, _).runAsync),
            props.controlStrategies.get(Server.Dhs)
          ),
          renderToggle("GWS".refined, true, js.undefined, props.controlStrategies.get(Server.Gws)),
          renderOverrideControl(
            NonEmptyString.unsafeFrom(props.instrument.longName),
            props.overrides
              .zoom(SystemOverrides.isInstrumentEnabled)
              .withOnMod(configApi.setInstrumentEnabled(props.obsId, _).runAsync),
            props.controlStrategies.get(props.instrument)
          )
        )
    )

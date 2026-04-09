// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.components.sequence

import cats.syntax.all.*
import crystal.*
import crystal.react.*
import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Instrument
import lucuma.core.model.Observation
import lucuma.react.common.*
import lucuma.react.primereact.Tooltip
import lucuma.react.primereact.tooltip.*
import lucuma.refined.*
import lucuma.ui.primereact.{*, given}
import observe.model.Server
import observe.model.SubsystemEnabled
import observe.model.SubsystemOrServer
import observe.model.SystemOverrides
import observe.model.enums.ControlStrategy
import observe.model.enums.Resource
import observe.ui.ObserveStyles
import observe.ui.model.AppContext
import observe.ui.services.ConfigApi

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

        def renderOverrideControl(
          label:           NonEmptyString,
          enabled:         View[SubsystemEnabled],
          controlStrategy: Option[ControlStrategy]
        ): VdomNode =
          <.span(
            CheckboxView(
              id = label,
              value = enabled.as(SubsystemEnabled.Value),
              label = label.value,
              disabled = controlStrategy.contains_(ControlStrategy.Simulated),
              clazz = controlStrategy match
                case Some(ControlStrategy.Simulated) => ObserveStyles.SimulatedSubsystem
                case Some(ControlStrategy.ReadOnly)  => ObserveStyles.ReadonlySubsystem
                case _                               => Css.Empty
            )
          ).withTooltip(
            content = controlStrategy match
              case Some(ControlStrategy.Simulated) => "Simulated"
              case Some(ControlStrategy.ReadOnly)  => "Read-only"
              case _                               => "",
            position = Tooltip.Position.Top
          )

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
          renderOverrideControl(
            NonEmptyString.unsafeFrom(props.instrument.longName),
            props.overrides
              .zoom(SystemOverrides.isInstrumentEnabled)
              .withOnMod(configApi.setInstrumentEnabled(props.obsId, _).runAsync),
            props.controlStrategies.get(props.instrument)
          )
        )
    )

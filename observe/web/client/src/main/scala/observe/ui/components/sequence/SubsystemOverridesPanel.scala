// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.components.sequence

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
import lucuma.ui.primereact.CheckboxView
import lucuma.ui.primereact.given
import observe.model.Subsystem
import observe.model.SubsystemEnabled
import observe.model.SubsystemOverrides
import observe.model.enums.ObservatorySubsystem
import observe.model.enums.SubsystemControlMode
import observe.ui.ObserveStyles
import observe.ui.model.AppContext
import observe.ui.services.ConfigApi

case class SubsystemOverridesPanel(
  obsId:        Observation.Id,
  instrument:   Instrument,
  overrides:    View[SubsystemOverrides],
  controlModes: Map[Subsystem, SubsystemControlMode]
) extends ReactFnProps(SubsystemOverridesPanel)

object SubsystemOverridesPanel
    extends ReactFnComponent[SubsystemOverridesPanel](props =>
      for
        ctx       <- useContext(AppContext.ctx)
        configApi <- useContext(ConfigApi.ctx)
      yield
        import ctx.given

        def renderToggle(
          label:       NonEmptyString,
          enabled:     View[Boolean],
          editable:    Boolean,
          controlMode: Option[SubsystemControlMode]
        ): VdomNode =
          val simulated = controlMode.contains(SubsystemControlMode.Simulated)
          <.div(ObserveStyles.SubsystemToggle)(
            CheckboxView(
              id = NonEmptyString.unsafeFrom(
                s"subsystem-${props.obsId}-${label.value.replaceAll("\\s+", "-")}"
              ),
              value = enabled,
              label = TagMod(
                label.value,
                <.sup(ObserveStyles.SubsystemSimTag)("SIM").when(simulated)
              ),
              disabled = !editable || simulated
            )
          ).withTooltip(
            content = if (simulated) "Simulated" else "",
            position = Tooltip.Position.Top
          )

        def renderOverrideControl(
          label:       NonEmptyString,
          enabled:     View[SubsystemEnabled],
          controlMode: Option[SubsystemControlMode]
        ): VdomNode =
          renderToggle(label, enabled.as(SubsystemEnabled.Value), true, controlMode)

        <.span(ObserveStyles.ObsSummarySubsystems)(
          renderOverrideControl(
            "TCS".refined,
            props.overrides
              .zoom(SubsystemOverrides.isTcsEnabled)
              .withOnMod(configApi.setTcsEnabled(props.obsId, _).runAsync),
            props.controlModes.get(ObservatorySubsystem.Tcs)
          ),
          renderOverrideControl(
            "GCAL".refined,
            props.overrides
              .zoom(SubsystemOverrides.isGcalEnabled)
              .withOnMod(configApi.setGcalEnabled(props.obsId, _).runAsync),
            props.controlModes.get(ObservatorySubsystem.Gcal)
          ),
          renderOverrideControl(
            "DHS".refined,
            props.overrides
              .zoom(SubsystemOverrides.isDhsEnabled)
              .withOnMod(configApi.setDhsEnabled(props.obsId, _).runAsync),
            props.controlModes.get(ObservatorySubsystem.Dhs)
          ),
          renderToggle(
            "GWS".refined,
            View[Boolean](true, (_, _) => Callback.empty),
            false,
            props.controlModes.get(ObservatorySubsystem.Gws)
          ),
          renderOverrideControl(
            NonEmptyString.unsafeFrom(props.instrument.longName),
            props.overrides
              .zoom(SubsystemOverrides.isInstrumentEnabled)
              .withOnMod(configApi.setInstrumentEnabled(props.obsId, _).runAsync),
            props.controlModes.get(props.instrument)
          )
        )
    )

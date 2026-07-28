// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.components.sequence

import cats.syntax.all.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ComaOption
import lucuma.core.enums.MountGuideOption
import lucuma.core.model.M1GuideConfig
import lucuma.core.model.M2GuideConfig
import lucuma.core.model.TelescopeGuideConfig
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Tag
import observe.ui.ObserveStyles

/**
 * Read-only display of the current telescope guide configuration (Mount, M1, Tip/Tilt and Coma).
 */
final case class GuideConfigStatus(config: TelescopeGuideConfig)
    extends ReactFnProps(GuideConfigStatus)

object GuideConfigStatus
    extends ReactFnComponent[GuideConfigStatus](props =>
      val c: TelescopeGuideConfig = props.config

      // A labelled guide badge. Active guides are highlighted with the running-tag style.
      def badge(label: String, value: String, active: Boolean): Tag =
        Tag(
          value = s"$label: $value",
          clazz = if (active) ObserveStyles.RunningTag else ObserveStyles.IdleTag
        )

      val mountActive: Boolean = c.mountGuide === MountGuideOption.MountGuideOn
      val mountValue: String   = if (mountActive) "On" else "Off"

      val (m1Value, m1Active) = c.m1Guide match
        case M1GuideConfig.M1GuideOn(source) => (source.tag.toUpperCase, true)
        case M1GuideConfig.M1GuideOff        => ("Off", false)

      val (tipTiltValue, tipTiltActive, comaValue, comaActive) = c.m2Guide match
        case M2GuideConfig.M2GuideOn(coma, sources) =>
          val srcs = sources.toList.map(_.tag.toUpperCase).mkString("+")
          (
            if (srcs.nonEmpty) srcs else "Off",
            sources.nonEmpty,
            if (coma === ComaOption.ComaOn) "On" else "Off",
            coma === ComaOption.ComaOn
          )
        case M2GuideConfig.M2GuideOff               =>
          ("Off", false, "Off", false)

      <.div(ObserveStyles.GuideConfigSection)(
        <.span(ObserveStyles.ConditionsLabel)("Guide Config"),
        badge("Mount", mountValue, mountActive),
        badge("M1", m1Value, m1Active),
        badge("Tip/Tilt", tipTiltValue, tipTiltActive),
        badge("Coma", comaValue, comaActive)
      )
    )

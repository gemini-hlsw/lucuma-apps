// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.ui.components.sequence

import cats.syntax.all.*
import japgolly.scalajs.react.*
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

import scalajs.js

/**
 * Read-only display of the current telescope guide configuration (Mount, M1, Tip/Tilt and Coma) as
 * a set of badges.
 */
final case class GuideConfigStatus(config: TelescopeGuideConfig)
    extends ReactFnProps(GuideConfigStatus)

object GuideConfigStatus
    extends ReactFnComponent[GuideConfigStatus](props =>
      val c: TelescopeGuideConfig = props.config

      // A labelled guide badge. Default blue when on, gray when "Off".
      def badge(label: String, value: String): Tag =
        val on = value =!= "Off"
        Tag(
          value = s"$label: $value",
          clazz = if on then js.undefined else ObserveStyles.TagDisabled
        )

      val mountValue: String = if (c.mountGuide === MountGuideOption.MountGuideOn) "On" else "Off"

      val m1Value: String = c.m1Guide match
        case M1GuideConfig.M1GuideOn(source) => source.tag.toUpperCase
        case M1GuideConfig.M1GuideOff        => "Off"

      val (tipTiltValue: String, comaValue: String) = c.m2Guide match
        case M2GuideConfig.M2GuideOn(coma, sources) =>
          val srcs = sources.toList.map(_.tag.toUpperCase).mkString("+")
          (
            if (srcs.nonEmpty) srcs else "Off",
            if (coma === ComaOption.ComaOn) "On" else "Off"
          )
        case M2GuideConfig.M2GuideOff               =>
          ("Off", "Off")

      React.Fragment(
        badge("Mount", mountValue),
        badge("M1", m1Value),
        badge("Tip/Tilt", tipTiltValue),
        badge("Coma", comaValue)
      )
    )

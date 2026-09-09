// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.probes
import lucuma.core.util.Display
import lucuma.react.common.ReactFnProps
import lucuma.ui.display.given
import lucuma.ui.primereact.EnumDropdownOptionalView
import lucuma.ui.primereact.given

// Lets the user override the guide probe AGS uses. Only probes the mode supports are offered.
case class GuideProbeControl(
  obsModeType:        ObservingModeType,
  defaultProbe:       Option[GuideProbe],
  explicitGuideProbe: View[Option[GuideProbe]],
  readonly:           Boolean
) extends ReactFnProps(GuideProbeControl.component)

object GuideProbeControl:
  private type Props = GuideProbeControl

  private val component =
    ScalaFnComponent[Props]: props =>
      val allowed: Set[GuideProbe] = probes.allowedProbes(props.obsModeType).toSet
      val placeholder: String      =
        props.defaultProbe.fold("Default")(p => s"${Display[GuideProbe].shortName(p)} (default)")

      <.div(
        ExploreStyles.AladinGuideProbe,
        EnumDropdownOptionalView(
          id = NonEmptyString.unsafeFrom("guide-probe"),
          value = props.explicitGuideProbe,
          exclude = GuideProbe.values.toSet -- allowed,
          showClear = props.explicitGuideProbe.get.isDefined,
          disabled = props.readonly,
          placeholder = placeholder
        )
      )

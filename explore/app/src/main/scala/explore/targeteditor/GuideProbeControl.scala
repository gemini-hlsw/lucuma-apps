// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import crystal.react.View
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.probes
import lucuma.core.util.Display
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.DropdownOptional
import lucuma.react.primereact.SelectItem
import lucuma.ui.display.given

// Lets the user override the guide probe AGS uses. Only probes the mode supports are offered,
// best-first as ordered by `allowedProbes`.
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
      val display                               = Display[GuideProbe]
      val options: List[SelectItem[GuideProbe]] =
        probes
          .allowedProbes(props.obsModeType)
          .toList
          .map(p => SelectItem(label = display.shortName(p), value = p))
      val placeholder: String                   =
        props.defaultProbe.fold("Default")(p => s"${display.shortName(p)} (default)")

      <.div(
        ExploreStyles.AladinGuideProbe,
        DropdownOptional(
          id = "guide-probe",
          value = props.explicitGuideProbe.get,
          options = options,
          showClear = props.explicitGuideProbe.get.isDefined,
          disabled = props.readonly,
          panelClass = ExploreStyles.AladinGuideProbePanel,
          placeholder = placeholder,
          onChange = props.explicitGuideProbe.set
        )
      )

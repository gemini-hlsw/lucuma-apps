// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import crystal.react.View
import explore.components.ui.ExploreStyles
import explore.model.GuiderChoice
import explore.model.GuidingConfiguration
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.ObservingModeType
import lucuma.core.util.Display
import lucuma.odb.data.AltairConfiguration
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.DropdownOptional
import lucuma.react.primereact.SelectItem
import lucuma.ui.display.given

// Lets the user override the guide probe AGS uses, or observe behind Altair. Only probes the mode
// supports are offered, best-first as ordered by `allowedProbes`, followed by the Altair modes when
// the instrument supports Altair.
case class GuideProbeControl(
  obsModeType:  ObservingModeType,
  defaultProbe: Option[GuideProbe],
  guiding:      View[GuidingConfiguration],
  readonly:     Boolean
) extends ReactFnProps(GuideProbeControl.component)

object GuideProbeControl:
  private type Props = GuideProbeControl

  private val component =
    ScalaFnComponent[Props]: props =>
      val altair: Option[AltairConfiguration]     = props.guiding.get.altair
      val current: Option[GuiderChoice]           = GuiderChoice.current(props.guiding.get)
      val options: List[SelectItem[GuiderChoice]] =
        GuiderChoice
          .options(props.obsModeType)
          .map(choice =>
            SelectItem(label = Display[GuiderChoice].shortName(choice), value = choice)
          )
      val placeholder: String                     =
        props.defaultProbe.fold("Default")(p => s"${Display[GuideProbe].shortName(p)} (default)")

      <.div(
        ExploreStyles.AladinGuideProbe,
        DropdownOptional(
          id = "guide-probe",
          value = current,
          options = options,
          showClear = current.isDefined,
          disabled = props.readonly,
          panelClass = ExploreStyles.AladinGuideProbePanel,
          placeholder = placeholder,
          onChange = choice => props.guiding.set(GuiderChoice.select(choice, altair))
        )
      )

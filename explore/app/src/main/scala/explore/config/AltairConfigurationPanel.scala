// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import crystal.react.*
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model.AltairControls
import explore.model.display.altairModeLabel
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.AltairMode
import lucuma.core.math.Angle
import lucuma.odb.data.AltairConfiguration
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.ui.display.given
import lucuma.ui.primereact.FormEnumDropdownOptionalView
import lucuma.ui.primereact.FormEnumDropdownView
import lucuma.ui.primereact.FormLabel
import lucuma.ui.primereact.LucumaPrimeStyles
import lucuma.ui.primereact.given

case class AltairConfigurationPanel(
  altair:              View[AltairConfiguration],
  guideStarSeparation: Option[Angle],
  permissions:         ConfigEditPermissions
) extends ReactFnProps(AltairConfigurationPanel.component)

object AltairConfigurationPanel:
  private type Props = AltairConfigurationPanel

  private val component =
    ScalaFnComponent[Props]: props =>
      val mode: AltairMode = props.altair.get.mode

      // The PA rules, without options staff may change on ongoing observations.
      val readonly: Boolean = !props.permissions.isFullEdit

      <.div(LucumaPrimeStyles.FormColumnCompact, ExploreStyles.AltairConfigurationForm)(
        FormLabel(htmlFor = "altair-mode".refined)("Adaptive Optics"),
        <.label(^.id := "altair-mode", ExploreStyles.AltairConfigurationMode)(
          altairModeLabel(mode)
        ),
        FormEnumDropdownOptionalView(
          id = "altair-field-lens".refined,
          label = React.Fragment(
            "Field Lens",
            HelpIcon("configuration/altair/field-lens.md".refined)
          ),
          value = props.altair.zoom(AltairConfiguration.explicitFieldLens),
          showClear = true,
          placeholder = AltairControls.fieldLensPlaceholder(props.guideStarSeparation),
          disabled = readonly
        ).when(AltairControls.fieldLensSelectable(mode)),
        FormEnumDropdownView(
          id = "altair-cass-rotator".refined,
          label = React.Fragment(
            "Cass Rotator",
            HelpIcon("configuration/altair/cass-rotator.md".refined)
          ),
          value = props.altair.zoom(AltairConfiguration.cassRotator),
          disabled = readonly
        ),
        FormEnumDropdownView(
          id = "altair-nd-filter".refined,
          label = React.Fragment(
            "ND Filter",
            HelpIcon("configuration/altair/nd-filter.md".refined)
          ),
          value = props.altair.zoom(AltairConfiguration.ndFilter),
          disabled = readonly || AltairControls.ndFilterLocked(mode)
        )
      )

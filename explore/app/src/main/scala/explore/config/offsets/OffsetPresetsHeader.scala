// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.offsets

import explore.components.CustomizedGroupAddon
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given

object OffsetPresetsHeader:

  /**
   * Shared header for the long-slit / IFU telescope-offset editors.
   */
  def apply[A](
    id:           NonEmptyString,
    helpId:       NonEmptyString,
    activePreset: Option[A],
    onSelect:     Option[A] => Callback,
    disabled:     Boolean,
    showRevert:   Boolean,
    onRevert:     Callback
  )(using Enumerated[A], Display[A]): VdomElement =
    <.span(ExploreStyles.SlitTelescopeConfigEditorHeader)(
      FormLabel(htmlFor = id)(
        "Spatial Offsets",
        HelpIcon(helpId)
      ),
      EnumOptionalDropdown(
        id = id,
        value = activePreset,
        showClear = false,
        placeholder = "Custom",
        disabled = disabled,
        onChange = onSelect
      ),
      CustomizedGroupAddon(
        "default offsets",
        onRevert,
        allowRevert = true
      ).when(showRevert)
    )

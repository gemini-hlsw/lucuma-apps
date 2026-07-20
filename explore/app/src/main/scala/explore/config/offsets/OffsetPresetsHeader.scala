// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.offsets

import cats.Eq
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.CustomizedGroupAddon
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.primereact.DropdownOptional
import lucuma.react.primereact.SelectItem
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given

object OffsetPresetsHeader:

  /**
   * Shared header for the long-slit / IFU telescope-offset editors.
   *
   * The preset key type `A` is opaque: callers pass the selectable `presets` and a `label` to
   * render each.
   */
  def apply[A](
    id:           NonEmptyString,
    helpId:       NonEmptyString,
    presets:      List[A],
    label:        A => String,
    activePreset: Option[A],
    onSelect:     Option[A] => Callback,
    disabled:     Boolean,
    showRevert:   Boolean,
    onRevert:     Callback
  )(using Eq[A]): VdomElement =
    <.span(ExploreStyles.SlitTelescopeConfigEditorHeader)(
      FormLabel(htmlFor = id)(
        "Spatial Offsets",
        HelpIcon(helpId)
      ),
      DropdownOptional(
        id = id.value,
        value = activePreset,
        options = presets.map(p => SelectItem(label = label(p), value = p)),
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

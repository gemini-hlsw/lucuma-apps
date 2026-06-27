// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.data.NonEmptySet
import cats.syntax.all.*
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.display.given
import japgolly.scalajs.react.*
import lucuma.core.enums.ScienceBand
import lucuma.core.syntax.all.*
import lucuma.react.common.*
import lucuma.react.common.style.Css
import lucuma.react.primereact.MenuItem
import lucuma.react.primereact.PopupMenu
import lucuma.react.primereact.PopupMenuRef
import lucuma.ui.reusability.given

case class ScienceBandPopupMenu(
  currentBand:           Option[ScienceBand],
  allocatedScienceBands: NonEmptySet[ScienceBand],
  onSelect:              ScienceBand => Callback,
  menuRef:               PopupMenuRef
) extends ReactFnProps(ScienceBandPopupMenu.component)

object ScienceBandPopupMenu:
  private type Props = ScienceBandPopupMenu

  private def menuItems(
    currentBand: Option[ScienceBand],
    bands:       NonEmptySet[ScienceBand],
    onSelect:    ScienceBand => Callback
  ): List[MenuItem] =
    bands.toNonEmptyList.toList.map: b =>
      val isCurrent = currentBand.exists(_ === b)
      // We need to always have an icon to keep the items consistent.
      val iconClass = if (isCurrent) Css.Empty else ExploreStyles.Hidden
      MenuItem.Item(
        icon = Icons.Checkmark.withClass(iconClass),
        label = b.longName,
        command = if (isCurrent) Callback.empty else onSelect(b)
      )

  private val component =
    ScalaFnComponent[Props]: p =>
      // Keep a stable menu model
      useMemo((p.currentBand, p.allocatedScienceBands))((currentBand, bands) =>
        menuItems(currentBand, bands, p.onSelect)
      ).map: items =>
        PopupMenu(model = items, clazz = ExploreStyles.ScienceBandPopupMenu).withRef(p.menuRef.ref)

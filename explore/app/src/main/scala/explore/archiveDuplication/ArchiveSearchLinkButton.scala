// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.archiveDuplication

import cats.data.NonEmptyList
import explore.Icons
import explore.model.ArchiveSearchLink
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.MenuItem
import lucuma.react.primereact.PopupMenu
import lucuma.react.primereact.hooks.all.*
import lucuma.ui.primereact.*
import org.scalajs.dom

/**
 * Opens the archive's own search page for an observation. A Search that fanned out into several
 * queries offers one entry per query instead of silently opening only one of them.
 */
case class ArchiveSearchLinkButton(links: NonEmptyList[ArchiveSearchLink])
    extends ReactFnProps(ArchiveSearchLinkButton.component)

object ArchiveSearchLinkButton:
  private type Props = ArchiveSearchLinkButton

  private val Tooltip = "Open this Search in the Gemini Observatory Archive"

  private def open(url: String): Callback =
    Callback(dom.window.open(url, "_blank", "noopener,noreferrer")).void

  private val component = ScalaFnComponent[Props]: props =>
    usePopupMenuRef.map: menuRef =>
      props.links match
        case NonEmptyList(link, Nil) =>
          Button(
            icon = Icons.ArrowUpRightFromSquare,
            text = true,
            tooltip = Tooltip,
            onClick = open(link.url)
          ).tiny.compact
        case links                   =>
          React.Fragment(
            Button(
              icon = Icons.ArrowUpRightFromSquare,
              text = true,
              tooltip = Tooltip,
              onClickE = e => e.stopPropagationCB >> menuRef.toggle(e)
            ).tiny.compact,
            PopupMenu(
              model = links.toList.map: link =>
                MenuItem.Item(label = link.label, url = link.url, target = "_blank")
            ).withRef(menuRef.ref)
          )

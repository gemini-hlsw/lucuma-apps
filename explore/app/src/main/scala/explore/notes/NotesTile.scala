// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import crystal.react.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.*
import explore.components.HelpIcon
import explore.components.MarkdownEditor
import explore.components.ui.ExploreStyles
import explore.model.ObsTabTileIds
import explore.model.enums.TileSizeState
import explore.utils.showCount
import explore.utils.wordCount
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.*
import lucuma.refined.*

final case class NotesTile(notes: View[Option[NonEmptyString]], override val hidden: Boolean)
    extends Tile[NotesTile](
      ObsTabTileIds.NotesId.id,
      s"Note for Observer (${showCount(notes.get.fold(0)(n => wordCount(n.value)), "word")})",
      bodyClass = ExploreStyles.NotesTile
    )(NotesTile)

object NotesTile
    extends TileComponent[NotesTile]((props, tileSize) =>
      TileContents(
        title = HelpIcon("observer-notes.md".refined).unless(tileSize.isMinimized),
        body = MarkdownEditor(props.notes)
      )
    )

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.undo

import lucuma.react.fa.FAIcon
import lucuma.react.fa.FontAwesome
import lucuma.react.fa.FontAwesomeIcon

import scala.scalajs.js
import scala.scalajs.js.annotation.*

object UndoIcons:
  @js.native
  @JSImport("@fortawesome/pro-solid-svg-icons", "faArrowRotateLeft")
  val faUndo: FAIcon = js.native

  @js.native
  @JSImport("@fortawesome/pro-solid-svg-icons", "faArrowRotateRight")
  val faRedo: FAIcon = js.native

  FontAwesome.library.add(
    faUndo,
    faRedo
  )

  val Undo = FontAwesomeIcon(faUndo)
  val Redo = FontAwesomeIcon(faRedo)

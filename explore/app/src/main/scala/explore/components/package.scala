// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.*
import cats.effect.IO
import crystal.react.*
import explore.Icons
import explore.model.IsActive
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.fa.FontAwesomeIcon
import lucuma.react.primereact.ConfirmDialog
import lucuma.react.primereact.DialogPosition
import lucuma.react.primereact.PrimeStyles
import lucuma.ui.syntax.effect.*
import org.typelevel.log4cats.Logger

def deleteConfirmation(
  msg:         String,
  header:      String,
  acceptLabel: String,
  action:      IO[Unit],
  active:      View[IsActive],
  icon:        FontAwesomeIcon = Icons.SkullCrossBones(^.color.red)
)(using
  Logger[IO]
) =
  ConfirmDialog.confirmDialog(
    message = <.div(msg),
    header = header,
    acceptLabel = acceptLabel,
    position = DialogPosition.Top,
    accept = action.switching(active.async, IsActive(_)).runAsync,
    acceptClass = PrimeStyles.ButtonSmall,
    rejectClass = PrimeStyles.ButtonSmall,
    icon = icon
  )

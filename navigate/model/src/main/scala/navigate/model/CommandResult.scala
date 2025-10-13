// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import cats.Eq
import cats.derived.*

sealed trait CommandResult derives Eq

object CommandResult {
  case object CommandSuccess extends CommandResult

  case object CommandPaused extends CommandResult

  case class CommandFailure(msg: String) extends CommandResult

  object CommandFailure {
    given Eq[CommandFailure] = Eq.by(_.msg)
  }

}

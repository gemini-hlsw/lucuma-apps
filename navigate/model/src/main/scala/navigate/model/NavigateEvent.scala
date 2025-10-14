// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.model

import cats.Eq
import cats.Order
import cats.syntax.all.*
import lucuma.core.model.User
import navigate.model.enums.ServerLogLevel

import java.time.Instant

sealed trait NavigateEvent

object NavigateEvent {
  case class ServerLogMessage(level: ServerLogLevel, timestamp: Instant, msg: String)
      extends NavigateEvent
  object ServerLogMessage {
    private given Order[Instant]  =
      Order.by(_.getNano)
    given Order[ServerLogMessage] =
      Order.by(x => (x.level, x.timestamp, x.msg))
  }

  case object NullEvent extends NavigateEvent

  case class ConnectionOpenEvent(
    userDetails:   Option[User],
    clientId:      ClientId,
    serverVersion: String
  ) extends NavigateEvent

  object ConnectionOpenEvent {
    given Eq[ConnectionOpenEvent] =
      Eq.by(x => (x.userDetails, x.clientId, x.serverVersion))
  }

  case class CommandStart(cmd: NavigateCommand) extends NavigateEvent
  object CommandStart {
    given Eq[CommandStart] = Eq.by(_.cmd)
  }

  case class CommandEnd(cmd: NavigateCommand, result: CommandResult) extends NavigateEvent
  object CommandEnd {
    given Eq[CommandEnd] = Eq.by(x => (x.cmd, x.result))
  }

  given Eq[NavigateEvent] =
    Eq.instance {
      case (a: ServerLogMessage, b: ServerLogMessage)       => a === b
      case (a: ConnectionOpenEvent, b: ConnectionOpenEvent) => a === b
      case (NullEvent, NullEvent)                           => true
      case (a: CommandStart, b: CommandStart)               => a === b
      case (a: CommandEnd, b: CommandEnd)                   => a === b
      case _                                                => false
    }

  sealed trait ForClient extends NavigateEvent {
    def clientId: ClientId
  }

}

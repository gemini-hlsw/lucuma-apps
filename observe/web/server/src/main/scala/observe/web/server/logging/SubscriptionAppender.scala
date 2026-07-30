// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.web.server.logging

import cats.effect.std.Dispatcher
import cats.syntax.all.*
import ch.qos.logback.classic.Level
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.UnsynchronizedAppenderBase
import fs2.concurrent.Topic
import observe.model.ClientId
import observe.model.LogMessage
import observe.model.enums.ObserveLogLevel
import observe.model.events.ClientEvent

import java.time.Instant

// Logback appender that forwards application log events to the UI
class SubscriptionAppender[F[_]](out: Topic[F, (Option[ClientId], ClientEvent)])(using
  dispatcher: Dispatcher[F]
) extends UnsynchronizedAppenderBase[ILoggingEvent]:
  private def observeLogLevel(level: Level): Option[ObserveLogLevel] =
    level match
      case Level.ERROR => ObserveLogLevel.Error.some
      case Level.WARN  => ObserveLogLevel.Warning.some
      case Level.INFO  => ObserveLogLevel.Info.some
      case _           => none

  override def append(event: ILoggingEvent): Unit =
    // We are outside the normal execution loop, thus we need to call unsafeRunAndForget directly.
    observeLogLevel(event.getLevel).foreach: level =>
      val msg =
        LogMessage(level, Instant.ofEpochMilli(event.getTimeStamp), event.getFormattedMessage)
      dispatcher.unsafeRunAndForget(out.publish1(none, ClientEvent.LogEvent(msg)))

// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.web.server.logging

import cats.effect.std.Dispatcher
import cats.syntax.all.*
import ch.qos.logback.classic.Level
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.spi.IThrowableProxy
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
      case Level.DEBUG => ObserveLogLevel.Debug.some
      case _           => none

  override def append(event: ILoggingEvent): Unit =
    // We are outside the normal execution loop, thus we need to call unsafeRunAndForget directly.
    observeLogLevel(event.getLevel).foreach: level =>
      val msg: LogMessage =
        LogMessage(
          level,
          Instant.ofEpochMilli(event.getTimeStamp),
          SubscriptionAppender.text(event)
        )
      dispatcher.unsafeRunAndForget(out.publish1(none, ClientEvent.LogEvent(msg)))

object SubscriptionAppender:

  /**
   * The text shown in the UI for a log event. Logback keeps the throwable apart from the formatted
   * message, and it is often where the actual reason is (e.g. the error the ODB answered with), so
   * its message is appended.
   */
  private[logging] def text(event: ILoggingEvent): String =
    Option(event.getThrowableProxy).fold(event.getFormattedMessage): tp =>
      s"${event.getFormattedMessage}: ${throwableMessage(tp)}"

  // Many exceptions carry no message; the class name still tells what kind of failure it was.
  private def throwableMessage(tp: IThrowableProxy): String =
    Option(tp.getMessage).getOrElse(tp.getClassName)

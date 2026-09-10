// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.web.server.otel

import cats.Monad
import cats.syntax.all.*
import org.typelevel.log4cats.Logger
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.trace.Tracer

/**
 * Mirrors trace-level log lines onto the current span as events.
 *
 * Ember's client has no tracing hooks, but it logs connection lifecycle at TRACE level ("Created
 * Connection", "Connection Taken - Reused: false - ...", "Shutting Down Connection").
 *
 * Passing this logger via `EmberClientBuilder.withLogger` makes those lines visible in Tempo inside
 * the client span, so connection setup can be told apart from request time.
 */
object SpanEventLogger:

  /**
   * A parsed ember log line: the event name and the attributes it carries. `spanAttributes` are the
   * ones also worth setting on the span itself, so TraceQL can group request latency by them.
   */
  case class ConnectionEvent(
    name:           String,
    attributes:     List[Attribute[?]],
    spanAttributes: List[Attribute[?]]
  )

  val Created: String  = "Created Connection"
  val Taken: String    = "Connection Taken"
  val ShutDown: String = "Shutting Down Connection"

  private val Known     = List(Created, Taken, ShutDown)
  private val Reused    = """Reused: (true|false)""".r.unanchored
  private val PoolTotal = """PoolState: \((\d+),""".r.unanchored

  /**
   * Only the three lifecycle lines ember emits at trace level are recognised, by prefix; any other
   * line is ignored.
   */
  def parse(message: String): Option[ConnectionEvent] =
    Known
      .find(message.startsWith)
      .map: name =>
        val reused    = Reused.findFirstMatchIn(message).flatMap(_.group(1).toBooleanOption)
        val poolTotal = PoolTotal.findFirstMatchIn(message).flatMap(_.group(1).toLongOption)
        val onSpan    =
          reused.toList.map(Attribute("ember.connection.reused", _)) ++
            poolTotal.toList.map(Attribute("ember.pool.total", _))
        ConnectionEvent(name, Attribute("ember.message", message) :: onSpan, onSpan)

  def apply[F[_]: {Monad, Tracer as T}](underlying: Logger[F]): Logger[F] =
    new Logger[F]:
      private def event(message: String): F[Unit] =
        parse(message).fold(Monad[F].unit): ev =>
          T.currentSpanOrNoop.flatMap: span =>
            span.addEvent(ev.name, ev.attributes) *> span.addAttributes(ev.spanAttributes*)

      def trace(message: => String): F[Unit]               =
        val msg = message
        event(msg) *> underlying.trace(msg)
      def trace(t: Throwable)(message: => String): F[Unit] =
        val msg = message
        event(msg) *> underlying.trace(t)(msg)

      def error(message: => String): F[Unit]                     = underlying.error(message)
      def error(t:       Throwable)(message: => String): F[Unit] = underlying.error(t)(message)
      def warn(message:  => String): F[Unit]                     = underlying.warn(message)
      def warn(t:        Throwable)(message: => String): F[Unit] = underlying.warn(t)(message)
      def info(message:  => String): F[Unit]                     = underlying.info(message)
      def info(t:        Throwable)(message: => String): F[Unit] = underlying.info(t)(message)
      def debug(message: => String): F[Unit]                     = underlying.debug(message)
      def debug(t:       Throwable)(message: => String): F[Unit] = underlying.debug(t)(message)

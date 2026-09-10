// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.web.server.otel

import munit.FunSuite

/**
 * Pins the ember 0.23 log line formats `SpanEventLogger` relies on. If an http4s upgrade rewords
 * them, these fail and the parser is updated deliberately instead of silently degrading.
 */
class SpanEventLoggerSuite extends FunSuite:

  private def parse(message: String): SpanEventLogger.ConnectionEvent =
    SpanEventLogger.parse(message).getOrElse(fail(s"not recognised: $message"))

  private def attr[A](ev: SpanEventLogger.ConnectionEvent, name: String): Option[A] =
    ev.attributes.collectFirst:
      case a if a.key.name == name => a.value.asInstanceOf[A]

  private def reused(ev: SpanEventLogger.ConnectionEvent): Option[Boolean] =
    attr[Boolean](ev, "ember.connection.reused")

  test("created connection"):
    val ev = parse("Created Connection - RequestKey: https://odb.example")
    assertEquals(ev.name, SpanEventLogger.Created)
    assertEquals(reused(ev), None)

  test("connection taken carries the reuse flag"):
    val fresh = parse(
      "Connection Taken - Key: https://odb.example - Reused: false - PoolState: (0,Map())"
    )
    val warm  = parse(
      "Connection Taken - Key: https://odb.example - Reused: true - PoolState: (1,Map())"
    )
    assertEquals(fresh.name, SpanEventLogger.Taken)
    assertEquals(reused(fresh), Some(false))
    assertEquals(reused(warm), Some(true))
    assertEquals(attr[Long](fresh, "ember.pool.total"), Some(0L))
    assertEquals(attr[Long](warm, "ember.pool.total"), Some(1L))
    assertEquals(warm.spanAttributes.map(_.key.name),
                 List("ember.connection.reused", "ember.pool.total")
    )

  test("pool state with several keys"):
    val ev = parse(
      "Connection Taken - Key: https://odb.example - Reused: true - PoolState: (7,Map(https://a -> 3, https://b -> 4))"
    )
    assertEquals(attr[Long](ev, "ember.pool.total"), Some(7L))

  test("shutting down connection"):
    val ev = parse("Shutting Down Connection - RequestKey: https://odb.example")
    assertEquals(ev.name, SpanEventLogger.ShutDown)

  test("unknown lines are ignored"):
    assertEquals(SpanEventLogger.parse("Something New - with - many - dashes"), None)

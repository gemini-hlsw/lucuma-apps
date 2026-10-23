// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.web.server.logging

import ch.qos.logback.classic.Level
import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.spi.LoggingEvent
import munit.FunSuite

class SubscriptionAppenderSuite extends FunSuite:

  private def event(message: String, t: Throwable | Null): LoggingEvent =
    val ctx: LoggerContext = new LoggerContext()
    new LoggingEvent(
      classOf[SubscriptionAppenderSuite].getName,
      ctx.getLogger("test"),
      Level.ERROR,
      message,
      t,
      null
    )

  test("message without a throwable is unchanged"):
    assertEquals(SubscriptionAppender.text(event("Plain message", null)), "Plain message")

  test("throwable message is appended"):
    assertEquals(
      SubscriptionAppender.text(
        event("Error sending ODB event", new RuntimeException("Step s-1 is not executable"))
      ),
      "Error sending ODB event: Step s-1 is not executable"
    )

  test("class name is used when no message is found"):
    assertEquals(
      SubscriptionAppender.text(event("Failed", new IllegalStateException())),
      "Failed: java.lang.IllegalStateException"
    )

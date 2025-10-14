// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.keywords

import cats.effect.IO
import lucuma.core.enums.Site
import observe.model.enums.KeywordName
import observe.server.keywords.DhsClient.Permanent
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger

import java.time.LocalDateTime

class DhsClientSimSpec extends munit.CatsEffectSuite:
  private given Logger[IO] = NoOpLogger.impl[IO]

  test("produce data labels for today"):
    DhsClientSim[IO](Site.GN, LocalDateTime.of(2016, 4, 15, 0, 0, 0))
      .flatMap(_.createImage(DhsClient.ImageParameters(Permanent, Nil)))
      .map(_.value)
      .map(a => assertEquals("N20160415S0001", a))

  test("accept keywords"):
    (for {
      client <- DhsClientSim.apply[IO](Site.GS)
      id     <- client.createImage(DhsClient.ImageParameters(Permanent, Nil))
      _      <-
        client.setKeywords(id, KeywordBag(Int32Keyword(KeywordName.TELESCOP, 10)), finalFlag = true)
    } yield ()).assert

  test("filter non-ASCII characters from string keywords"):
    val keyword = StringKeyword(KeywordName.OBJECT, "Café\u0001Test")
    assertEquals(keyword.stringValue, "CafTest")

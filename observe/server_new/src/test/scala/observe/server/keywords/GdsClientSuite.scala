// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server.keywords

import observe.model.enums.KeywordName

import scala.xml.XML

class GdsClientSuite extends munit.DisciplineSuite:
  test("GDSClient should reject bad responses"):
    val xml = XML.load(getClass.getResource("/gds-bad-resp.xml"))
    assert(GdsClient.parseError(xml).isLeft)

  test("filter non-ASCII characters from string keywords"):
    val keyword = StringKeyword(KeywordName.OBJECT, "Café\u0001Test")
    assertEquals(keyword.stringValue, "CafTest")

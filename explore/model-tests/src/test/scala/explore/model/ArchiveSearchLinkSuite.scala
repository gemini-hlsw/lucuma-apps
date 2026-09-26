// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import munit.FunSuite

class ArchiveSearchLinkSuite extends FunSuite:

  private def queryUrl(instrument: String): String =
    s"https://archive.gemini.edu/jsonsummary/notengineering/NotFail/$instrument/OBJECT/imaging/" +
      "ra=10.6847/dec=41.269/sr=30.0"

  test("GMOS-N query opens the search page, labelled by instrument"):
    assertEquals(
      ArchiveSearchLink.fromQueryUrl(queryUrl("GMOS-N"), 0),
      ArchiveSearchLink(
        "GMOS-N",
        "https://archive.gemini.edu/searchform/notengineering/NotFail/GMOS-N/OBJECT/imaging/" +
          "ra=10.6847/dec=41.269/sr=30.0"
      )
    )

  test("GMOS-S is told apart from GMOS-N"):
    assertEquals(ArchiveSearchLink.fromQueryUrl(queryUrl("GMOS-S"), 1).label, "GMOS-S")

  test("hyphenated instrument names are recognised"):
    assertEquals(ArchiveSearchLink.fromQueryUrl(queryUrl("IGRINS-2"), 0).label, "IGRINS-2")

  test("a query with no instrument segment falls back to its position"):
    val url = "https://archive.gemini.edu/jsonsummary/notengineering/NotFail/OBJECT/sr=30.0"
    assertEquals(ArchiveSearchLink.fromQueryUrl(url, 2).label, "Search 3")

  test("a url that is not a jsonsummary query is left as it is"):
    val url = "https://archive.gemini.edu/searchform/GMOS-N/sr=30.0"
    assertEquals(ArchiveSearchLink.humanUrl(url), url)

  test("only the jsonsummary path segment is rewritten"):
    val url = "https://archive.gemini.edu/jsonsummary/NotFail/object=jsonsummary/sr=30.0"
    assertEquals(
      ArchiveSearchLink.humanUrl(url),
      "https://archive.gemini.edu/searchform/NotFail/object=jsonsummary/sr=30.0"
    )

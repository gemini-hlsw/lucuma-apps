// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import lucuma.core.enums.Instrument
import munit.FunSuite

class ArchiveSearchLinkSuite extends FunSuite:

  private def queryUrl(endpoint: String, instrument: String): String =
    s"https://archive.gemini.edu/$endpoint/notengineering/NotFail/$instrument/OBJECT/IMAGING/" +
      "ra=10.6847/dec=41.269/sr=30.0"

  test("a stored query opens the search form, labelled by instrument"):
    assertEquals(
      ArchiveSearchLink.fromQueryUrl(queryUrl("jsonsummary", "GMOS-N"), 0),
      ArchiveSearchLink(Instrument.GmosNorth.shortName, queryUrl("searchform", "GMOS-N"))
    )

  test("GMOS-S is told apart from GMOS-N"):
    assertEquals(
      ArchiveSearchLink.fromQueryUrl(queryUrl("jsonsummary", "GMOS-S"), 1).label,
      Instrument.GmosSouth.shortName
    )

  test("a query with no instrument segment falls back to its position"):
    val url = "https://archive.gemini.edu/jsonsummary/notengineering/NotFail/OBJECT/sr=30.0"
    assertEquals(ArchiveSearchLink.fromQueryUrl(url, 2).label, "Search 3")

  test("a url that is not a GOA query is kept, labelled by its position"):
    val url = "https://archive.gemini.edu/searchform/GMOS-N"
    assertEquals(ArchiveSearchLink.fromQueryUrl(url, 2), ArchiveSearchLink("Search 3", url))

  test("a malformed url is kept rather than dropped"):
    val url = "not a url at all %%"
    assertEquals(ArchiveSearchLink.fromQueryUrl(url, 0).url, url)

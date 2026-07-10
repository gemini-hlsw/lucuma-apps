// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

// ObservationSubquery (lightweight BasicConfiguration) and
// ObservationWithFullModeSubquery (full ObservingMode) MUST select the same
// fields, differing only in the `observingMode` subselection.
//
// clue cannot share the body (no fragment/string-val reuse in subqueries), so the two are
// hand-kept copies.
//
// This test verifies both remain in sync
class ObservationSubquerySyncSuite extends munit.FunSuite:

  // Removes the `observingMode { ... }` block (the sole intended difference) via
  // brace-balanced matching
  private def stripObservingMode(query: String): String =
    val marker = "observingMode"
    val idx    = query.indexOf(marker)
    assert(idx >= 0, s"'$marker' field not found in subquery")

    val afterMarker = query.indexOf('{', idx)
    assert(afterMarker >= 0, s"no selection set found after '$marker'")

    // Walk from the opening brace to its matching close.
    var depth = 0
    var i     = afterMarker
    var end   = -1
    while i < query.length && end < 0 do
      query.charAt(i) match
        case '{' => depth += 1
        case '}' => depth -= 1; if (depth == 0) end = i
        case _   => ()
      i += 1
    assert(end >= 0, s"unbalanced braces after '$marker'")

    val withoutMode = query.substring(0, idx) + query.substring(end + 1)
    withoutMode.replaceAll("\\s+", " ").trim

  test("both observation subqueries select the same fields except observingMode"):
    assertEquals(
      stripObservingMode(ObservationSubquery.subquery),
      stripObservingMode(ObservationWithFullModeSubquery.subquery)
    )

  test("detector flags a field present in only one query"):
    val a = "{ id  observingMode { x }  title }"
    val b = "{ id  observingMode { y }  title  subtitle }" // extra `subtitle`
    assertNotEquals(stripObservingMode(a), stripObservingMode(b))

  test("detector ignores differences inside the observingMode block"):
    val a = "{ id  observingMode { x }  title }"
    val b = "{ id  observingMode { y z w }  title }"
    assertEquals(stripObservingMode(a), stripObservingMode(b))

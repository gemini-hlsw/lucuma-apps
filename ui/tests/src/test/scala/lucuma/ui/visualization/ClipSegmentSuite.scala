// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.visualization

class ClipSegmentSuite extends munit.FunSuite:

  private def clip(x1: Double, y1: Double, x2: Double, y2: Double) =
    clipSegment(x1, y1, x2, y2, 0, 0, 100, 50)

  private def assertSegment(
    obtained: Option[(Double, Double, Double, Double)],
    expected: (Double, Double, Double, Double)
  )(using munit.Location): Unit =
    val (ox1, oy1, ox2, oy2) = obtained.getOrElse(fail("expected a segment"))
    val (ex1, ey1, ex2, ey2) = expected
    assertEqualsDouble(ox1, ex1, 1e-6)
    assertEqualsDouble(oy1, ey1, 1e-6)
    assertEqualsDouble(ox2, ex2, 1e-6)
    assertEqualsDouble(oy2, ey2, 1e-6)

  test("a segment inside the rectangle is unchanged"):
    assertSegment(clip(10, 10, 90, 40), (10, 10, 90, 40))

  test("a segment crossing an edge is cut at the edge"):
    assertSegment(clip(50, 25, 150, 25), (50, 25, 100, 25))
    assertSegment(clip(50, -25, 50, 25), (50, 0, 50, 25))

  test("a segment crossing the whole rectangle is cut at both ends"):
    assertSegment(clip(-100, 25, 200, 25), (0, 25, 100, 25))
    assertSegment(clip(-50, -25, 150, 75), (0, 0, 100, 50))

  test("a segment outside the rectangle is dropped"):
    assertEquals(clip(-10, -10, -5, 60), None)
    assertEquals(clip(110, 10, 200, 40), None)
    // Its line passes through the rectangle, but the segment stops short of it
    assertEquals(clip(-50, -25, -10, -5), None)

  test("a degenerate segment is kept only when inside"):
    assertSegment(clip(20, 20, 20, 20), (20, 20, 20, 20))
    assertEquals(clip(-20, 20, -20, 20), None)

  // Far off-screen track vertices can be very large, they must come back inside the rectangle
  test("huge endpoints are brought back to the rectangle"):
    assertSegment(clip(-1e9, 25, 1e9, 25), (0, 25, 100, 25))
    assertSegment(clip(50, 25, 50, 1e9), (50, 25, 50, 50))

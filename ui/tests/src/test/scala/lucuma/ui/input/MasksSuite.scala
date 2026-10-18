// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.input

import munit.FunSuite

final class MasksSuite extends FunSuite:
  private def matches(pattern: String, s: String): Boolean = s"^$pattern$$".r.matches(s)

  test("range emits compact alternations") {
    assertEquals(Masks.range(0, 23, 2), "([0-1][0-9]|2[0-3])")
    assertEquals(Masks.range(0, 59, 2), "([0-5][0-9])")
    assertEquals(Masks.range(0, 90, 2), "([0-8][0-9]|90)")
    assertEquals(Masks.range(1, 12, 2), "(0[1-9]|1[0-2])")
    assertEquals(Masks.range(0, 359, 3), "([0-2][0-9]{2}|3[0-5][0-9])")
  }

  test("range matches exactly its integers") {
    val pattern: String = Masks.range(7, 123, 3)
    (0 to 999).foreach: n =>
      val s: String = f"$n%03d"
      assertEquals(matches(pattern, s), 7 <= n && n <= 123, s)
  }

  test("segment makes leading zeros optional") {
    val hours: String = Masks.segment(23, 2)
    List("0", "3", "03", "12", "23").foreach(s => assert(matches(hours, s), s))
    List("24", "123", "", "a").foreach(s => assert(!matches(hours, s), s))
  }

  test("Ra accepts canonical and short forms") {
    List("03:39:59.344", "3:9:5.5", "23:59:59", "00:00:00.1").foreach: s =>
      assert(matches(Masks.Ra, s), s)
  }

  test("Ra rejects out of range and too many decimals") {
    List("24:00:00", "12:60:00", "12:30:60", "12:30:45.1234", "12:30").foreach: s =>
      assert(!matches(Masks.Ra, s), s)
  }

  test("Dec accepts signed and unsigned forms") {
    List("+20:48:14.90", "-5:3:2", "90:00:00", "00:00:00.00").foreach: s =>
      assert(matches(Masks.Dec, s), s)
  }

  test("Dec rejects out of range") {
    List("91:00:00", "+-1:00:00", "20:48:14.901").foreach(s => assert(!matches(Masks.Dec, s), s))
  }

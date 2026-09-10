// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.dnd

import lucuma.react.pragmaticdnd.facade.Edge

class MoveInListSuite extends munit.FunSuite:

  private val abcd: List[String] = List("a", "b", "c", "d")

  test("moves an element down the list"):
    assertEquals(moveInList[String](0, 2, Edge.Bottom)(abcd), List("b", "c", "a", "d"))
    assertEquals(moveInList[String](0, 2, Edge.Top)(abcd), List("b", "a", "c", "d"))

  test("moves an element up the list"):
    assertEquals(moveInList[String](3, 1, Edge.Top)(abcd), List("a", "d", "b", "c"))
    assertEquals(moveInList[String](3, 1, Edge.Bottom)(abcd), List("a", "b", "d", "c"))

  test("moves to either end"):
    assertEquals(moveInList[String](2, 0, Edge.Top)(abcd), List("c", "a", "b", "d"))
    assertEquals(moveInList[String](1, 3, Edge.Bottom)(abcd), List("a", "c", "d", "b"))

  // Without the from == nextTo guard this drops the element instead of leaving it alone:
  // the predicate finds no match in the list the source was just removed from.
  test("dropping an element onto itself is a no-op"):
    List(Edge.Top, Edge.Bottom).foreach: edge =>
      assertEquals(moveInList[String](2, 2, edge)(abcd), abcd)

  test("an out-of-range source leaves the list alone"):
    assertEquals(moveInList[String](9, 1, Edge.Top)(abcd), abcd)

  // The reason this function exists: with duplicate elements a predicate on the value
  // cannot pick out a single row, so only the dragged position may move.
  test("only the dragged position moves when elements repeat"):
    val dup = List("a", "x", "b", "x")
    assertEquals(moveInList[String](3, 0, Edge.Top)(dup), List("x", "a", "x", "b"))
    assertEquals(moveInList[String](1, 3, Edge.Bottom)(dup), List("a", "b", "x", "x"))

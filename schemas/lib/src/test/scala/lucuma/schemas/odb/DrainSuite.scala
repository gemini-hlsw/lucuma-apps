// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import cats.Id
import munit.FunSuite

class DrainSuite extends FunSuite:
  case class Page(matches: List[Int], hasMore: Boolean)

  // Mimics the ODB: results from `offset` inclusive, at most `limit` of them.
  def fetch(all: List[Int], limit: Int)(offset: Option[Int]): Id[Page] =
    val from = offset.fold(all)(o => all.dropWhile(_ < o))
    Page(from.take(limit), from.sizeIs > limit)

  def drainAll(all: List[Int], limit: Int): List[Int] =
    drain[Id, Int, Int, Page](fetch(all, limit), _.matches, _.hasMore, identity)

  test("empty result"):
    assertEquals(drainAll(Nil, 3), Nil)

  test("single page"):
    assertEquals(drainAll(List(1, 2), 3), List(1, 2))

  test("several pages, without duplicating the offset element"):
    val all = (1 to 10).toList
    assertEquals(drainAll(all, 3), all)

  test("keeps the first element when the offset element stopped matching"):
    val all     = (1 to 10).toList
    val without = all.filterNot(_ == 3)
    // 3 is the last element of the first page, and disappears before the second request.
    val result  = drain[Id, Int, Int, Page](
      offset => fetch(if (offset.isEmpty) all else without, 3)(offset),
      _.matches,
      _.hasMore,
      identity
    )
    assertEquals(result, all)

  test("stops on an empty page that claims more"):
    assertEquals(
      drain[Id, Int, Int, Page](_ => Page(Nil, true), _.matches, _.hasMore, identity),
      Nil
    )

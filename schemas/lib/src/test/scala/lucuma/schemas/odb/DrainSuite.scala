// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import munit.FunSuite

class DrainSuite extends FunSuite:
  case class Page(matches: List[Int], hasMore: Boolean)

  // Mimics the ODB with `OFFSET_AFTER`: results after `after`, at most `limit` of them.
  def fetch(all: List[Int], limit: Int)(after: Option[Int]): Either[Throwable, Page] =
    val from = after.fold(all)(a => all.dropWhile(_ <= a))
    Right(Page(from.take(limit), from.sizeIs > limit))

  def drainAll(all: List[Int], limit: Int): Either[Throwable, List[Int]] =
    drain(fetch(all, limit), _.matches, _.hasMore, identity)

  test("empty result"):
    assertEquals(drainAll(Nil, 3), Right(Nil))

  test("single page"):
    assertEquals(drainAll(List(1, 2), 3), Right(List(1, 2)))

  test("several pages"):
    val all = (1 to 10).toList
    assertEquals(drainAll(all, 3), Right(all))

  test("one element per page"):
    val all = (1 to 5).toList
    assertEquals(drainAll(all, 1), Right(all))

  test("keeps going when the last element of a page stopped matching"):
    val all     = (1 to 10).toList
    val without = all.filterNot(_ == 3)
    // 3 is the last element of the first page, and disappears before the second request.
    val result  = drain(
      (after: Option[Int]) => fetch(if (after.isEmpty) all else without, 3)(after),
      _.matches,
      _.hasMore,
      identity
    )
    assertEquals(result, Right(all))

  test("fails on an empty page that claims more"):
    assert(
      drain(
        (_: Option[Int]) => Right(Page(Nil, true)).withLeft[Throwable],
        _.matches,
        _.hasMore,
        identity
      ).isLeft
    )

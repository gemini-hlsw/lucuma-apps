// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.utils

import cats.data.NonEmptyList
import cats.data.State
import cats.syntax.all.*
import crystal.ViewF
import munit.FunSuite

class ViewsOfElementsSuite extends FunSuite:
  private type S[L, A] = State[(L, Option[(Int, Int)]), A]

  /** A view whose state is the value plus the last (previous, current) pair seen by a callback. */
  private def view[L](l: L): ViewF[S[L, *], L] =
    ViewF(
      l,
      (f, cb) =>
        State.get[(L, Option[(Int, Int)])].flatMap { case (prev, _) =>
          val cur = f(prev)
          State.modify[(L, Option[(Int, Int)])](_.copy(_1 = cur)) >> cb(prev, cur)
        }
    )

  private def record[L](p: Int, c: Int): S[L, Unit] =
    State.modify[(L, Option[(Int, Int)])](_.copy(_2 = (p, c).some))

  test("toListOfViews gives each element and modifies it in place"):
    val list  = List(1, 2, 3)
    val views = view(list).toListOfViews
    assertEquals(views.map(_.get), list)
    assertEquals(views(1).mod(_ + 10).runS((list, none)).value._1, List(1, 12, 3))
    assertEquals(
      views(2).modCB(_ * 2, record[List[Int]]).runS((list, none)).value,
      (List(1, 2, 6), (3, 6).some)
    )

  test("toNelOfViews gives each element and modifies it in place"):
    val nel   = NonEmptyList.of(1, 2, 3)
    val views = view(nel).toNelOfViews
    assertEquals(views.map(_.get), nel)
    assertEquals(views.head.mod(_ - 1).runS((nel, none)).value._1, NonEmptyList.of(0, 2, 3))
    assertEquals(
      views.toList(1).modCB(_ + 5, record[NonEmptyList[Int]]).runS((nel, none)).value,
      (NonEmptyList.of(1, 7, 3), (2, 7).some)
    )

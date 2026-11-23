// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.undo

import cats.effect.IO
import cats.effect.Ref
import monocle.Focus
import monocle.Lens

import scala.concurrent.duration.*

class UndoSetterSpec extends munit.CatsEffectSuite:
  private case class Pair(a: Int, b: Int)

  private val A: Lens[Pair, Int] = Focus[Pair](_.a)
  private val B: Lens[Pair, Int] = Focus[Pair](_.b)

  test("undo of a zoomed edit restores only the zoomed field"):
    TestUndoable.simulated(Pair(0, 0)): u =>
      for
        _ <- u.exec(_.withContext(_.zoom(A).set(1)))
        _ <- u.external(B.replace(7))
        _ <- u.exec(_.undo)
        v <- u.value
      yield assertEquals(v, Pair(0, 7))

  test("an undoable view records the edit and runs its effect on set, undo and redo"):
    TestUndoable.simulated(Pair(0, 0)): u =>
      for
        effects <- Ref[IO].of(List.empty[Int])
        _       <- u.exec:
                     _.withContext:
                       _.undoableView(A, (n: Int) => effects.update(_ :+ n)).set(1)
        set     <- u.value
        _       <- u.exec(_.undo)
        undone  <- u.value
        _       <- u.exec(_.redo)
        redone  <- u.value
        e       <- effects.get
      yield
        assertEquals(set, Pair(1, 0))
        assertEquals(undone, Pair(0, 0))
        assertEquals(redone, Pair(1, 0))
        assertEquals(e, List(1, 0, 1))

  test("a list of undo setters edits and restores a single element"):
    TestUndoable.simulated(List(1, 2, 3)): u =>
      for
        _      <- u.exec(_.withContext(_.toListOfUndoSetters[Int].apply(1).set(9)))
        set    <- u.value
        _      <- u.exec(_.undo)
        undone <- u.value
      yield
        assertEquals(set, List(1, 9, 3))
        assertEquals(undone, List(1, 2, 3))

  test("an Action runs onSet on set and again on undo"):
    TestUndoable.simulated(Pair(0, 0)): u =>
      for
        calls  <- Ref[IO].of(List.empty[Int])
        action  = Action(A)(onSet = (_: Pair, a: Int) => calls.update(_ :+ a))
        _      <- u.exec(_.withContext(action.set(_)(5)))
        set    <- u.value
        _      <- u.exec(_.undo)
        undone <- u.value
        c      <- calls.get
      yield
        assertEquals(set, Pair(5, 0))
        assertEquals(undone, Pair(0, 0))
        assertEquals(c, List(5, 0))

  test("an AsyncAction sets the fetched value and uses its key in the callbacks"):
    TestUndoable.simulated(Pair(0, 0)): u =>
      for
        calls  <- Ref[IO].of(List.empty[(String, Int)])
        action  = AsyncAction[Pair, String, Int](
                    asyncGet = IO.pure(("key", 4)),
                    getter = _ => A.get,
                    setter = _ => A.replace,
                    onSet = k => (_, a) => calls.update(_ :+ (k, a)),
                    onRestore = k => (_, a) => calls.update(_ :+ (s"$k-restored", a))
                  )
        result <- IO(u.context.runNow()).flatMap(action(_))
        _      <- IO.sleep(1.minute)
        set    <- u.value
        _      <- u.exec(_.undo)
        undone <- u.value
        c      <- calls.get
      yield
        assertEquals(result, ("key", 4))
        assertEquals(set, Pair(4, 0))
        assertEquals(undone, Pair(0, 0))
        assertEquals(c, List(("key", 4), ("key-restored", 0)))

  private case object Rejected extends RuntimeException("rejected")

  private val Serial: IO[UndoEffectRunner[IO]] =
    UndoEffectRunner.serial[IO](
      10.seconds,
      {
        case Rejected => true
        case _        => false
      },
      _ => IO.unit,
      IO.unit
    )

  test("a rejected edit is reverted while a later edit to another field goes through"):
    TestUndoable.simulated(Pair(0, 0), Serial): u =>
      for
        sent <- Ref[IO].of(List.empty[Int])
        _    <- u.fire:
                  _.withContext:
                    _.undoableView(A, (_: Int) => IO.sleep(2.seconds) >> IO.raiseError(Rejected))
                      .set(1)
        _    <- u.fire(_.withContext(_.undoableView(B, (n: Int) => sent.update(_ :+ n)).set(2)))
        _    <- IO.sleep(1.minute)
        v    <- u.value
        s    <- u.currentStacks
        n    <- sent.get
      yield
        assertEquals(v, Pair(0, 2))
        assertEquals(s.undo.size, 1)
        assertEquals(n, List(2))
